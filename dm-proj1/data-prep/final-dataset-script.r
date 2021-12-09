library(tidyverse)
library(dplyr)
library(readr)
library(psych)


clients <- read.csv('./ficheiros_competicao/client.csv',sep = ';', header=TRUE)
trans <- read.csv('./ficheiros_competicao/trans_train.csv',sep = ';', header=TRUE)
account <- read.csv('./ficheiros_competicao/account.csv',sep = ';', header=TRUE)
district <- read.csv('./ficheiros_competicao/district.csv',sep = ';', header=TRUE)
disp <- read.csv('./ficheiros_competicao/disp.csv',sep = ';', header=TRUE)
loan <- read.csv('./ficheiros_competicao/loan_train.csv',sep = ';', header=TRUE)
card <- read.csv('./ficheiros_competicao/card_train.csv', sep =';', header=TRUE)
###
loan_test <- read.csv('./ficheiros_competicao/loan_test.csv',sep = ';', header=TRUE)
card_test <- read.csv('./ficheiros_competicao/card_test.csv', sep =';', header=TRUE)
trans_test <- read.csv('./ficheiros_competicao/trans_test.csv',sep = ';', header=TRUE)

# Our complete_df dataset (to test will be composed of both the training and test data for the cards and transactions' info)
card <- rbind(card_test, card)
trans <- rbind(trans_test, trans)

# After binding the test and training datasets, theses dataframe become useless
rm(trans_test)
rm(card_test)

# Cleaning the Client's attributes + generating new features (age, gender, birthday, age_range)
clients <- clients %>% 
  mutate(age_range = -(birth_number+19000000-20211126)%/%10000)
clients <- clients %>% 
  mutate(gender = if_else((birth_number%%10000) > 1231, "F", "M"))
clients <- clients %>% 
  mutate(birthday = ifelse(clients$gender == "F", birth_number-5000, birth_number))

clients <- clients %>% 
  mutate(age_range = ifelse((clients$age_range>=10 & clients$age_range<=20) , '10_20',clients$age_range))
clients <- clients %>% 
  mutate(age_range = ifelse((clients$age_range>=20 & clients$age_range<=30) , '20_30',clients$age_range))
clients <- clients %>% 
  mutate(age_range = ifelse((clients$age_range>=30 & clients$age_range<=40) , '30_40',clients$age_range))
clients <- clients %>% 
  mutate(age_range = ifelse((clients$age_range>=40 & clients$age_range<=50) , '40_50',clients$age_range))
clients <- clients %>% 
  mutate(age_range = ifelse((clients$age_range>=50 & clients$age_range<=60) , '50_60',clients$age_range))
clients <- clients %>% 
  mutate(age_range = ifelse((clients$age_range>=60 & clients$age_range<=70) , '60_70',clients$age_range))
clients <- clients %>% 
  mutate(age_range = ifelse((clients$age_range>=70 & clients$age_range<=80) , '70_80',clients$age_range))
clients <- clients %>% 
  mutate(age_range = ifelse((clients$age_range>=80 & clients$age_range<=90) , '80_90',clients$age_range))
clients <- clients %>% 
  mutate(age_range = ifelse((clients$age_range>=90 & clients$age_range<=100) , '90_100',clients$age_range))

clients <- clients %>% 
  mutate(age = -(birth_number+19000000-20211126)%/%10000)

# Auxiliary Functions that will help calculate new features (for the transactions' table in particular)
apply_method <- function(amount, type, METHOD){
  res <- c()
  for (i in 1:length(amount)){
    is_credit <- (type[i] == 'credit')
    value <- amount[i]
    if(!is_credit){
      value <- -value
    }
    res <- c(res, value)
  }
  METHOD(res)
}

apply_method_op <- function(amount, type, credit, METHOD){
  res <- c()
  for (i in 1:length(amount)){
    is_credit <- (type[i] == 'credit')
    value <- amount[i]
    if(!is_credit & credit){
      next
    }
    if(is_credit & !credit){
      next
    }
    res <- c(res, value)
  }
  if(length(res) > 0){
    METHOD(res)
  }else{
    0
  }
}

count_value <- function(coll, value){
  length(which(coll==value))
}

#Feature Engineering for transactions (Stats)
trans_stats <- trans %>% 
  group_by(account_id) %>% 
  summarize(
    num_credit = count_value(type, "credit"),
    num_withdrawal = count_value(type, "withdrawal") + count_value(type, "withdrawal cash"),
    
    max_credit=apply_method_op(amount,type, TRUE, max),
    avg_credit=apply_method_op(amount,type, TRUE, mean),
    min_credit=apply_method_op(amount,type, TRUE, min),
    sd_credit=apply_method_op(amount,type, TRUE, sd),
    skew_credit=apply_method_op(amount,type, TRUE, skew),
    kurtosi_credit=apply_method_op(amount,type, TRUE, kurtosi),
    median_credit=apply_method_op(amount,type, TRUE, median),
    mad_credit=apply_method_op(amount,type, TRUE, mad),
    var_credit=apply_method_op(amount,type, TRUE, var),
    iqr_credit=apply_method_op(amount,type, TRUE, IQR),
    
    max_withdrawal=apply_method_op(amount,type, FALSE, max),
    avg_withdrawal=apply_method_op(amount,type, FALSE, mean),
    min_withdrawal=apply_method_op(amount,type, FALSE, min),
    sd_withdrawal=apply_method_op(amount,type, FALSE, sd),
    skew_withdrawal=apply_method_op(amount,type, FALSE, skew),
    kurtosi_withdrawal=apply_method_op(amount,type, FALSE, kurtosi),
    median_withdrawal=apply_method_op(amount,type, FALSE, median),
    mad_withdrawal=apply_method_op(amount,type, FALSE, mad),
    var_withdrawal=apply_method_op(amount,type, FALSE, var),
    iqr_withdrawal=apply_method_op(amount,type, FALSE, IQR),
    
    avg_amount=apply_method(amount, type, mean),
    sd_amount=apply_method(amount, type, sd),
    skew_amount=apply_method(amount, type, skew),
    kurtosi_amount=apply_method(amount, type, kurtosi),
    median_amount=apply_method(amount, type, median),
    mad_amount=apply_method(amount, type, mad),
    var_amount=apply_method(amount,type, var),
    iqr_amount=apply_method(amount,type, IQR),
    
    avg_balance=mean(balance),
    max_balance=max(balance),
    min_balance=min(balance),
    sd_balance=sd(balance),
    skew_balance=skew(balance),
    kurtosi_balance=kurtosi(balance),
    median_balance=median(balance),
    mad_balance=mad(balance),
    var_balance=var(balance),
    iqr_balance=IQR(balance)
  )

#Summarizing by month

trans <- trans %>% mutate(date_by_month=floor(date/100))
trans_by_month_sum <- trans %>% group_by(date_by_month, account_id) %>% summarize(sum_balance = mean(balance))

trans_by_month <- trans_by_month_sum %>% group_by(account_id) %>% summarize(avg_monthly_balance=mean(sum_balance))
rm(trans_by_month_sum)

#Joining Tables

disp_owners <- filter(disp,type=="OWNER")
clients_accounts <- subset(left_join(disp_owners, clients, by='client_id'), select=-district_id)
rm(disp_owners)
account_district <- left_join(account, district, by=c('district_id'='code'))
account_district_client <- left_join(clients_accounts, account_district, 'account_id')
rm(clients_accounts)
rm(account_district)
trans_client_district <- left_join(trans_stats, account_district_client, 'account_id')
rm(trans_stats)
trans_client_district <- left_join(trans_client_district, trans_by_month, 'account_id')
rm(account_district_client)
rm(trans_by_month)

#### HERE TEST
#trans_client_district_loan <- left_join(loan, trans_client_district, 'account_id')
trans_client_district_loan <- left_join(loan_test, trans_client_district, 'account_id')

trans_client_district_loan$datediff <- trans_client_district_loan$date.x - trans_client_district_loan$birthday

names(trans_client_district_loan)[names(trans_client_district_loan) == 'no..of.commited.crimes..95' ] <- 'crimes95'
names(trans_client_district_loan)[names(trans_client_district_loan) == 'no..of.commited.crimes..96' ] <- 'crimes96'
trans_client_district_loan$crimes95 <- as.numeric(trans_client_district_loan$crimes95)


complete_df <- left_join(trans_client_district_loan, card, 'disp_id')


#Cleaning + Extra attributes

names(complete_df)[names(complete_df) == 'date.x' ] <- 'date_loan'
names(complete_df)[names(complete_df) == 'date.y' ] <- 'date_account'

names(complete_df)[names(complete_df) == 'type.x' ] <- 'account_type'
names(complete_df)[names(complete_df) == 'type.y' ] <- 'card_type'

names(complete_df)[names(complete_df) == 'datediff' ] <- 'age_at_loan'


complete_df$time_bf_loan <- complete_df$date_loan - complete_df$date_account
complete_df$frequency_num <- unclass(complete_df$frequency)
complete_df$region_num <- unclass(complete_df$region)
complete_df$gender_num <- unclass(complete_df$gender)
complete_df$card_num <- unclass(complete_df$card_type)
complete_df$card_num <- ifelse(is.na(complete_df$card_num), 0, complete_df$card_num)
complete_df$issued <- ifelse(is.na(complete_df$issued), 0, complete_df$issued)
complete_df$sd_withdrawal <- ifelse(is.na(complete_df$sd_withdrawal), 0, complete_df$sd_withdrawal)
complete_df$skew_withdrawal <- ifelse(is.na(complete_df$skew_withdrawal), 0, complete_df$skew_withdrawal)
complete_df$kurtosi_withdrawal <- ifelse(is.na(complete_df$kurtosi_withdrawal), 0, complete_df$kurtosi_withdrawal)
complete_df$mad_withdrawal <- ifelse(is.na(complete_df$mad_withdrawal), 0, complete_df$mad_withdrawal)

complete_df <- complete_df %>% 
  mutate(card_num = ifelse((complete_df$card_num == 0) , "none",complete_df$card_num))


#final <- complete_df
#final <- subset(final, select = -account_id )


# Keeping only some attributes for the final dataset

final <- complete_df[,c("account_id","num_credit","num_withdrawal","max_credit","min_credit","avg_credit","median_credit","iqr_credit")]

final$max_withdrawal <- complete_df$max_withdrawal[match(final$account_id, complete_df$account_id)]
final$min_withdrawal <- complete_df$min_withdrawal[match(final$account_id, complete_df$account_id)]
final$avg_withdrawal <- complete_df$avg_withdrawal[match(final$account_id, complete_df$account_id)]
final$median_withdrawal <- complete_df$median_withdrawal[match(final$account_id, complete_df$account_id)]
final$iqr_withdrawal <- complete_df$iqr_withdrawal[match(final$account_id, complete_df$account_id)]

final$max_amount <- complete_df$max_amount[match(final$account_id, complete_df$account_id)]
final$min_amount <- complete_df$min_amount[match(final$account_id, complete_df$account_id)]
final$avg_amount <- complete_df$avg_amount[match(final$account_id, complete_df$account_id)]
final$median_amount <- complete_df$median_amount[match(final$account_id, complete_df$account_id)]
final$iqr_amount <- complete_df$iqr_amount[match(final$account_id, complete_df$account_id)]

final$max_balance <- complete_df$max_balance[match(final$account_id, complete_df$account_id)]
final$min_balance <- complete_df$min_balance[match(final$account_id, complete_df$account_id)]
final$avg_balance <- complete_df$avg_balance[match(final$account_id, complete_df$account_id)]
final$median_balance <- complete_df$median_balance[match(final$account_id, complete_df$account_id)]
final$iqr_balance <- complete_df$iqr_balance[match(final$account_id, complete_df$account_id)]

final$frequency_num <- complete_df$frequency_num[match(final$account_id, complete_df$account_id)]
final$avg_monthly_balance <- complete_df$avg_monthly_balance[match(final$account_id, complete_df$account_id)]
final$time_bf_loan <- complete_df$time_bf_loan[match(final$account_id, complete_df$account_id)]
final$age_at_loan <- complete_df$age_at_loan[match(final$account_id, complete_df$account_id)]
final$card_num <- complete_df$card_num[match(final$account_id, complete_df$account_id)] 

final$status <- complete_df$status[match(final$account_id, complete_df$account_id)] 

######

final$age <- complete_df$age[match(final$account_id, complete_df$account_id)]
final$gender <- complete_df$gender[match(final$account_id, complete_df$account_id)]
final$avg_salary <- complete_df$average.salary[match(final$account_id, complete_df$account_id)]
final$district_id <- complete_df$district_id[match(final$account_id, complete_df$account_id)]
final$nb_inhabitants <- complete_df$no..of.inhabitants[match(final$account_id, complete_df$account_id)]
final$nb_cities <- complete_df$no..of.cities[match(final$account_id, complete_df$account_id)]
final$ratio_urban_inhabitants <- complete_df$ratio.of.urban.inhabitants[match(final$account_id, complete_df$account_id)]
final$unemploymant_95 <- complete_df$unemploymant.rate..95[match(final$account_id, complete_df$account_id)]
final$unemploymant_96 <- complete_df$unemploymant.rate..96[match(final$account_id, complete_df$account_id)]
final$crime_95 <- complete_df$crimes95[match(final$account_id, complete_df$account_id)]
final$crime_96 <- complete_df$crimes96[match(final$account_id, complete_df$account_id)]
final$nb_entrepeneurs <- complete_df$no..of.enterpreneurs.per.1000.inhabitants[match(final$account_id, complete_df$account_id)]

# Unemploymant_95 is stored as Char instead of Num
df <- final[,c("unemploymant_95")]
df[sapply(df, is.character)] <-lapply(df[sapply(df, is.character)], as.numeric)
final$unemploymant_95 <- df$unemploymant_95
rm(df)

# crime_95 has 5 NA's, will replace this value by the district's equivalent in 96
final$crime_95 <- ifelse(is.na(final$crime_95), final$crime_96, final$crime_95)
# same for unemployment_95
final$unemploymant_95 <- ifelse(is.na(final$unemploymant_95), final$unemploymant_96, final$unemploymant_95)

# Fixing typo
names(final)[names(final) == 'unemploymant_95' ] <- 'unemployment_95'
names(final)[names(final) == 'unemploymant_96' ] <- 'unemployment_96'

# Loans Info
final$loan_id <- complete_df$loan_id[match(final$account_id, complete_df$account_id)]
final$duration_loan <- complete_df$duration[match(final$account_id, complete_df$account_id)]
final$payments_loan <- complete_df$payments[match(final$account_id, complete_df$account_id)]

summary(final)
# Remove IDs
final <- subset(final, select = -account_id )
final <- subset(final, select = -status )

# Este é o csv final onde vamos correr as nossas previsões, fazendo aqui as alterações para melhorar o modelo (?)
write.csv(final,"./complete_test.csv", row.names = FALSE)

