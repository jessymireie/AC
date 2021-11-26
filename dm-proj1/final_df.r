library(tidyverse)
library(psych)

clients <- read.csv('ficheiros_competicao/client.csv',sep = ';', header=TRUE)
trans <- read.csv('ficheiros_competicao/trans_train.csv',sep = ';', header=TRUE)
account <- read.csv('ficheiros_competicao/account.csv',sep = ';', header=TRUE)
district <- read.csv('ficheiros_competicao/district.csv',sep = ';', header=TRUE)
disp <- read.csv('ficheiros_competicao/disp.csv',sep = ';', header=TRUE)
loan <- read.csv('ficheiros_competicao/loan_train.csv',sep = ';', header=TRUE)
card <- read.csv('ficheiros_competicao/card_train.csv', sep =';', header=TRUE)

clients <- clients %>% 
  mutate(age_range = -(birth_number+19000000-20211126)%/%10000)
clients <- clients %>% 
  mutate(gender = if_else((birth_number%%10000) > 1231, "F", "M"))
clients <- clients %>% 
  mutate(birthday = ifelse(clients$gender == "F", birth_number-5000, birth_number))

clients <- clients %>% 
  mutate(age_range = ifelse((clients$age_range>=10 & clients$age_range<=20) , '10_20',clients$age_range))
#clients$age_range <- ifelse((clients$birthday>=100000 & clients$birthday<=200000) , '10_20',clients$age_range)
clients <- clients %>% 
  mutate(age_range = ifelse((clients$age_range>=20 & clients$age_range<=30) , '20_30',clients$age_range))
#clients$age_range <- ifelse((clients$birthday>=200000 & clients$birthday<=300000) , '20_30',clients$age_range)
clients <- clients %>% 
  mutate(age_range = ifelse((clients$age_range>=30 & clients$age_range<=40) , '30_40',clients$age_range))
#clients$age_range <- ifelse((clients$birthday>=300000 & clients$birthday<=400000) , '30_40',clients$age_range)
clients <- clients %>% 
  mutate(age_range = ifelse((clients$age_range>=40 & clients$age_range<=50) , '40_50',clients$age_range))
#clients$age_range <- ifelse((clients$birthday>=400000 & clients$birthday<=500000) , '40_50',clients$age_range)
clients <- clients %>% 
  mutate(age_range = ifelse((clients$age_range>=50 & clients$age_range<=60) , '50_60',clients$age_range))
#clients$age_range <- ifelse((clients$birthday>=500000 & clients$birthday<=600000) , '50_60',clients$age_range)
clients <- clients %>% 
  mutate(age_range = ifelse((clients$age_range>=60 & clients$age_range<=70) , '60_70',clients$age_range))
#clients$age_range <- ifelse((clients$birthday>=600000 & clients$birthday<=700000) , '60_70',clients$age_range)
clients <- clients %>% 
  mutate(age_range = ifelse((clients$age_range>=70 & clients$age_range<=80) , '70_80',clients$age_range))
#clients$age_range <- ifelse((clients$birthday>=700000 & clients$birthday<=800000) , '70_80',clients$age_range)
clients <- clients %>% 
  mutate(age_range = ifelse((clients$age_range>=80 & clients$age_range<=90) , '80_90',clients$age_range))
#clients$age_range <- ifelse((clients$birthday>=800000 & clients$birthday<=900000) , '80_90',clients$age_range)
clients <- clients %>% 
  mutate(age_range = ifelse((clients$age_range>=90 & clients$age_range<=100) , '90_100',clients$age_range))
#clients$age_range <- ifelse((clients$birthday>=900000 & clients$birthday<=991231) , '90_99',clients$age_range)
##clients$age_range <- as.factor(clients$age_range)
clients <- clients %>% 
  mutate(age = -(birth_number+19000000-20211126)%/%10000)


all_values <- function(amount, type,METHOD){
  res <- c()
  for (i in 1:length(amount)){
    is_credit <- type[i] == 'credit'
    value <- amount[i]
    if(!is_credit){
      value <- -value
    }
    res <- c(res, value)
  }
  METHOD(res)
}

filtered_values <- function(amount, type, credit, METHOD){
  res <- c()
  for (i in 1:length(amount)){
    is_credit <- type[i] == 'credit'
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

count_cmp <- function(coll, value){
  length(which(coll==value))
}

#Stats

trans_calculations <- trans %>% 
  group_by(account_id) %>% 
  summarize(
    n_credit = count_cmp(type, "credit"),
    n_withdrawal = count_cmp(type, "withdrawal") + count_cmp(type, "withdrawal cash"),
    
    max_credit=filtered_values(amount,type, TRUE, max),
    avg_credit=filtered_values(amount,type, TRUE, mean),
    min_credit=filtered_values(amount,type, TRUE, min),
    sd_credit=filtered_values(amount,type, TRUE, sd),
    skew_credit=filtered_values(amount,type, TRUE, skew),
    kurtosi_credit=filtered_values(amount,type, TRUE, kurtosi),
    median_credit=filtered_values(amount,type, TRUE, median),
    mad_credit=filtered_values(amount,type, TRUE, mad),
    var_credit=filtered_values(amount,type, TRUE, var),
    iqr_credit=filtered_values(amount,type, TRUE, IQR),
    
    
    max_withdrawal=filtered_values(amount,type, FALSE, max),
    avg_withdrawal=filtered_values(amount,type, FALSE, mean),
    min_withdrawal=filtered_values(amount,type, FALSE, min),
    sd_withdrawal=filtered_values(amount,type, FALSE, sd),
    skew_withdrawal=filtered_values(amount,type, FALSE, skew),
    kurtosi_withdrawal=filtered_values(amount,type, FALSE, kurtosi),
    median_withdrawal=filtered_values(amount,type, FALSE, median),
    mad_withdrawal=filtered_values(amount,type, FALSE, mad),
    var_withdrawal=filtered_values(amount,type, FALSE, var),
    iqr_withdrawal=filtered_values(amount,type, FALSE, IQR),
    
    
    avg_amount=all_values(amount, type, mean),
    sd_amount=all_values(amount, type, sd),
    skew_amount=all_values(amount, type, skew),
    kurtosi_amount=all_values(amount, type, kurtosi),
    median_amount=all_values(amount, type, median),
    mad_amount=all_values(amount, type, mad),
    var_amount=all_values(amount,type, var),
    iqr_amount=all_values(amount,type, IQR),
    
    
    
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

trans_by_month_final <- trans_by_month_sum %>% group_by(account_id) %>% summarize(avg_monthly_balance=mean(sum_balance))

#Joining Tables


disp_owners <- filter(disp,type=="OWNER")
clients_accounts <- subset(left_join(disp_owners, clients, by='client_id'), select=-district_id)
#account_district <- subset(left_join(account, district, by=c('district_id'='code')), select=c(account_id, district_id, name, region))
account_district <- left_join(account, district, by=c('district_id'='code'))
account_district_client <- left_join(clients_accounts, account_district, 'account_id')
trans_client_district <- left_join(trans_calculations, account_district_client, 'account_id')
trans_client_district <- left_join(trans_client_district, trans_by_month_final, 'account_id')


trans_client_district_loan <- left_join(loan, trans_client_district, 'account_id')
trans_client_district_loan$datediff <- trans_client_district_loan$date.x
trans_client_district_loan$datediff <- trans_client_district_loan$date.x - trans_client_district_loan$birthday

names(trans_client_district_loan)[names(trans_client_district_loan) == 'no..of.commited.crimes..95' ] <- 'crimes95'
names(trans_client_district_loan)[names(trans_client_district_loan) == 'no..of.commited.crimes..96' ] <- 'crimes96'

trans_client_district_loan$crimes95 <- as.numeric(trans_client_district_loan$crimes95)
trans_client_district_loan <- na.omit(trans_client_district_loan)

#trans_ratio_calculations <- trans_client_district_loan %>% 
#  group_by(loan_id) %>% 
#  summarize(
#    credit_per_witdrawal = calculate_ratio(avg_credit, avg_withdrawal),
#    credit_per_payments = calculate_ratio(avg_credit, payments),
#    crimes96_per_crime95 = calculate_ratio(crimes96, crimes95),
#    loanAmount_per_avgAmount = calculate_ratio(amount, avg_amount)
#  )

#trans_client_final <- left_join(trans_client_district_loan, trans_ratio_calculations, 'loan_id')

final <- left_join(trans_client_district_loan, card, 'disp_id')


#Cleaning + Extra attributes

names(final)[names(final) == 'date.x' ] <- 'date_loan'
names(final)[names(final) == 'date.y' ] <- 'date_account'

names(final)[names(final) == 'type.x' ] <- 'account_type'
names(final)[names(final) == 'type.y' ] <- 'card_type'

final$time_bf_loan <- final$date_account - final$date_loan
final$frequency_num <- unclass(final$frequency)
final$region_num <- unclass(final$region)
final$gender_num <- unclass(final$gender)
final$card_num <- unclass(final$card_type)
final$card_num <- ifelse(is.na(final$card_num), 0, final$card_num)
final$issued <- ifelse(is.na(final$issued), 0, final$issued)
final$sd_withdrawal <- ifelse(is.na(final$sd_withdrawal), 0, final$sd_withdrawal)
final$skew_withdrawal <- ifelse(is.na(final$skew_withdrawal), 0, final$skew_withdrawal)
final$kurtosi_withdrawal <- ifelse(is.na(final$kurtosi_withdrawal), 0, final$kurtosi_withdrawal)
final$mad_withdrawal <- ifelse(is.na(final$mad_withdrawal), 0, final$mad_withdrawal)


write.csv(final,"final.csv", row.names = FALSE)

# IDs
df_final <- subset(final, select = -loan_id )
df_final <- subset(df_final, select = -account_id )
df_final <- subset(df_final, select = -disp_id )
df_final <- subset(df_final, select = -client_id )
df_final <- subset(df_final, select = -district_id )

# Redondant Attributes
df_final <- subset(df_final, select = -amount )
df_final <- subset(df_final, select = -date_loan )
df_final <- subset(df_final, select = -birth_number )
df_final <- subset(df_final, select = -age_range )
df_final <- subset(df_final, select = -birthday )
df_final <- subset(df_final, select = -date_account )
df_final <- subset(df_final, select = -name )
df_final <- subset(df_final, select = -no..of.municipalities.with.inhabitants...499 )
df_final <- subset(df_final, select = -no..of.municipalities.with.inhabitants.2000.9999 )
df_final <- subset(df_final, select = -no..of.municipalities.with.inhabitants.500.1999 )
df_final <- subset(df_final, select = -no..of.municipalities.with.inhabitants..10000 )

# Stats
df_final <- subset(df_final, select = -sd_credit )
df_final <- subset(df_final, select = -skew_credit )
df_final <- subset(df_final, select = -kurtosi_credit )
df_final <- subset(df_final, select = -median_credit )
df_final <- subset(df_final, select = -mad_credit )
df_final <- subset(df_final, select = -var_credit )

df_final <- subset(df_final, select = -sd_withdrawal )
df_final <- subset(df_final, select = -skew_withdrawal )
df_final <- subset(df_final, select = -kurtosi_withdrawal )
df_final <- subset(df_final, select = -median_withdrawal )
df_final <- subset(df_final, select = -mad_withdrawal )
df_final <- subset(df_final, select = -var_withdrawal )

df_final <- subset(df_final, select = -sd_amount )
df_final <- subset(df_final, select = -skew_amount )
df_final <- subset(df_final, select = -kurtosi_amount )
df_final <- subset(df_final, select = -median_amount )
df_final <- subset(df_final, select = -mad_amount )
df_final <- subset(df_final, select = -var_amount )


df_final <- subset(df_final, select = -sd_balance )
df_final <- subset(df_final, select = -skew_balance )
df_final <- subset(df_final, select = -kurtosi_balance )
df_final <- subset(df_final, select = -median_balance )
df_final <- subset(df_final, select = -mad_balance )
df_final <- subset(df_final, select = -var_balance )



#date.x Loan Data Date.y account_date




 group_by_region <- trans_client_district %>% 
   group_by(region) %>% 
   summarize(
     avg_credit=mean(avg_credit),
     avg_withdrawal=mean(avg_withdrawal),
     avg_amount=mean(avg_amount),
     avg_balance=mean(avg_balance)
   )
 
 group_by_gender <- trans_client_district %>% 
   group_by(gender) %>% 
   summarize(
     avg_credit=mean(avg_credit),
     avg_withdrawal=mean(avg_withdrawal),
     avg_amount=mean(avg_amount),
     avg_balance=mean(avg_balance)
   )
 
 group_by_name <- trans_client_district %>% 
   group_by(name) %>% 
   summarize(
     avg_credit=mean(avg_credit),
     avg_withdrawal=mean(avg_withdrawal),
     avg_amount=mean(avg_amount),
     avg_balance=mean(avg_balance)
   )
 
 group_by_age_range <- trans_client_district %>% 
   group_by(age_range) %>% 
   summarize(
     avg_credit=mean(avg_credit),
     avg_withdrawal=mean(avg_withdrawal),
     avg_amount=mean(avg_amount),
     avg_balance=mean(avg_balance)
   )
#Approved Loans
ggplot(trans_client_district_loan[trans_client_district_loan$status>0,], mapping=aes(x=age_range)) + geom_histogram(binwidth = 300, stat="count")
 + theme_bw() + coord_cartesian() + scale_color_gradient()
#Paid vs not paid Loans
ggplot(trans_client_district_loan, mapping=aes(x=status, group=status)) + geom_bar(stat="count") + xlab("Status") + ylab("Number of Loans") + ggtitle("Number of Paid vs Not Paid Loans")

ggplot(data = group_by_age_range) + geom_bar(mapping=aes(x=age_range, y=avg_amount), stat="identity") + xlab('Decades') + ylab('Averaged Credited Amount (M.U)') + ggtitle('Averaged Credited Amount Grouped By Decades') + theme_bw() + coord_cartesian() + scale_color_gradient()
ggplot(data = group_by_age_range) + geom_bar(mapping=aes(x=age_range, y=avg_balance), stat="identity") + xlab('Decades') + ylab('Average Balance (M.U)') + ggtitle('Average Balance Grouped By Decades') + theme_bw() + coord_cartesian() + scale_color_gradient()
ggplot(data = trans_client_district) + geom_point(mapping=aes(x=avg_credit, y=avg_withdrawal), stat="identity") + theme_bw() + coord_cartesian() + scale_color_gradient() + geom_abline(intercept=0, slope=1, linetype="dashed", color="red", size=1.25) + xlab("Average Credit") + ylab("Average Withdrawal") + ggtitle("Comparison of the Averages Between Credit and Withdrawal (per account).") 
print(trans_calculations)
#[1] "loan_id"               "account_id"            "date.x"                "amount"               
#[5] "duration"              "payments"              "status"                "max_credit"           
#[9] "avg_credit"            "min_credit"            "max_withdrawal"        "avg_withdrawal"       
#[13] "min_withdrawal"        "avg_amount"            "avg_balance"           "max_balance"          
#[17] "min_balance"           "disp_id"               "client_id"             "type"                 
#[21] "birthday"              "gender"                "age_range"             "district_id"          
#[25] "frequency"             "date.y"                "name"                  "region"               
#[29] "hab"                   "m_hab.499"             "m_hab500.1999"         "m_hab2000.9999"       
#[33] "m_hab.10000"           "cities"                "urban_hab"             "salary"               
#[37] "unemployment95"        "unemployment96"        "enterpreneurs_per_hab" "crimes95"             
#[41] "crimes96"