library(dplyr)
library(readr)

# Reading all files
clients <- read_delim("ficheiros_competicao/client.csv", delim = ";")
loans <- read_delim("ficheiros_competicao/loan_train.csv", delim = ";")
accounts <- read_delim("ficheiros_competicao/account.csv", delim = ";")
cards <- read_delim("ficheiros_competicao/card_train.csv", delim = ";")
districts <- read_delim("ficheiros_competicao/district.csv", delim = ";")
disp <- read_delim("ficheiros_competicao/disp.csv", delim = ";")
trans <- read_delim("ficheiros_competicao/trans_train.csv", delim = ";")


# Cleaning Client Dataframe
# Decomposing birthnumber into gender and age
clients <- clients %>% 
  mutate(gender = if_else((birth_number%%10000) > 1231, "F", "M"))

# birthday is actually useless for data mining / processing purposes
#clients <- clients %>% 
  #mutate(birthday = if_else(gender == "M", birth_number, birth_number-5000))

clients <- clients %>% 
  mutate(age = -(birth_number+19000000-20211119)%/%10000)


# Cleaning District Dataframe
# Fixing Column Names
names(districts)[names(districts) == 'code '] <- 'code'
names(districts)[names(districts) == 'name '] <- 'name'
names(districts)[names(districts) == 'no. of inhabitants'] <- 'nb_hab'
names(districts)[names(districts) == 'no. of cities '] <- 'nb_cities'
names(districts)[names(districts) == 'ratio of urban inhabitants '] <- 'ratio_urban_hab'
names(districts)[names(districts) == 'average salary '] <- 'avg_salary'
names(districts)[names(districts) == 'unemploymant rate \'95 ' ] <- 'unemployment95'
names(districts)[names(districts) == 'unemploymant rate \'96 ' ] <- 'unemployment96'
names(districts)[names(districts) == 'no. of enterpreneurs per 1000 inhabitants '] <- 'nb_enterpreneurs_per1000'
names(districts)[names(districts) == 'no. of commited crimes \'95 ' ] <- 'crime95'
names(districts)[names(districts) == 'no. of commited crimes \'96 ' ] <- 'crime96'

# Unemployment and Crime Rate from '95 are stored as Characters instead of Numeric values
df <- districts[,c("unemployment95","crime95")]
df[sapply(df, is.character)] <-lapply(df[sapply(df, is.character)], as.numeric)
districts$unemployment95 <- df$unemployment95
districts$crime95 <- df$crime95
rm(df)

# Standardize Crime Values (normalization)
#districts$crime95_Standardized <- (districts$crime95 - mean(districts$crime95, na.rm = TRUE)) / sd(districts$crime95, na.rm = TRUE)
#districts$crime96_Standardized <- (districts$crime96 - mean(districts$crime96, na.rm = TRUE)) / sd(districts$crime96, na.rm = TRUE)
districts$crime95_Standardized <- (districts$crime95 / districts$nb_hab)
districts$crime96_Standardized <- (districts$crime96 / districts$nb_hab)


# Cleaning Account Dataframe
# Simplifying creation date into MMYY only
accounts <- accounts %>% 
  mutate(account_creation_date = date%/%100)


# Cleaning Card Dataframe
# Simplifying creation date into MMYY only
cards <- cards %>% 
  mutate(card_issue_date = issued%/%100)


# Check if any client has more to one account to their name
sum(duplicated(disp$client_id))

# Cleaning Loan Request Date into Month (numeric and char) - Year
# Changing Status to Numeric Value
loans <- loans %>%
  # Change status to Successful (1) and Unsuccessful(-1)
  mutate(status = ifelse(status == 1, "Successful", "Unsuccessful")) %>%
  # Create 3 letter abbreviation of month column based off date column
  transform(monthNumber = (date %% 10000) %/% 100) %>%
  mutate(month = case_when(
    monthNumber == 1 ~ "Jan",
    monthNumber == 2 ~ "Feb",
    monthNumber == 3 ~ "Mar",
    monthNumber == 4 ~ "Apr",
    monthNumber == 5 ~ "May",
    monthNumber == 6 ~ "Jun",
    monthNumber == 7 ~ "Jul",
    monthNumber == 8 ~ "Aug",
    monthNumber == 9 ~ "Sep",
    monthNumber == 10 ~ "Oct",
    monthNumber == 11 ~ "Nov",
    monthNumber == 12 ~ "Dec")) %>%
  mutate(year = date %/% 10000)


# Cleaning and Generating new Columns For Accounts and Transactions 
# Aggregating Transactions by originator Account
group_by_account <-
  aggregate(x = trans$trans_id,          
            by = list(trans$account_id),      
            FUN = function(x) length(unique(x))) 
list=as.list(group_by_account)
list$Group.1
# Keeping only account that have at least one transaction
accounts<-subset(accounts, (account_id %in% list$Group.1))
# Adding a new column - Nb of transactions made by this account
accounts$nb_transactions <- group_by_account$x[match(group_by_account$Group.1, accounts$account_id)]
rm(list)
rm(group_by_account)



# create new dataframe containing all info
df <- disp[,c("account_id","client_id", "disp_id")]

#clients
df$gender <- clients$gender[match(df$client_id, clients$client_id)]
df$age <- clients$age[match(df$client_id, clients$client_id)]
df$district_id <- clients$district_id[match(df$client_id, clients$client_id)]

#district
df$region <- districts$region[match(df$district_id, districts$code)]
df$nb_inhabitants <- districts$nb_hab[match(df$district_id, districts$code)]
df$nb_cities <- districts$nb_cities[match(df$district_id, districts$code)]
df$ratio_urban_inhabitants <- districts$ratio_urban_hab[match(df$district_id, districts$code)]
df$average_salary <- districts$avg_salary[match(df$district_id, districts$code)]
df$nb_enterpreneurs_per1000 <- districts$nb_enterpreneurs_per1000[match(df$district_id, districts$code)]
df$unemployment95 <- districts$unemployment95[match(df$district_id, districts$code)]
df$crime95_S <- districts$crime95_Standardized[match(df$district_id, districts$code)]
df$unemployment96 <- districts$unemployment96[match(df$district_id, districts$code)]
df$crime96_S <- districts$crime96_Standardized[match(df$district_id, districts$code)]

#accounts
df$account_creation_date <- accounts$account_creation_date[match(df$account_id, accounts$account_id)]
df$account_freq_access <- accounts$frequency[match(df$account_id, accounts$account_id)]
df$nb_transactions <- accounts$nb_transactions[match(df$account_id, accounts$account_id)]

#loans
df$loan_amount <- loans$amount[match(df$account_id, loans$account_id)]
df$loan_duration <- loans$duration[match(df$account_id, loans$account_id)]
df$payments <- loans$payments[match(df$account_id, loans$account_id)]
df$status <- loans$status[match(df$account_id, loans$account_id)]
df$loan_date_month <- loans$month[match(df$account_id, loans$account_id)]
df$loan_date_year <- loans$year[match(df$account_id, loans$account_id)]
df$loan_date_monthNB <- loans$monthNumber[match(df$account_id, loans$account_id)]

# removing clients who haven't requested any loans
df <- na.omit(df)

#cards
df$card_issue_date <- cards$card_issue_date[match(df$disp_id, cards$disp_id)]
df$card_type <- cards$type[match(df$disp_id, cards$disp_id)]

# disp_id is now useless
df <- subset(df, select = -disp_id )


#####################################################################################                          


# Retrieving how many client's each account has associated to it
# Grouping Accounts and Respective Client IDS
nb_of_clients_per_account <-
  aggregate(x = df$client_id,         
            by = list(df$account_id),      
            FUN = function(x) length(unique(x))) 
# Renaming columns for simplicity
names(nb_of_clients_per_account)[names(nb_of_clients_per_account) == 'Group.1'] <- 'account_id'
names(nb_of_clients_per_account)[names(nb_of_clients_per_account) == 'x'] <- 'nb_of_clients'
# Converting Dataframe to List for easier access
list=as.list(nb_of_clients_per_account)
# We're only keeping accounts that have an associated client to them (usually all of them, but better safe than sorry)
df <-subset(df, (account_id %in% list$account_id))
rm(list)

# DATAFRAMES COM DIMENS�ES DIFERENTES N�O D�O MATCH
# PODE-SE RETIRAR TODAS AS ACCOUNTS REPETIDAS, MAS PERDE-SE OS ATRIBUTOS age, gender...
# Adding Column containing the number of client's per account
#df$nb_clients_per_acc <- nb_of_clients_per_account$nb_of_clients[match(nb_of_clients_per_account$account_id, df$account_id)]
df_noperso <- df
df_noperso <- subset(df_noperso, select = -client_id )
df_noperso <- subset(df_noperso, select = -gender )
df_noperso <- subset(df_noperso, select = -age )
df_noperso <- subset(df_noperso, select = -card_type )
df_noperso <- subset(df_noperso, select = -card_issue_date )
df_noperso <- distinct(df_noperso)
df_noperso$nb_clients_per_acc <- nb_of_clients_per_account$nb_of_clients[match(nb_of_clients_per_account$account_id, df_noperso$account_id)]
rm(nb_of_clients_per_account)

#####################################################################################

# DATES 
#df_trans <- loans[,c("account_id","date", "amount", "duration","payments")]
#names(df_trans)[names(df_trans) == 'date'] <- 'loan_date'

#df_trans$loan_date <- as.Date(as.character(df_trans$loan_date), format='%y%m%d')
#trans$date <- as.Date(as.character(trans$date), format='%y%m%d')
#trans <- arrange(trans, date)
#df_trans$last_trans_bf_loan <- trans$trans_id[match(trans$account_id, df_trans$account_id) + tail(which(trans$date < df_trans$loan_date),1)]


#########################################################################################

# Creating a new purely numerical dataframe for correlation matrix calculations
df_num <- df
df_num <- df_num %>%
  # Change status to Successful (1) and Unsuccessful(-1)
  mutate(status = ifelse(status == "Successful", 1, -1)) 
df_num <- df_num %>%
  # Change gender to Female (1) and Male (-1)
  mutate(gender = ifelse(gender == "F", 1, -1)) 
df_num <- df_num %>%
  # Change account frequency access to monthly issuance (1) and issuance after transaction (2) and weekly issuance (3)
  mutate(account_freq_access = case_when(
    account_freq_access == "monthly issuance" ~ 1,
    account_freq_access == "issuance after transaction" ~ 2,
    account_freq_access == "weekly issuance" ~ 3))
df_num <- df_num %>%
  # Change region to 
  mutate(region = case_when(
    region == "central Bohemia" ~ 1,
    region == "east Bohemia" ~ 2,
    region == "north Bohemia" ~ 3,
    region == "north Moravia" ~ 4,
    region == "Prague" ~ 5,
    region == "south Bohemia" ~ 6,
    region == "south Moravia" ~ 7, 
    region == "west Bohemia" ~ 8))
df_num <- df_num %>%
  # Change account frequency access to monthly issuance (1) and issuance after transaction (2) and weekly issuance (3)
  mutate(card_type = case_when(
    card_type == "classic" ~ 1,
    card_type == "gold" ~ 2,
    card_type == "junior" ~ 3))
df_num <- subset(df_num, select = -loan_date_month )
# Too many NA values (about 94.8%)
df_num <- subset(df_num, select = -card_type )
df_num <- subset(df_num, select = -card_issue_date )
# Removing any left behind Null Values (theoretically 0)
df_num <- na.omit(df_num)

###################################################################################


loans <- read_delim("ficheiros_competicao/loan_train.csv", delim = ";")
accounts <- read_delim("ficheiros_competicao/account.csv", delim = ";")
cards <- read_delim("ficheiros_competicao/card_train.csv", delim = ";")
disp <- read_delim("ficheiros_competicao/disp.csv", delim = ";")


new_df <- disp[,c("account_id", "disp_id")]
new_df$status <- loans$status[match(new_df$account_id, loans$account_id)]
new_df <- na.omit(new_df)
new_df$card <- cards$type[match(new_df$disp_id, cards$disp_id)]
