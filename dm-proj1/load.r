library(dplyr)
library(readr)

# Reading all files
clients <- read_delim("ficheiros_competicao/client.csv", delim = ";")
loans <- read_delim("ficheiros_competicao/loan_train.csv", delim = ";")
accounts <- read_delim("ficheiros_competicao/account.csv", delim = ";")
cards <- read_delim("ficheiros_competicao/card_train.csv", delim = ";")
districts <- read_delim("ficheiros_competicao/district.csv", delim = ";")
disp <- read_delim("ficheiros_competicao/disp.csv", delim = ";")


# Cleaning Client Dataframe
# Decomposing birthnumber into gender and age
clients <- clients %>% 
  mutate(gender = if_else((birth_number%%10000) > 1231, "F", "M"))

# birthday is actually useless for data mining / processing purposes
#clients <- clients %>% 
  #mutate(birthday = if_else(gender == "M", birth_number, birth_number-5000))

clients <- clients %>% 
  mutate(age = -(birth_number+19000000-20211112)%/%10000)


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

# create new dataframe containing all info
df <- disp[,c("account_id","client_id", "disp_id")]
#df$disp_id <- disp$disp_id[match(df$client_id, disp$client_id)]

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
#df$crime95 <- districts$crime95[match(df$district_id, districts$code)]
df$crime95_S <- districts$crime95_Standardized[match(df$district_id, districts$code)]
df$unemployment96 <- districts$unemployment96[match(df$district_id, districts$code)]
#df$crime96 <- districts$crime96[match(df$district_id, districts$code)]
df$crime96_S <- districts$crime96_Standardized[match(df$district_id, districts$code)]

#accounts
df$account_creation_date <- accounts$account_creation_date[match(df$account_id, accounts$account_id)]
df$account_freq_access <- accounts$frequency[match(df$account_id, accounts$account_id)]

#cards
df$card_issue_date <- cards$card_issue_date[match(df$disp_id, cards$disp_id)]
df$card_type <- cards$type[match(df$disp_id, cards$disp_id)]

#loans
#df$loan_date <- loans$date[match(df$account_id, loans$account_id)]
df$loan_amount <- loans$amount[match(df$account_id, loans$account_id)]
df$loan_duration <- loans$duration[match(df$account_id, loans$account_id)]
df$payments <- loans$payments[match(df$account_id, loans$account_id)]
df$status <- loans$status[match(df$account_id, loans$account_id)]
df$loan_date_month <- loans$month[match(df$account_id, loans$account_id)]
df$loan_date_year <- loans$year[match(df$account_id, loans$account_id)]
df$loan_date_monthNB <- loans$monthNumber[match(df$account_id, loans$account_id)]
# removing clients who haven't requested any loans
df <- na.omit(df)
# Simplifying loan date into MMYY only
#df <- df %>% 
#  mutate(loan_date = loan_date%/%100)

# disp_id is now useless
df <- subset(df, select = -disp_id )

#write.csv(df,"C:/Users/xanaf/Desktop/feup/4 ano/1 semestre/ac/repos/AC/dm-proj1/final.csv", row.names = FALSE)                           
