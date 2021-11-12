library(dplyr)
library(readr)

loans <- read_delim("ficheiros_competicao/loan_train.csv", delim = ";")
clients <- read_delim("ficheiros_competicao/client.csv", delim = ";")
accounts <- read_delim("ficheiros_competicao/account.csv", delim = ";")
cards <- read_delim("ficheiros_competicao/card_train.csv", delim = ";")
districts <- read_delim("ficheiros_competicao/district.csv", delim = ";")
disp <- read_delim("ficheiros_competicao/disp.csv", delim = ";")



# fix col names
names(districts)[names(districts) == 'code '] <- 'code'
names(districts)[names(districts) == 'name '] <- 'name'
names(districts)[names(districts) == 'no. of inhabitants'] <- 'nb_inhabitants'
names(districts)[names(districts) == 'no. of cities '] <- 'nb_cities'
names(districts)[names(districts) == 'ratio of urban inhabitants '] <- 'ratio_urban_inhabitants'
names(districts)[names(districts) == 'average salary '] <- 'average_salary'
names(districts)[names(districts) == 'unemploymant rate \'95 ' ] <- 'unemployment95'
names(districts)[names(districts) == 'unemploymant rate \'96 ' ] <- 'unemployment96'
names(districts)[names(districts) == 'no. of enterpreneurs per 1000 inhabitants '] <- 'nb_enterpreneurs_per1000'
names(districts)[names(districts) == 'no. of commited crimes \'95 ' ] <- 'crime95'
names(districts)[names(districts) == 'no. of commited crimes \'96 ' ] <- 'crime96'


# create new dataframe containing all info
dpersonal <- disp[,c("account_id","client_id")]



#clients
dpersonal$gender <- clients$gender[match(dpersonal$client_id, clients$client_id)]
dpersonal$birthday <- clients$birthday[match(dpersonal$client_id, clients$client_id)]
dpersonal$age <- clients$age[match(dpersonal$client_id, clients$client_id)]
dpersonal$district_id <- clients$district_id[match(dpersonal$client_id, clients$client_id)]

#district
dpersonal$region <- districts$region[match(dpersonal$district_id, districts$code)]
dpersonal$nb_inhabitants <- districts$nb_inhabitants[match(dpersonal$district_id, districts$code)]
dpersonal$nb_cities <- districts$nb_cities[match(dpersonal$district_id, districts$code)]
dpersonal$ratio_urban_inhabitants <- districts$ratio_urban_inhabitants[match(dpersonal$district_id, districts$code)]
dpersonal$average_salary <- districts$average_salary[match(dpersonal$district_id, districts$code)]
dpersonal$nb_enterpreneurs_per1000 <- districts$nb_enterpreneurs_per1000[match(dpersonal$district_id, districts$code)]
dpersonal$unemployment95 <- districts$unemployment95[match(dpersonal$district_id, districts$code)]
dpersonal$crime95 <- districts$crime95[match(dpersonal$district_id, districts$code)]
dpersonal$unemployment96 <- districts$unemployment96[match(dpersonal$district_id, districts$code)]
dpersonal$crime96 <- districts$crime96[match(dpersonal$district_id, districts$code)]

#status of loan
dpersonal$status <- loans$status[match(dpersonal$account_id, loans$account_id)]

dpersonal <- na.omit(dpersonal)

# birthdate are actually useless
dpersonal <- subset(dpersonal, select = -birthday)

# unemployment and crime rate from '95 are stored as char instead of numeric
transform(dpersonal, unemployment95 = as.numeric(unemployment95), crime95 = as.numeric(crime95))
