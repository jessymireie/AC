loans_successful <- dplyr::filter(dpersonal, status == 1)
loans_unsuccessful <- dplyr::filter(dpersonal, status == -1)

print("Successful loans")
print(summarise(loans_successful, nbWomen = sum(gender == "F"), nbMen = sum(gender == "M"), avgAge = mean(age), 
                avgSalary = mean(average_salary), avgUnemploymentRate95 = mean(unemployment95, na.rm=TRUE),
                avgUnemploymentRate96 = mean(unemployment96), avgCrimeRate95 = mean(crime95, na.rm=TRUE), avgCrimeRate96 = mean(crime96)))

###############################################


library(dplyr)
library(readr)

# Reading all files
clients <- read_delim("./ficheiros_competicao/client.csv", delim = ";")
loans <- read_delim("./ficheiros_competicao/loan_train.csv", delim = ";")
accounts <- read_delim("./ficheiros_competicao/account.csv", delim = ";")
cards <- read_delim("./ficheiros_competicao/card_train.csv", delim = ";")
districts <- read_delim("./ficheiros_competicao/district.csv", delim = ";")
disp <- read_delim("./ficheiros_competicao/disp.csv", delim = ";")
transactions <- read_delim("./ficheiros_competicao/trans_train.csv", delim = ";")


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

# Standardize Crime Values (normalization)) 
districts$crime95_S <- (districts$crime95 / districts$nb_hab)
districts$crime96_S <- (districts$crime96 / districts$nb_hab)

sum(duplicated(disp$client_id))

##################################################################################

#unique(df)
summary(df)
#rm(group_by_region2)

group_by_region <-
  aggregate(x = df$client_id,          # Specify data column
            by = list(df$region),      # Specify group indicator
            FUN = function(x) length(unique(x))) #Desired function
group_by_region$Percentage <- round(group_by_region$x*100/sum(group_by_region$x), digits=2)

central_Bohemia <- dplyr::filter(districts, region == "central Bohemia")
south_Bohemia <- dplyr::filter(districts, region == "south Bohemia")
north_Bohemia <- dplyr::filter(districts, region == "north Bohemia")
west_Bohemia <- dplyr::filter(districts, region == "west Bohemia")
east_Bohemia <- dplyr::filter(districts, region == "east Bohemia")
north_Moravia <- dplyr::filter(districts, region == "north Moravia")
south_Moravia <- dplyr::filter(districts, region == "south Moravia")
prague <- dplyr::filter(districts, region == "Prague")


print("Central Bohemia")
print(summarise(prague, nbInhabitants = sum(nb_hab), nbCities = sum(nb_cities), avgSalary = mean(avg_salary), 
                avgRatioUrbanHab = mean(ratio_urban_hab), avgEntrepeneurs = mean(nb_enterpreneurs_per1000), avgUnemploymentRate95 = mean(unemployment95, na.rm=TRUE),
                avgUnemploymentRate96 = mean(unemployment96), avgCrimeRate95 = mean(crime95_Standardized, na.rm=TRUE), avgCrimeRate96 = mean(crime96_Standardized)))

##################################################################################

df_acc <- accounts
df_acc$region <- districts$region[match(df_acc$district_id, districts$code)]

print(summarise(df_acc, centralBohemia = sum(region == "central Bohemia"), eastBohemia = sum(region == "east Bohemia"),
                northlBohemia = sum(region == "north Bohemia"), southBohemia = sum(region == "south Bohemia"),
                westBohemia = sum(region == "west Bohemia"), northMoravia = sum(region == "north Moravia"),
                southMoravia = sum(region == "south Moravia"), prague = sum(region == "Prague")
                ))

#rm(df_acc)
#################################################################################

df_acc$status <- loans$status[match(df_acc$account_id, loans$account_id)]

df_acc <- subset(df_acc, select = -account_id )
df_acc <- subset(df_acc, select = -district_id )
df_acc <- subset(df_acc, select = -frequency )
df_acc <- subset(df_acc, select = -date )
df_acc <- subset(df_acc, select = -account_creation_date )

df_acc <- na.omit(df_acc)

print(summarise(df_acc, centralBohemia = sum(region == "central Bohemia"), eastBohemia = sum(region == "east Bohemia"),
                northlBohemia = sum(region == "north Bohemia"), southBohemia = sum(region == "south Bohemia"),
                westBohemia = sum(region == "west Bohemia"), northMoravia = sum(region == "north Moravia"),
                southMoravia = sum(region == "south Moravia"), prague = sum(region == "Prague")
))


# Region's Impact on loan's success 
ggplot(df_acc, aes(x = region, group = status, fill = status)) +
  geom_bar(position = "fill") +
  ggtitle("Region impact on loan's success")

round(554*100/4500, digits=2)

summary(loans)


###########################

#Number of loans
nrow(loans)

# Number of transactions
nrow(trans)

# How many cards for each unique type of card
cards %>%
  group_by(type) %>%
  summarize(n = n())

# How many Owners and Disponents
disp %>%
  group_by(type) %>%
  summarize(n = n())

# How many Clients per Account (max is 2)
disp %>%
  group_by(account_id) %>%
  summarize(Nclients = n()) %>%
  count(Nclients)

# Number of transactions by account (maybe add per month)
trans %>%
  group_by(account_id) %>%
  summarize(Ntrans = n()) %>%
  count(Ntrans)