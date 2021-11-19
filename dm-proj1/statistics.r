library(tidyverse)
# disp.csv maps clients to accounts
# loans accepted dependendo do tipo de card (graph)

# cartões e clientes, contas, transações nº, loans nº (Vitor)
# average number of accounts per client


cards <- read_delim("../dm-proj1/ficheiros_competicao/card_train.csv", ";")
client <- read_delim("../dm-proj1/ficheiros_competicao/client.csv", ";")
accounts <- read_delim("../dm-proj1/ficheiros_competicao/account.csv", ";")
disp <- read_delim("../dm-proj1/ficheiros_competicao/disp.csv", ";")
trans <- read_delim("../dm-proj1/ficheiros_competicao/trans_train.csv", ";")
loans <- read_delim("../dm-proj1/ficheiros_competicao/loan_train.csv", ";")

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
