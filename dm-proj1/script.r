library(dplyr)
library(readr)

loans <- read_delim("ficheiros_competicao/loan_train.csv", delim = ";")

loans_successful <- dplyr::filter(loans, status == 1)
loans_unsuccessful <- dplyr::filter(loans, status == -1)

#print(loans %>% dplyr::select("status"))
#loans_accepted <- select(loans, status)

print("Successful loans")
print(summarise(loans_successful,avgAmount = mean(amount), avgTime = mean(duration), avgPayments = mean(payments)))

print("Unsuccessful loans")
print(summarise(loans_unsuccessful,avgAmount = mean(amount), avgTime = mean(duration), avgPayments = mean(payments)))
