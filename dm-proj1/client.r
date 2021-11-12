library(dplyr)
library(readr)

clients <- read_delim("ficheiros_competicao/client.csv", delim = ";")

# R adding a column to dataframe based on values in other columns:
clients <- clients %>% 
  mutate(gender = if_else((birth_number%%10000) > 1231, "F", "M"))

clients <- clients %>% 
  mutate(birthday = if_else(gender == "M", birth_number, birth_number-5000))

clients <- clients %>% 
  mutate(age = -(birth_number+19000000-20211110)%/%10000)

####

#women <- dplyr::filter(clients, gender == "F")
#men <- dplyr::filter(clients, gender == "M")


#print("Women Stats")
#print(summarise(women,nbWomen = dplyr::n(), avgAge = mean(age), youngest = min(age), oldest = max(age)))
# quantile(age)

#print("Men Stats")
#print(summarise(men, nbMen = dplyr::n(), avgAge = mean(age), youngest = min(age), oldest = max(age)))
