library(tidyverse)

loans <- read_delim("ficheiros_competicao/loan_train.csv", delim = ";")

loans_successful <- dplyr::filter(loans, status == 1)
loans_unsucessful <- dplyr::filter(loans, status == -1)

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
    monthNumber == 12 ~ "Dec"))

ggplot(loans, aes(x = status, y = amount)) +
  geom_boxplot() +
  ggtitle("Accepted loans according to loan amount")

ggplot(loans, aes(x = status, y = payments)) +
  geom_boxplot() +
  ggtitle("Accepted loans according to monthly pay")

ggplot(loans, aes(x = status, y = duration)) +
  geom_boxplot() +
  ggtitle("Accepted loans according to loan duration")

# Month impact on loan approval
# TODO change to ratio
ggplot(loans, aes(x = month, group = status, fill = status)) +
  geom_bar() +
  ggtitle("Month impact on loans")
  

# Year impact on loan approval
