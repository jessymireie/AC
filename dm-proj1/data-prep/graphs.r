library("ggpubr")
library(tidyverse)
library(hrbrthemes)
library(viridis)

####### PLOTS #########

loans <- read_delim("./ficheiros_competicao/loan_train.csv", delim = ";")

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


####### PLOTS #########

df <- read.csv('./complete_train.csv',sep = ',', header=TRUE)

## Scatter Plot 
# looking for any connection between the Age and Average Salary
ggscatter(df, x = "age", y = "avg_salary", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Age", ylab = "Average Salary")

# normalizing the results for a more visual inspection
shapiro.test(df$age)
ggqqplot(df, 'age')


## Box Plots
# 2 variable box plot - having the status of the loan as the x variable
df <- df %>%
  # Change status to Successful (1) and Unsuccessful(-1)
  mutate(status = ifelse(status == 1, "Successful", "Unsuccessful")) 

# geom_boxplot
# geom_jitter
# geom_violin

###################################

ggplot(loans, aes(x = status, y = amount)) +
  geom_boxplot() +
  ggtitle("Loans' Results in function of the Requested Loan Amount")

ggplot(loans, aes(x = status, y = payments)) +
  geom_boxplot() +
  ggtitle("Loans' Results in function of the Monthly Loan Payments")

ggplot(loans, aes(x = status, y = duration)) +
  geom_boxplot() +
  ggtitle("Loans' Results in function of the Requested Loan Duration")

# Month impact on loan approval
ggplot(loans, aes(x = month, group = status, fill = status)) +
  geom_bar(position = "fill") +
  ggtitle("Month's impact on Loans")

# Year impact on loan approval
ggplot(loans, aes(x = year, group = status, fill = status)) +
  geom_bar(position = "fill") +
  ggtitle("Year's impact on Loans")

#####################################################

# Loan's Acceptance Rate in function of the loan's amount
ggplot(df, aes(x = status, y = payments_loan*duration_loan)) +
  geom_boxplot() +
  ggtitle("Accepted loans according to loan amount")

# Loan's Acceptance Rate in function of the loan's payments
ggplot(df, aes(x = status, y = payments_loan)) +
  geom_boxplot() +
  ggtitle("Accepted loans according to loan monthly paymenrs")

# Loan's Acceptance Rate in function of the loan's duration
ggplot(df, aes(x = status, y = duration_loan)) +
  geom_boxplot() +
  ggtitle("Accepted loans according to loan duration")

# Loan's Acceptance Rate in function of the client's average salary
ggplot(df, aes(x = status, y = avg_salary)) +
  geom_boxplot() +
  ggtitle("Loans' Results in function of the Client's District's Average Salary")

# Loan's Acceptance Rate in function of the client's region's crime rate in 96 
ggplot(df, aes(x = status, y = crime_96)) +
  geom_boxplot() +
  ggtitle("Accepted loans according to client's district crime rate in 96")

# Loan's Acceptance Rate in function of the client's region's crime rate in 95
ggplot(df, aes(x = status, y = crime_95)) +
  geom_boxplot() +
  ggtitle("Accepted loans according to client's district crime rate in 95")

# Loan's Acceptance Rate in function of the client's region's unemployment rate in 96
ggplot(df, aes(x = status, y = unemployment_96)) +
  geom_boxplot() +
  ggtitle("Accepted loans according to client's district unemployment rate in 96")

# Loan's Acceptance Rate in function of the client's region's nb of entrepreneurs per 1000 inhabitants 
ggplot(df, aes(x = status, y = nb_entrepeneurs)) +
  geom_boxplot() +
  ggtitle("Loans' Results in function of the Client's District's Number of Entrepreneurs per 1000 inhabitants")

# Loan's Acceptance Rate in function of the client's region's ratio urban inhabitants 
ggplot(df, aes(x = status, y = ratio_urban_inhabitants)) +
  geom_boxplot() +
  ggtitle("Accepted loans according to client's district ratio of urban inhabitants")

# Loan's Acceptance Rate in function of the client's region's nb of cities 
ggplot(df, aes(x = status, y = nb_cities)) +
  geom_boxplot() +
  ggtitle("Accepted loans according to client's district nb of cities")

# Loan's Acceptance Rate in function of the client's region's nb of inhabitants 
ggplot(df, aes(x = status, y = nb_inhabitants)) +
  geom_boxplot() +
  ggtitle("Accepted loans according to client's district nb of inhabitants")

# Loan's Acceptance Rate in function of the client's age 
ggplot(df, aes(x = status, y = age)) +
  geom_boxplot() +
  ggtitle("Loans' Results in function of the Client's Age")

####

# Region's Impact on loan's success 
ggplot(df, aes(x = region, group = status, fill = status)) +
  geom_bar(position = "fill") +
  ggtitle("Region's Impact on Loan's success")

# Gender's Impact on loan's success 
ggplot(df, aes(x = gender, group = status, fill = status)) +
  geom_bar(position = "fill") +
  ggtitle("Gender's impact on loan's success")

# Account's Frequency Access Impact on loan's success 
ggplot(df, aes(x = frequency_num, group = status, fill = status)) +
  geom_bar(position = "fill") +
  ggtitle("Account's Frequency Access impact on loan's success")

# Loan's Date Impact on loan's success 
ggplot(df, aes(x = loan_date_month, group = status, fill = status)) +
  geom_bar(position = "fill") +
  ggtitle("Month Impact on Loan's success")

####

# Jitter Plot for Account Frequency Access
ggplot(df, aes(status, frequency_num)) +
  geom_jitter(aes(color = frequency_num), size = 0.5)

####

# Density Plot relating age to gender
qplot(age, data = df, geom = "density",
      color = gender, linetype = gender)

####

df_num <- subset(df, select = -status )
df_num <- df_num %>% mutate(card_num = case_when(card_num == 'none' ~ 0, card_num == 'classic' ~ 1, card_num == 'junior' ~ 2, card_num == 'gold' ~ 3))
df_num <- df_num %>% mutate(frequency_num = case_when( frequency_num == 'monthly issuance' ~ 0, frequency_num == 'weekly issuance' ~ 1, frequency_num == 'issuance after transaction' ~ 2))
df_num <- df_num %>% mutate(gender = case_when(gender == 'F' ~ -1, gender == 'M' ~ 1))


# 1. Compute correlation
# Can be run using 3 different methods : 1- pearson, 2- kendall, 3-spearman -- pearson is considered the most fitting overall
cormat <- round(cor(df_num, method = "pearson"),2)
# 2. Reorder the correlation matrix by 
# Hierarchical clustering
hc <- hclust(as.dist(1-cormat)/2)
cormat.ord <- cormat[hc$order, hc$order]
# 3. Get the upper triangle
cormat.ord[lower.tri(cormat.ord)]<- NA
# 4. Melt the correlation matrix
require(reshape2)
melted_cormat <- melt(cormat.ord, na.rm = TRUE)
# Create the heatmap
ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") + # Change gradient color
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed() #+
  # This would print the actual correlation calculations on each cell (since our matrix is dense, it becomes to messy to read)
  #geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  #theme(
  #  axis.title.x = element_blank(),
  #  axis.title.y = element_blank(),
  #  panel.grid.major = element_blank(),
  #  panel.border = element_blank(),
  #  panel.background = element_blank(),
  #  axis.ticks = element_blank(),
  #  legend.justification = c(1, 0),
  #  legend.position = c(0.6, 0.7),
  #  legend.direction = "horizontal")+
  #guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
  #                             title.position = "top", title.hjust = 0.5))

#######################

library("PerformanceAnalytics")
# Overall Analytics, probably more useful if df had fewer columns
chart.Correlation(df_num, histogram=TRUE, pch=19)

#######################

library(corrplot)
# correlation Matrix but make it more aesthetically appealing
corrplot(cormat, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#######################


df_district <- df_num[,c("region","nb_inhabitants", "nb_cities", "ratio_urban_inhabitants", "average_salary", "nb_enterpreneurs_per1000", "unemployment95",  "crime95_S", "crime96_S")]

cormat <- round(cor(df_district, method = "pearson"),2)
corrplot(cormat, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# 2. Reorder the correlation matrix by 
# Hierarchical clustering
hc <- hclust(as.dist(1-cormat)/2)
cormat.ord <- cormat[hc$order, hc$order]
# 3. Get the upper triangle
cormat.ord[lower.tri(cormat.ord)]<- NA
# 4. Melt the correlation matrix
require(reshape2)
melted_cormat <- melt(cormat.ord, na.rm = TRUE)
# Create the heatmap
ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") + # Change gradient color
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed() + #
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

###########################################

df_loans <- df_num[,c("gender","age", "avg_salary", "duration_loan", "payments_loan")]

cormat <- round(cor(df_loans, method = "pearson"),2)
corrplot(cormat, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# 2. Reorder the correlation matrix by 
# Hierarchical clustering
hc <- hclust(as.dist(1-cormat)/2)
cormat.ord <- cormat[hc$order, hc$order]
# 3. Get the upper triangle
cormat.ord[lower.tri(cormat.ord)]<- NA
# 4. Melt the correlation matrix
require(reshape2)
melted_cormat <- melt(cormat.ord, na.rm = TRUE)
# Create the heatmap
ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") + # Change gradient color
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed() + #
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
