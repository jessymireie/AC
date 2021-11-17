library("ggpubr")
library(tidyverse)
library(hrbrthemes)
library(viridis)

## Scatter Plot 
# looking for any connection between the Age and Average Salary
ggscatter(df, x = "age", y = "average_salary", 
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

# Loan's Acceptance Rate in function of the loan's amount
ggplot(df, aes(x = status, y = loan_amount)) +
  geom_boxplot() +
  ggtitle("Accepted loans according to loan amount")

# Loan's Acceptance Rate in function of the loan's payments
ggplot(df, aes(x = status, y = payments)) +
  geom_boxplot() +
  ggtitle("Accepted loans according to loan monthly paymenrs")

# Loan's Acceptance Rate in function of the loan's duration
ggplot(df, aes(x = status, y = loan_duration)) +
  geom_boxplot() +
  ggtitle("Accepted loans according to loan duration")

# Loan's Acceptance Rate in function of the client's average salary
ggplot(df, aes(x = status, y = average_salary)) +
  geom_boxplot() +
  ggtitle("Accepted loans according to client's district average salary")

# Loan's Acceptance Rate in function of the client's region's crime rate in 96 
ggplot(df, aes(x = status, y = crime96_S)) +
  geom_boxplot() +
  ggtitle("Accepted loans according to client's district crime rate in 96")

# Loan's Acceptance Rate in function of the client's region's crime rate in 95
ggplot(df, aes(x = status, y = crime95_S)) +
  geom_boxplot() +
  ggtitle("Accepted loans according to client's district crime rate in 95")

# Loan's Acceptance Rate in function of the client's region's unemployment rate in 95 
ggplot(df, aes(x = status, y = unemployment95)) +
  geom_boxplot() +
  ggtitle("Accepted loans according to client's district unemployment rate in 95")

# Loan's Acceptance Rate in function of the client's region's unemployment rate in 96
ggplot(df, aes(x = status, y = unemployment96)) +
  geom_boxplot() +
  ggtitle("Accepted loans according to client's district unemployment rate in 96")

# Loan's Acceptance Rate in function of the client's region's nb of entrepeneurs per 1000 inhabitants 
ggplot(df, aes(x = status, y = nb_enterpreneurs_per1000)) +
  geom_boxplot() +
  ggtitle("Accepted loans according to client's district nb of entrepeneurs per 1000 inhabitants")

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
  ggtitle("Accepted loans according to client's age")

####

# Region's Impact on loan's success 
ggplot(df, aes(x = region, group = status, fill = status)) +
  geom_bar() +
  ggtitle("Region impact on loan's success")

# Gender's Impact on loan's success 
ggplot(df, aes(x = gender, group = status, fill = status)) +
  geom_bar() +
  ggtitle("Gender's impact on loan's success")

# Account's Frequency Access Impact on loan's success 
ggplot(df, aes(x = account_freq_access, group = status, fill = status)) +
  geom_bar() +
  ggtitle("Account's Frequency Access impact on loan's success")

# Loan's Date Impact on loan's success 
ggplot(df, aes(x = loan_date, group = status, fill = status)) +
  geom_bar() +
  ggtitle("Loan's Date impact on loan's success")

####

# Jitter Plot for Account Frquency Access
ggplot(df, aes(status, account_freq_access)) +
  geom_jitter(aes(color = account_freq_access), size = 0.5)

####

# Density Plot relating age to gender
qplot(age, data = df, geom = "density",
      color = gender, linetype = gender)

####

# Creating a new dataframe purely numerical for correlation matrix calculations
df_new <- df
df_new <- df_new %>%
  # Change status to Successful (1) and Unsuccessful(-1)
  mutate(status = ifelse(status == "Successful", 1, -1)) 
df_new <- df_new %>%
  # Change gender to Female (1) and Male (-1)
  mutate(gender = ifelse(gender == "F", 1, -1)) 
df_new <- df_new %>%
  # Change account frequency access to monthly issuance (1) and issuance after transaction (2) and weekly issuance (3)
  mutate(account_freq_access = case_when(
    account_freq_access == "monthly issuance" ~ 1,
    account_freq_access == "issuance after transaction" ~ 2,
    account_freq_access == "weekly issuance" ~ 3))
df_new <- df_new %>%
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
df_new <- df_new %>%
  # Change account frequency access to monthly issuance (1) and issuance after transaction (2) and weekly issuance (3)
  mutate(card_type = case_when(
    card_type == "classic" ~ 1,
    card_type == "gold" ~ 2,
    card_type == "junior" ~ 3))

df_new <- subset(df_new, select = -account_id )
df_new <- subset(df_new, select = -client_id )
df_new <- subset(df_new, select = -district_id )

# Dropping the card attributes here, since 94.8% of their entries would be na
df_new <- subset(df_new, select = -card_type )
df_new <- subset(df_new, select = -card_issue_date )

# Removing Null Values
# not actually doing this now, bc card's attributes have a lot of na values
df_new <- na.omit(df_new)


# 1. Compute correlation
# Can be run using 3 different methods : 1- pearson, 2- kendall, 3-spearman -- pearson is considered the most fitting overall
cormat <- round(cor(df_new, method = "pearson"),2)
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
# Overall Analytics, probably more useful of df with fewer columns
chart.Correlation(df_new, histogram=TRUE, pch=19)

#######################

library(corrplot)
# correlation Matrix but make it more aesthetically appealing
corrplot(cormat, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

