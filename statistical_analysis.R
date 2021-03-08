library(tidyverse)
library(stargazer)
install.packages("stargazer")

setwd ("~/Desktop/Desktop - MacBook Pro/Data and Programming II/final project/Fortunato-Frisbie-final-project")

joint_support_scores <- read_csv("joint_support_scores.csv")

debt_database <- read_csv("HRT_ChinaDebtStockDatabase.csv", col_names = TRUE)

debt_database <- debt_database %>% select(1:7)


target_year <- c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")
  
##create a variable of being either over or under the median for Chinese investment 

##there are 16 countries which had 0 Chinese debt to GDP for 2010-2018.  This will be our control group.  

debt_database %>%
  ggplot(aes(x=Year, y = ChinaDebt_GDP, na.rm = TRUE)) +
  geom_smooth()

debt_database %>%
  ggplot(aes(x=Year, y = ChinaDebt_GDP, na.rm = TRUE)) +
  geom_boxplot(aes(group = Year))

##make this into a table with stargazer

test <- debt_database %>%
  group_by(Year) %>%
  summarise(year_mean = mean(ChinaDebt_GDP, na.rm = TRUE),
            year_median = median(ChinaDebt_GDP, na.rm = TRUE),
            quantile_25 = quantile(ChinaDebt_GDP, 0.25),
            quantile_75 = quantile(ChinaDebt_GDP, 0.75))
 
##https://stats.stackexchange.com/questions/76058/specifying-a-difference-in-differences-model-with-multiple-time-periods

debt_database <- debt_database %>% left_join(test, by = "Year")

##the debt levels are skewed right, so for treatment I created a dummy variable based on whether a country 
##in a given year had over or below the median investment measured as a percentage of GDP.  

debt_database <- debt_database %>%
  mutate(above_median = if_else(ChinaDebt_GDP > year_median, 1, 0)) %>%
  mutate(above_mean = if_else(ChinaDebt_GDP > year_mean, 1, 0)) %>%
  mutate(above_75 = if_else(ChinaDebt_GDP > quantile_75, 1, 0))

debt_database %>%
  ggplot(aes(x=Year, y = ChinaDebt_GDP, color = above_median)) +
  geom_smooth(aes(group=above_median), method = "loess")

v <- debt_database %>%
  group_by(Country) %>%
  summarise(above_mean = sum(above_mean)) %>%
  arrange(desc(above_mean))

##run ols on level of debt to China in year before voting and overall support score

debt_2012 <- debt_database %>% filter(Year == 2012)
debt_2017 <- debt_database %>% filter(Year == 2017)

score_2013 <- joint_support_scores %>% select(country, support_score_13)
score_2018 <- joint_support_scores %>% select(country, support_score_18)

ols_prep_13 <- debt_2012 %>% left_join(score_2013, by = c("Country" = "country"))
ols_prep_18 <- debt_2017 %>% left_join(score_2018, by = c("Country" = "country"))

ols_13 <- lm(support_score_13 ~ ChinaDebt_GDP, data = ols_prep_13)
stargazer(ols_13, type='text', header=FALSE)

ols_18 <- lm(support_score_18 ~ ChinaDebt_GDP, data = ols_prep_18)
stargazer(ols_18, type='text', header=FALSE)

##----------------




