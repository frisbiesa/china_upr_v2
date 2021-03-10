library(tidyverse)
library(stargazer)
library(mFilter)
library(plm)
library(lmtest)
library(fuzzyjoin)


setwd ("~/Desktop/Desktop - MacBook Pro/Data and Programming II/final project/Fortunato-Frisbie-final-project")

joint_support_scores <- read_csv("joint_support_scores.csv")
debt_database <- read_csv("HRT_ChinaDebtStockDatabase.csv", col_names = TRUE)
debt_database <- debt_database %>% select(1:7)

##overall trend of debt
debt_database %>%
  ggplot(aes(x=Year, y = ChinaDebt_GDP, na.rm = TRUE)) +
  geom_smooth()

##boxplot by year
debt_database %>%
  ggplot(aes(x=Year, y = ChinaDebt_GDP, na.rm = TRUE)) +
  geom_boxplot(aes(group = Year))

##make this into a table with stargazer

##this shows there is a lot of positive skewness
quantiles <- debt_database %>%
  group_by(Year) %>%
  summarise(year_mean = mean(ChinaDebt_GDP, na.rm = TRUE),
            year_median = median(ChinaDebt_GDP, na.rm = TRUE),
            quantile_25 = quantile(ChinaDebt_GDP, 0.25),
            quantile_75 = quantile(ChinaDebt_GDP, 0.75))
 
##https://stats.stackexchange.com/questions/76058/specifying-a-difference-in-differences-model-with-multiple-time-periods

debt_database <- debt_database %>% left_join(quantiles, by = "Year")

##the debt levels are skewed right, so here will log china debttoGDP (use log+1 to address zeros)
##run ols on level of debt to China in year before voting and overall support score

## use a loop on all of these

debt_2012 <- debt_database %>% filter(Year == 2012)
debt_2017 <- debt_database %>% filter(Year == 2017)

score_2013 <- joint_support_scores %>% select(country, support_score_13)
score_2018 <- joint_support_scores %>% select(country, support_score_18)

ols_prep_13 <- debt_2012 %>% left_join(score_2013, by = c("Country" = "country"))
ols_prep_18 <- debt_2017 %>% left_join(score_2018, by = c("Country" = "country"))

ols_13 <- lm(support_score_13 ~ log(ChinaDebt_GDP + 1) + GDP_USDbn, data = ols_prep_13)
stargazer(ols_13, type='text', header=FALSE)

ols_18 <- lm(support_score_18 ~ ChinaDebt_GDP, data = ols_prep_18)
stargazer(ols_18, type='text', header=FALSE)

##--------------------------------- diff and diff ---------------------------------------

## write a function to make this long for both recs and support

long <- joint_support_scores %>% pivot_longer(Supported_13:total_recs_18, 
                                      names_to = c("variable", "year"),
                                      names_pattern = "(.+)_(.+$)",
                                      values_to = "values")

long$year <- str_replace_all(long$year, "13", "2012")
long$year <-  str_replace_all(long$year, "18", "2017")

long$year <- as.numeric(long$year)
long$total_recommendations <- NULL

support_long <-long %>% filter(variable == "support_score")

debt_subset <- debt_database %>% filter(Year == 2012|Year == 2017)

diff_prep <- debt_subset %>% left_join(support_long, by = c("Country" = "country", "Year" = "year"))

diff_prep <- diff_prep %>%
  mutate(time_dummy = if_else(Year == "2017", 1, 0))

log(diff_prep$ChinaDebt_GDP+1)
##-------------------------------------cleaning and joining control variables-----------------------

##prep distance to china as a control variable

distance_china <- read_csv("distance_china.csv")
distance_china$X1 <- NULL

debt_subset <- debt_subset %>% left_join(distance_china, by = c("Country" = "country"), ignore_case = TRUE)

debt_subset$distance[debt_subset$Country == "Cote d'Ivoire"] <- 11255

##

devecon <- read_csv("/Users/sonnetfrisbie/Desktop/Desktop - MacBook Pro/ISDC/R working directory ISDC/DEVECON_fulldataset.csv")

#EIU_TDBT: Total_foreign_debt_[Y]
#EIU_TDPY: Total_debt/GDP_[Y]
#WB_ny_gdp_pcap_cd: GDP per capita (current US$)

controls <- devecon %>% 
  select(iso, year, EIU_TDBT, EIU_TDPY, WB_ny_gdp_pcap_cd) %>% 
  filter(year == 2012|year == 2017) %>%
  rename("total_foreign_debt" = "EIU_TDBT", 
         "total_debt_GDP" = "EIU_TDPY",
         "GDP_percapita" = "WB_ny_gdp_pcap_cd")

debt_subset <- debt_subset %>% left_join(controls, by = c("ISO" = "iso", "Year" = "year"))


##---------------------------------------------------------------------------------------

test <- diff_prep %>% select(Country, Year, ChinaDebt_GDP, GDP_USDbn, values, time_dummy)

test <- pdata.frame(test, index = c("Country", "Year"), drop.index = FALSE)


##without FE

ols <- lm(values ~ ChinaDebt_GDP + factor(Country) - 1, data = test)

diff_and_diff_fe <- lm(values ~ time_dummy*log(ChinaDebt_GDP+1) + factor(Country) - 1, data = test)

diff_and_diff <- lm(values ~ time_dummy*log(ChinaDebt_GDP+1), data = test)

test$log_debt_gdp <- log(test$ChinaDebt_GDP+1)

diff_prep

diff_prep %>%
  drop_na(values) %>%
  ggplot(aes(x = log(ChinaDebt_GDP+1), y = values)) +
  geom_point(aes(color = factor(Year))) +
  geom_smooth(method = "lm", aes(group = factor(Year), color = factor(Year)), se = FALSE, show.legend = TRUE)

##this is significant, but not if we log debt
coeftest(ols, vcov = vcovHC(ols, type="HC1"))

##these are not, but only measuring the difference between 2013 and 2018
coeftest(diff_and_diff, vcov = vcovHC(diff_and_diff, type="HC1"))
coeftest(diff_and_diff_fe, vcov = vcovHC(diff_and_diff_fe, type="HC1"))

##---------------------
##regressing total recommendations on the log of debt.  It appears that there is a strong negative relationsip
##between high indebtedness to China and making comments in the UPR

recs_long <- long %>% filter(variable == "total_recs")

recs_long <- debt_subset %>% left_join(recs_long, by = c("Country" = "country", "Year" = "year"))

recs_long <- recs_long %>%
  mutate(time_dummy = if_else(Year == "2017", 1, 0))

recs_long %>%
  drop_na(values) %>%
  ggplot(aes(x = log(ChinaDebt_GDP+1), y = values)) +
  geom_point(aes(color = factor(Year))) +
  geom_smooth(method = "lm", aes(group = factor(Year), color = factor(Year)), se = FALSE, show.legend = TRUE)

ols <- lm(values ~ log(ChinaDebt_GDP+1) + distance + GDP_per_capita, data = recs_long)

##note this is robust to controlling for distance from China (which should account for much of the variation in trade) 
##and also to controlling for total GDP per capita.

stargazer(ols, type='text', header=FALSE)

coeftest(ols, vcov = vcovHC(ols, type="HC1"))

diff_and_diff_fe <- lm(values ~ time_dummy*log(ChinaDebt_GDP+1) + factor(Country) - 1, data = recs_long)

diff_and_diff <- lm(values ~ time_dummy*log(ChinaDebt_GDP+1) + distance + GDP_per_capita + total_debt_GDP, data = recs_long)

coeftest(diff_and_diff, vcov = vcovHC(diff_and_diff, type="HC1"))
coeftest(diff_and_diff_fe, vcov = vcovHC(diff_and_diff_fe, type="HC1"))
##run the diff and diff on the total number of comments made




##y = a + treat + time + (treat*time) + controls + e

#time <- [0, 1] for 2013 or 2018
#treat <- continuous change in Chinese debt between 2013 and 2018
#treat*time <- diff in diff estimator

##-------------------------------------------HP_test ----------------------------------------------------


hp <- debt_database %>% select(Year, year_mean) %>% head(18)

debt_hp <- hpfilter(hp$year_mean, type = c("lambda"), freq = 1)

names(debt_hp)

objectList <- list(debt_hp$x,debt_hp$trend,debt_hp$cycle)
names(objectList) = c("unemp","trend","cycle")
sapply(objectList,class)


debt_hp %>%
  ggplot() +
  geom_line(hp$year_mean)

##coalesce is great for filling in values from another column

##--------------------------

joint_support_scores$country[!(joint_support_scores$country %in% debt_database$Country)]

unique(debt_database$Country)




