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


##-------------------------------------basic stats visualizations and HP_test ----------------------------------------------------

## https://www.rdocumentation.org/packages/mFilter/versions/0.1-5/topics/hpfilter
##Hodrick-Prescott filter to see trend and cycle of debt to GDP owed to China

hp <- debt_database %>% select(Year, year_mean) %>% head(18) 
debt_hp <- hpfilter(hp$year_mean, type = c("lambda"), freq = 1)
objectList <- list(debt_hp$x,debt_hp$trend,debt_hp$cycle)
hp <- as.data.frame(do.call(cbind, objectList))
names(hp) <- c("ChinaDebt_GDP","trend","cycle")
hp$year <- 2000:2017

hp %>%
  ggplot(aes(x=year)) +
  geom_line(aes(y = ChinaDebt_GDP)) +
  geom_line(aes(y = trend), color = "red") +
  geom_line(aes(y = cycle), color = "blue")


## Note: The trend is very pronounced and seems to swamp out a lot of other variation.
## this will likely cause issues in our regressions as we will see below.  

## Using boxplot and quantiles to look at the skew of the explanatory variable
## this shows there is a lot of positive skewness, so we will log the ChinaDebt_GDP variable

##boxplot by year
debt_database %>%
  ggplot(aes(x=Year, y = ChinaDebt_GDP, na.rm = TRUE)) +
  geom_boxplot(aes(group = Year))

quantiles <- debt_database %>%
  group_by(Year) %>%
  summarise(year_mean = mean(ChinaDebt_GDP, na.rm = TRUE),
            year_median = median(ChinaDebt_GDP, na.rm = TRUE),
            quantile_25 = quantile(ChinaDebt_GDP, 0.25),
            quantile_75 = quantile(ChinaDebt_GDP, 0.75))

print(quantiles)


##-------------------------------Changing data into long format, joining --------------------------

##https://stats.stackexchange.com/questions/76058/specifying-a-difference-in-differences-model-with-multiple-time-periods
## write a function to make this long for both recs and support

long <- joint_support_scores %>% pivot_longer(Supported_13:total_recs_18, 
                                      names_to = c("variable", "year"),
                                      names_pattern = "(.+)_(.+$)",
                                      values_to = "values")

long$year <- str_replace_all(long$year, "13", "2012")
long$year <-  str_replace_all(long$year, "18", "2017")
long$year <- as.numeric(long$year)
long$total_recommendations <- NULL

long <-long %>% filter(variable == "support_score" | variable == "total_recs")



##-------------------------------cleaning and joining control variables to database--------------------

##loading distance to china as a control variable

distance_china <- read_csv("distance_china.csv")
distance_china$X1 <- NULL

debt_subset <- debt_database %>% filter(Year == 2012|Year == 2017)
debt_subset <- debt_subset %>% left_join(distance_china, by = c("Country" = "country"), ignore_case = TRUE)

##correcting a matching error
debt_subset$distance[debt_subset$Country == "Cote d'Ivoire"] <- 11255

##loading in other controls 

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

long_join <- debt_subset %>% left_join(long, by = c("Country" = "country", "Year" = "year"))

long_join <- long_join %>%
  mutate(time_dummy = if_else(Year == "2017", 1, 0)) %>%
  mutate(foreign_debt_gdp = total_foreign_debt/GDP_percapita)

  
##------------------------------------running OLS and diff and diff----------------------------------------

support_df <- long_join %>% 
  select(Country, 
         Year, ChinaDebt_GDP, GDP_USDbn, values, time_dummy, variable, distance, total_debt_GDP, foreign_debt_gdp, GDP_percapita) %>%
  filter(variable == "support_score")

support_df %>% 
  select(-Country, -variable, -Year, -values, -time_dummy, -GDP_USDbn) %>% 
  cor(method = "kendall", use = "complete.obs")


ols <- lm(values ~ ChinaDebt_GDP, data = support_df)

##model adjusted from discussion with Prof. Levy March 9

##y = a + treat + time + (treat*time) + controls + e
#time <- [0, 1] for 2013 or 2018
#treat <- continuous change in Chinese debt between 2013 and 2018
#treat*time <- diff in diff estimator

diff_and_diff_fe <- lm(values ~ time_dummy*log(ChinaDebt_GDP+1) + factor(Country) - 1, data = support_df)

diff_and_diff_1 <- lm(values ~ time_dummy*log(ChinaDebt_GDP+1), data = support_df)
diff_and_diff_2 <- lm(values ~ time_dummy*log(ChinaDebt_GDP+1) + distance, data = support_df)
diff_and_diff_3 <- lm(values ~ time_dummy*log(ChinaDebt_GDP+1) + distance + foreign_debt_gdp, data = support_df)

stargazer(diff_and_diff_1, diff_and_diff_2, diff_and_diff_3, type='text', dep.var.labels = c("Level of Support from Recommendations"), out = "diff_and_diff_support.txt", title = "Diff and Diff--Support Scores with Controls" )

stargazer(diff_and_diff_fe, type='text', header=FALSE)

##not significant and coefficients are negative

stargazer(ols, type='text', header=FALSE)
stargazer(diff_and_diff, type = "text", header = FALSE)

##If we compare the relationship between each year, we see the relationship seems to have changed.  
##This led us to believe that perhaps it is the quantity, and not the nature, of comments, which is changing.
##i.e. perhaps heavily indebted countries are less likely to make comments at all, especially if they
## are unsure of China's reaction.  

support_df %>%
  drop_na(values) %>%
  ggplot(aes(x = log(ChinaDebt_GDP+1), y = values)) +
  geom_point(aes(color = factor(Year))) +
  geom_smooth(method = "lm", aes(group = factor(Year), color = factor(Year)), se = FALSE, show.legend = TRUE) +
  labs(title = "Relationship between support and debt to China, by Year of UPR", x = "log debt to China, pct of GDP") +
  theme(legend.title = element_blank())


##------------------regressing on total recommendations instead of whether China accepted them-------
##regressing total recommendations on the log of debt.  It appears that there is a strong negative relationsip
##between high indebtedness to China and making comments in the UPR

recs_df <- long_join %>% 
  select(Country, 
         Year, ChinaDebt_GDP, GDP_USDbn, values, time_dummy, variable, distance, total_debt_GDP, foreign_debt_gdp, GDP_percapita) %>%
  filter(variable == "total_recs")

recs_long %>%
  drop_na(values) %>%
  ggplot(aes(x = log(ChinaDebt_GDP+1), y = values)) +
  geom_point(aes(color = factor(Year))) +
  geom_smooth(method = "lm", aes(group = factor(Year), color = factor(Year)), se = FALSE, show.legend = TRUE) +
  labs(title = "Relationship between total comments and debt to China, by Year of UPR", x = "log debt to China, pct of GDP") +
  theme(legend.title = element_blank())

ols_1 <- lm(values ~ log(ChinaDebt_GDP+1) , data = recs_df)
ols_2 <- lm(values ~ log(ChinaDebt_GDP+1) + distance, data = recs_df)
ols_3 <- lm(values ~ log(ChinaDebt_GDP+1) + distance + foreign_debt_gdp, data = recs_df)

##note this is robust to controlling for distance from China (which should account for much of the variation in trade) 
##and also to controlling for overall foreign debt levels.  

stargazer(ols_1, ols_2, ols_3, type='text', dep.var.labels = c("total comments"), out = "ols_comments.txt", title = "OLS--Total Comments with Controls" )

diff_and_diff_fe <- lm(values ~ time_dummy*log(ChinaDebt_GDP+1) + factor(Country) - 1, data = recs_df)

diff_and_diff_1 <- lm(values ~ time_dummy*log(ChinaDebt_GDP+1), data = recs_df)
diff_and_diff_2 <- lm(values ~ time_dummy*log(ChinaDebt_GDP+1) + distance, data = recs_df)
diff_and_diff_3 <- lm(values ~ time_dummy*log(ChinaDebt_GDP+1) + distance + foreign_debt_gdp, data = recs_df)

stargazer(diff_and_diff_1, diff_and_diff_2, diff_and_diff_3, type='text', dep.var.labels = c("total comments"), out = "diff_and_diff_comments.txt", title = "Diff and Diff--Total Comments with Controls" )

##not including the FE table in write-up--too long with coefficients

stargazer(diff_and_diff_fe, type='text', header=FALSE)

