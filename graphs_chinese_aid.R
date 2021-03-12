
library(tidyverse)
library(gghighlight)
library(rvest)
library(utils)
library(readxl)
library(countrycode)
library(gapminder)

c_f <- read.csv("chinese_finance.csv")
c_pd <- read.csv("chinese_public_diplo.csv")
debt <- read.csv("debt_stock_china.csv")


debt$debt_usd <- str_remove_all(debt$debt_usd, ",")
debt$debt_usd <- as.integer(debt$debt_usd)

## diplomatic visits mean nothing 

visits <- c_pd %>%
  rename(country = 1) %>% 
  select(country, year, total_elite_visits) %>% 
  inner_join(debt, by = c("country", "year"))

ggplot(visits) +
  geom_point(aes(debt_usd, total_elite_visits), stat = "identity")



## evolution of total chinese finance

debt %>%
  group_by(year) %>%
  summarise(total = sum(debt_usd, na.rm = TRUE)) %>% 
  ggplot() +
  geom_line(aes(year, total), stat = "identity") +
  theme(legend.position = "none")

## growth rate of total chinese invsetments

debt %>%
  group_by(year) %>%
  summarise(total = sum(debt_usd, na.rm = TRUE)) %>% 
  arrange(year) %>%
  mutate(diff_year = year - lag(year),  
         diff_growth = total - lag(total), 
         growth_rate = (diff_growth / diff_year)/lag(total) * 100) %>% 
  ggplot() +
  geom_line(aes(year, growth_rate), stat = "identity") +
  theme(legend.position = "none")

## loan types

c_f %>% 
  group_by(loan_type) %>% 
  filter(loan_type != "") %>% 
  summarise(total = n()) %>% 
  ggplot() +
  geom_bar(aes(total, fct_reorder(loan_type, total)), stat = "identity")


## number within each type of instrument 

c_f %>% 
  group_by(flow) %>% 
  #filter(loan_type != "") %>% 
  summarise(total = n()) %>% 
  ggplot() +
  geom_bar(aes(total, fct_reorder(flow, total)), stat = "identity")

## amount per instrument
c_f %>% 
  group_by(flow) %>% 
  #filter(loan_type != "") %>% 
  summarise(total = sum(as.integer(str_remove(usd_defl_2014, ",")), na.rm = TRUE)) %>% 
  ggplot() +
  geom_bar(aes(total, fct_reorder(flow, total)), stat = "identity")

## sectors

c_f %>% 
  group_by(crs_sector_name) %>% 
  #filter(loan_type != "") %>% 
  summarise(total = sum(as.integer(str_remove(usd_defl_2014, ",")), na.rm = TRUE)) %>% 
  ggplot() +
  geom_bar(aes(total, fct_reorder(crs_sector_name, total)), stat = "identity")


health <- c_f %>% 
  filter(crs_sector_name == "Health")


## distance

distance <- read.csv("distance_to_china.csv") %>% 
  rename(country = 1)

distance$country <- str_remove(distance$country, "Distance from ")
distance$country <- str_remove(distance$country, " to China")
distance$country <- str_remove(distance$country, "China to ")

distance$distance <- str_remove(distance$distance, " km")
distance$distance <- str_remove(distance$distance, ",")
distance$distance <- as.numeric(distance$distance)

distance$mileage <- str_remove(distance$mileage, " miles")
distance$mileage <- str_remove(distance$mileage, ",")
distance$mileage <- as.numeric(distance$mileage)

distance <- distance %>% 
  select(country, distance)

### trade data

comtrade <- rbind(read.csv("comtrade_2018.csv"), read.csv("comtrade_2013.csv"))


comtrade_china_total <- comtrade %>% 
  select(Year, Trade.Flow, Partner, Partner.ISO, Trade.Value..US..) %>% 
  rename(year = Year, trade_flow = Trade.Flow, partner = Partner, ISO = Partner.ISO, total_usd = Trade.Value..US..)

un_scores <- read.csv("joint_support_scores.csv") %>% 
  select(country, diff_support_score) %>% 
  left_join(comtrade_china_total, by = c("country" = "partner"))

un_scores$continent <- countrycode(sourcevar = un_scores$country,
                                   origin = "country.name",
                                   destination = "continent")

c2 <- debt %>% 
  select(ISO, year, china_debt_gdp) %>% 
  right_join(comtrade_china_total, by = c("year", "ISO"))


c2 %>% 
  filter(trade_flow == "Import" & china_debt_gdp < 50) %>% 
  ggplot() +
  geom_point(aes(china_debt_gdp, log(total_usd)), stat = "identity") +
  geom_smooth(aes(china_debt_gdp, log(total_usd)))


ave <- c2 %>%
  group_by(ISO) %>%
  arrange(year) %>%
  mutate(diff_year = year - lag(year),  
         diff_growth = total_usd - lag(total_usd), 
         growth_rate = (diff_growth / diff_year)/lag(total_usd) * 100)




debt$continent <- countrycode(sourcevar = debt$country,
                              origin = "country.name",
                              destination = "continent")

finance <- read.csv("chinese_finance.csv")

finance$usd_defl_2014 <- str_remove_all(finance$usd_defl_2014, ",")
finance$usd_defl_2014 <- as.integer(finance$usd_defl_2014)

finance$continent <- countrycode(sourcevar = finance$recipient_condensed,
                              origin = "country.name",
                              destination = "continent")

finance_tree <- finance %>% 
  group_by(continent, crs_sector_name) %>% 
  summarise(total = sum(usd_defl_2014, na.rm = TRUE))




  ggplotly(ggplot(finance_tree) +
             geom_bar(aes(total, crs_sector_name, fill = continent), 
                      stat= "identity", position = "fill") +
             labs(title = "Chinese development finance per type and continent",
                  y = "", x = "Total", fill = "Continent") +
             theme_minimal() +
             theme(title = element_text(face = "bold"),
                   legend.position = "bottom"))

    

