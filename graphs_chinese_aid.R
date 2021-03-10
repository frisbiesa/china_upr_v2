
library(tidyverse)
library(gghighlight)
library(rvest)
library(utils)
library(readxl)


projects <- read.csv("projects.csv")
## check were this came from

projects %>% 
  group_by(ad_sector_names) %>% 
  summarise(commited_aid = sum(total_commitments, na.rm = "TRUE")) %>% 
  ggplot() +
  geom_bar(aes(commited_aid, fct_reorder(ad_sector_names, commited_aid)), stat = "identity") +
  theme_minimal() +
  labs(x = "", y = "")

projects %>% 
  group_by(transactions_start_year) %>% 
  summarize(commited_aid = sum(total_commitments, na.rm = "TRUE")) %>% 
  ggplot() +
  geom_bar(aes(transactions_start_year, commited_aid), stat = "identity") +
  scale_x_continuous(breaks = 2000:2014)

projects %>% 
  group_by(recipients, transactions_start_year) %>% 
  summarize(commited_aid = sum(total_commitments, na.rm = "TRUE")) %>%
  ggplot() +
  geom_line(aes(transactions_start_year, commited_aid, color = recipients), stat = "identity") +
  scale_x_continuous(breaks = 2000:2014) +
  theme(legend.position = 'none') + 
  gghighlight(max(commited_aid) > .5e+10,
            unhighlighted_params = list(size = 0.75))



library(tidyverse)
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


debt <- read.csv("debt_stock_china.csv")

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


ave %>%
  filter(trade_flow == "Export") %>% 
  ggplot() +
  geom_point(aes(china_debt_gdp, growth_rate), stat = "identity") +
  theme(legend.position = "none")


c_pd <- c_pd %>% 
  rename(country = 1) %>% 
  select(country, total_elite_visits) %>% 
  left_join(c2, by = c("country" = "partner"))

c_pd %>%
  filter(trade_flow == "Import") %>% 
  ggplot() +
  geom_point(aes(log(total_usd), total_elite_visits), stat = "identity") +
  theme(legend.position = "none")




