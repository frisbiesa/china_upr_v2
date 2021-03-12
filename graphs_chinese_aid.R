library(tidyverse)
library(countrycode)
library(gapminder)
library(stargazer)
library(WDI)

c_f <- read.csv("chinese_finance.csv")
c_pd <- read.csv("chinese_public_diplo.csv")
debt <- read.csv("debt_stock_china.csv")


debt$debt_usd <- str_remove_all(debt$debt_usd, ",")
debt$debt_usd <- as.integer(debt$debt_usd)

## diplomatic visits means nothing 

visits <- c_pd %>%
  rename(country = 1) %>% 
  select(country, year, total_elite_visits) %>% 
  inner_join(debt, by = c("country", "year"))

ggplot(visits) +
  geom_point(aes(debt_usd, total_elite_visits), stat = "identity")


## evolution of total chinese finance per continent

debt %>%
  group_by(year, continent) %>%
  summarise(total = sum(debt_usd, na.rm = TRUE)/1000000) %>% 
  ggplot() +
  geom_line(aes(year, total, color = continent), stat = "identity") +
  theme_minimal() +
  labs(title = "Evolution of chinese finance per continent (2000-2017)",
       x = "", y = "Millions of USD", color = "Continent")  +
  theme(title = element_text(face = "bold")) 

## growth rate of total chinese invsetments - we would have to fix the start

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

## distance data for controlling

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

comtrade <- rbind(read.csv("comtrade_2018.csv"), read.csv("comtrade_2013.csv"), 
                  read.csv("comtrade_2008.csv"), read.csv("comtrade_2003.csv"))

comtrade_china_total <- comtrade %>% 
  select(Year, Trade.Flow, Partner, Partner.ISO, Trade.Value..US..) %>% 
  rename(year = Year, trade_flow = Trade.Flow, country = Partner, ISO = Partner.ISO, total_usd = Trade.Value..US..) %>%
  filter(country != "World")


trade_debt <- debt %>% 
  select(ISO, year, china_debt_gdp) %>% 
  right_join(comtrade_china_total, by = c("year", "ISO"))


trade_debt$continent <- countrycode(sourcevar = trade_debt$country,
                              origin = "country.name",
                              destination = "continent")

## is there any correlation between debt and trade?

trade_debt %>% 
  filter(china_debt_gdp < 50) %>% 
  ggplot() +
  geom_point(aes(log(total_usd), china_debt_gdp), stat = "identity") +
  geom_smooth(aes(log(total_usd), china_debt_gdp))

reg <- lm(data = trade_debt, china_debt_gdp ~ total_usd + continent)

stargazer(reg, title = "Results", type = "text")

## its seems that there is no clear correlation

## lets see in Africa

africa <- trade_debt %>% 
  filter(continent == "Africa")

reg_africa <- lm(data = africa, china_debt_gdp ~ total_usd)

stargazer(reg_africa, title = "Results", type = "text")

## not much

## bar plot with types of finance

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

## sectors through time
  
finance %>% 
  group_by(year, crs_sector_name) %>% 
  summarise(total = sum(usd_defl_2014, na.rm = TRUE)/1000000) %>%
  ungroup() %>% 
  group_by(crs_sector_name) %>% 
  mutate(totaltotal = sum(total)) %>% 
  filter(totaltotal > 10000 & crs_sector_name != "Unallocated / Unspecified") %>% 
  ggplot() +
  geom_line(aes(year, total, color = crs_sector_name), size = 1, stat = "identity") +
  theme_minimal() +
  labs(title = "Evolution of chinese finance per most relevant sector (2000-2017)",
       x = "", y = "Millions of USD (2014)", color = "Sector")  +
  theme(title = element_text(face = "bold"))

  
## does trade or finance correlates with gdp per capita 
  
gdp <- WDI(indicator='NY.GDP.PCAP.KD', start=2000, end=2017) %>% 
  rename(gdp_per_capita = 3) %>% 
  select(country, gdp_per_capita, year)
  
trade_debt_gdp <- left_join(trade_debt, gdp, by = c("country", "year"))

## nothing interesting to show here
trade_debt_gdp %>% 
  filter(china_debt_gdp < 60 & gdp_per_capita < 30000) %>% 
  ggplot() +
  geom_point(aes(gdp_per_capita, total_usd), stat = "identity") +
  geom_smooth(aes(gdp_per_capita, total_usd))

## evolution of trade per continent

trade_debt_gdp %>% 
  group_by(continent, year) %>% 
  summarise(total = sum(total_usd, na.rm = TRUE)) %>% 
  ggplot() +
  geom_line(aes(year, total, color = continent), stat = "identity") +
  theme_minimal() +
  labs(title = "Evolution of chinese trade per continent (2000-2017)",
       x = "", y = "Total in USD", color = "Continent") 

