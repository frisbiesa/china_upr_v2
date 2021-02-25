
library(tidyverse)
library(gghighlight)

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



### https://github.com/AidData-WM/public_datasets/raw/master/chinaglobal/GlobalChineseOfficialFinanceDataset_v1.0.zip

## read zip - read csv

## TRADE DATA
## https://comtrade.un.org/Data/ 


## public diplomacy
## https://github.com/AidData-WM/public_datasets/blob/master/ChinaPublicDiplomacyDashboardDataset_v1.0.zip?raw=true
## zip