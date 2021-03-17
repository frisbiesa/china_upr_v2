library(tidyverse)
library(RColorBrewer)
library(purrr)
library(readr)
library(reshape2)
library(rlist)
library(varhandle)
library(forcats)
library(treemap)
library(d3treeR)
library(htmlwidgets)

clean_2013 <- read_csv("text_cleaning_list_2013.csv")
clean_2018 <- read_csv("text_cleaning_list_2018.csv")

###------------------------------------ starting to plot and analyze ------------------------------

##looking at countries by how many supported vs. noted recommendations they made

get_support_scores <- function(x){
  country_support_score <- x %>% 
    group_by(country, position) %>%
    summarise(n = n())
  
  country_support_score <-
    country_support_score %>%
    pivot_wider(names_from = position, values_from = n) %>%
    replace_na(list(Noted = 0)) %>%
    mutate(support_score = Supported-Noted)
}

##subtract noted from supported measures for each session and this is the country's "support score" for that 
##session.  Then, subtract the 2013 support score from the 2018 support score.  A positive value will mean 
##that country was easier on China in 2018 than 2013.  A negative score will mean the opposite.  Since
##all numbers are relative, magnitude of numbers of comments shouldnt matter. 

support_score_2013 <- get_support_scores(clean_2013)
support_score_2018 <- get_support_scores(clean_2018)


joint_support_scores <- full_join(support_score_2013, support_score_2018, by = "country", suffix = c("_13", "_18"))



joint_support_scores <- joint_support_scores %>%
  mutate_if(is.numeric, replace_na, replace = 0) %>%
  mutate(total_recs_13 = Supported_13 + Noted_13) %>%
  mutate(total_recs_18 = Supported_18 + Noted_18) %>%
  mutate(total_recommendations = Supported_13 + Noted_13 + Supported_18 + Noted_18) 

##writing to use later in regression

#write_csv(joint_support_scores, "joint_support_scores.csv")

joint_support_scores <- joint_support_scores %>% 
  mutate(diff_support_score = support_score_18-support_score_13) %>%
  ungroup(country)

joint_support_scores %>% 
  filter(!is.na(diff_support_score)) %>%
  select(country, support_score_13, support_score_18) %>%
  pivot_longer(cols = support_score_13:support_score_18, names_to = "year", values_to = "score") %>%
  ggplot(aes(x = year, y = score, fill = year)) +
  geom_boxplot()

##compression towards the mean
##backs up text analysis 

joint_support_scores %>%
  filter(!is.na(diff_support_score)) %>%
  mutate(country = fct_reorder(country, diff_support_score)) %>%
  ggplot(aes(x = country, y = diff_support_score, fill = diff_support_score)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95)) +
  theme(legend.position = "none")




##---------------------------------------------------------------

clean_2013

unique(clean_2013$theme_code)

require(data.table)

theme_response <- clean_2013 %>% 
  group_by(position, theme_code, description) %>%
  summarise(n = n()) %>%
  group_by(position) %>%
  arrange(desc(n))



theme_response_subset <- data.table(theme_response, key="n")
theme_response_subset <- theme_response_subset[, tail(.SD, 5), by=position]    

theme_response_subset[1, 3] <- "Civil & political rights (disappeared people)"
theme_response_subset[3, 3] <- "Civil & political rights (general)"
theme_response_subset[6, 3] <- "Economic, social & cultural rights"

  
theme_response_subset %>%
  mutate(description = fct_reorder(description, n))  %>%
  ggplot(aes(x = description, y = n)) +
  geom_col(position = "dodge", aes(fill = position)) +
  coord_flip() +
  theme(legend.position="bottom") +
  labs(title = "Top Five Categories for Accepted (Positive) and Noted (Negative) Chinese Responses", y = "Number of Comments per Theme", x = "") +
  theme_minimal()


##-----------------------------trying a treemap instead---------------------------------

theme_response_tree <- data.table(theme_response, key="n")
theme_response_tree <- theme_response_tree[, tail(.SD, 15), by=position]    

theme_response_tree$description <- gsub("\\-.*","",theme_response_tree$description)

theme_response_tree[29,3] <- "Equality and non-Discrimination"

p <- treemap(dtf = theme_response_tree, 
        index = c("position", "description"), 
        vSize = "n", 
        type = "index",
        palette = "Set1",
        bg.labels = c("white"),
        title = c("Recall that Noted Means China Rejected the Recommendation"),
        fontsize.title = 16,
        fontsize.labels = c(20, 11),
        align.labels=list(
          c("center", "center"), 
          c("right", "bottom")
            )
          )

dev.copy(png,'static_tree_map')
dev.off()

test_d3_tree <- d3tree2(p,  rootname = "Themes by Support Status" )
test_d3_tree
##saving my html widget

saveWidget(test_d3_tree, file=paste0(getwd(), "/interactiveTreemap.html"))


