
library(tidyverse)
library(tidytext)
library(docxtractr)

setwd ("~/Desktop/Desktop - MacBook Pro/Data and Programming II/final project/Fortunato-Frisbie-final-project")
#------------------------------------------------------------------------------------------
##SENTIMENT ANALYSIS GRAPHS

filenames <- c("Matrix_recommendations_2013.txt", "Matrix_recommendations_2018.txt")

sentiment_list <- list()

for (i in filenames) {
  s <- read_file(i)
  text_df <- tibble(text = s)
  word_tokens_df <- unnest_tokens(text_df, word_tokens,  text, token = "words")
  df <- anti_join(word_tokens_df, stop_words, by = c("word_tokens" = "word"))
  
  for (s in c("nrc", "afinn", "bing")) {
    df <- df %>% 
      left_join(get_sentiments(s), by = c("word_tokens" = "word")) %>%
      plyr::rename(replace = c(sentiment = s, value = s), warn_missing = FALSE)
  }
  sentiment_list <- append(sentiment_list, list(df))
}

names(sentiment_list) <- c("Comments About China in 2013", "Comments About China in 2018") # Define names of df's

##using sentiment list in the function in order to allow for automatic title changing 
plot <- function(df){
    ggplot(data = sentiment_list[[df]] %>% filter(!is.na(bing))) +
    geom_histogram(aes(bing), stat = "count") +
    scale_x_discrete(guide= guide_axis(angle = 45)) +
    labs(title = df)
}

lapply(names(sentiment_list), plot)

#--------------------------------------------------------------------------------------------

recommendations_2013 <- read_docx("Matrix_recommendations_2013.docx")
recommendations_2013 <- docx_extract_all_tbls(recommendations_2013)

recommendations_2018 <- read_docx("Matrix_recommendations_2018.docx")
recommendations_2018 <- docx_extract_all_tbls(recommendations_2018)




