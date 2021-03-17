
library(tidyverse)
library(tidytext)
library(docxtractr)
library(textdata)
library(gridExtra)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(rvest)
library(purrr)
library(readr)
library(reshape2)
library(rlist)
library(varhandle)
library(udpipe)


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

  df$word_tokens <- removeNumbers(df$word_tokens)
  df$word_tokens <-  removePunctuation(df$word_tokens)
  df <- df %>% filter(word_tokens != "")

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
    geom_histogram(aes(bing, fill = bing), stat = "count") +
    scale_x_discrete(guide= guide_axis(angle = 45)) +
    labs(title = df) +
    theme(legend.title = element_blank(),
          axis.title.x = element_blank()) +
    theme(plot.title = element_text(size = 20, face = "bold"),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"))
}

lapply(names(sentiment_list), plot)

dev.copy(png,'sentiment_2013.png')
dev.off()
dev.copy(png,'sentiment_2018.png')
dev.off()

##-----------------------------frequency, lemmas, and wordcloud-----------------------------

freq_list <- list()

for (i in names(sentiment_list)) {
freq_list[[i]] <- count(sentiment_list[[i]], word_tokens, sort = TRUE)
}

freq_list[[2]]

##simple bag-of-words approach with lemmas for both documents combined

lemma_list <- list()

for (i in filenames){
s <- read_file(i)
text_df <- tibble(text = s)
df_udp <- udpipe(text_df$text, "english")

test_lemmas <- df_udp %>%
  filter(upos != "PUNCT") %>%
  anti_join(stop_words, by = c("lemma" = "word"))

test_lemmas$lemma <- removeNumbers(test_lemmas$lemma)
test_lemmas$lemma <- removePunctuation(test_lemmas$lemma)
test_lemma <- test_lemmas %>% select(token, lemma)

lemma_list <- append(lemma_list, list(test_lemma))

}

lemma_list <- list.stack(lemma_list)


lemma_count <- print(lemma_list %>%
                       group_by(lemma) %>%
                       filter(lemma != "" & lemma != "Ahrcadd" & lemma != "Sdg") %>%
                       filter(!grepl("(^| ).( |$)", lemma)) %>%
                       summarise(n = n()) %>%
                       arrange(desc(n)))

wordcloud(words = lemma_count$lemma, freq = lemma_count$n, min.freq = 10,
          max.words=500, random.order=FALSE, rot.per=0.35, scale=c(2,0.25),
          colors=brewer.pal(8, "Dark2"))

dev.copy(png,'wordcloud.png')
dev.off()

### ------------------------ generalizing data cleaning ---------------------------------


filenames_docs <- c("Matrix_recommendations_2013.docx", "Matrix_recommendations_2018.docx")
text_cleaning_list <- list()

for (i in filenames_docs){
  s <- read_docx(i)
  s <- docx_extract_all_tbls(s)
  s <- as_tibble(s[[1]])
  s$Assessment.comments.on.level.of.implementation <- NULL

  ##removing all rows which start with "theme"

  s <- s %>%
    filter(!str_detect(Recommendation, 'Theme:'))

  ##splitting off first codes
  y <-  colsplit(s$Recommendation," ",c("code","recommendations"))

  ##binding back together
  s <- cbind(s, y)

  ##dropping original column
  s$Recommendation <- NULL

  ##use regex to remove ending redundant information from recommendations
  ##Source: https://r4ds.had.co.nz/strings.html
  s$recommendations <- str_extract(s$recommendations, "(.*);")

  ##separating country comments
  s <- separate(s, col = recommendations, into = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k"), sep = ";", remove = FALSE)

  ##replacing empty strings with NA
  s[s==""]<-NA
  s$recommendations <- NULL

  s <- s %>% pivot_longer(cols = a:k, values_to = "recommendations", values_drop_na = TRUE)

  s$name <- NULL

  ##removing ()
  s$country <- str_extract(s$recommendations, "(?<=\\().+?(?=\\))")
  s$recommendations <- str_remove(s$recommendations, "\\([^()]*\\)")

  ##cleaning theme column and separating
  s <- separate(s, col = Full.list.of.themes, into = c("a", "b", "c", "d", "e", "f", "g", "h"), sep = "(?<=.)(?=[a-zA-Z]\\d{1,2})", remove = TRUE)
  s <- s %>% pivot_longer(cols = a:h, values_to = "themes", values_drop_na = TRUE)

  s[s==""]<-NA
  s$name <- NULL

  ##saving a list of themes
  theme_list <- unique(s$themes)
  theme_list <- as_tibble(theme_list)

  s <- separate(s, col = themes, into = c("theme_code", "description"), sep = "\\s|\\-", extra = "merge", remove = TRUE)

  names(s) <- tolower(names(s))

  text_cleaning_list <- append(text_cleaning_list, list(s))
}

names(text_cleaning_list) <- filenames_docs  # Define names of df's

write_csv(text_cleaning_list[[1]], "text_cleaning_list_2013.csv")
write_csv(text_cleaning_list[[2]], "text_cleaning_list_2018.csv")

### ------------------------ generalizing data cleaning ---------------------------------


filenames_docs <- c("Matrix_recommendations_2013.docx", "Matrix_recommendations_2018.docx")
text_cleaning_list <- list()

for (i in filenames_docs){
  s <- read_docx(i)
  s <- docx_extract_all_tbls(s)
  s <- as_tibble(s[[1]])
  s$Assessment.comments.on.level.of.implementation <- NULL

  ##removing all rows which start with "theme"

  s <- s %>%
    filter(!str_detect(Recommendation, 'Theme:'))

  ##splitting off first codes
  y <-  colsplit(s$Recommendation," ",c("code","recommendations"))

  ##binding back together
  s <- cbind(s, y)

  ##dropping original column
  s$Recommendation <- NULL

  ##use regex to remove ending redundant information from recommendations
  ##Source: https://r4ds.had.co.nz/strings.html
  s$recommendations <- str_extract(s$recommendations, "(.*);")

  ##separating country comments
  s <- separate(s, col = recommendations, into = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k"), sep = ";", remove = FALSE)

  ##replacing empty strings with NA
  s[s==""]<-NA
  s$recommendations <- NULL

  s <- s %>% pivot_longer(cols = a:k, values_to = "recommendations", values_drop_na = TRUE)

  s$name <- NULL

  ##removing ()
  s$country <- str_extract(s$recommendations, "(?<=\\().+?(?=\\))")
  s$recommendations <- str_remove(s$recommendations, "\\([^()]*\\)")

  ##cleaning theme column and separating
  s <- separate(s, col = Full.list.of.themes, into = c("a", "b", "c", "d", "e", "f", "g", "h"), sep = "(?<=.)(?=[a-zA-Z]\\d{2})", remove = TRUE)
  s <- s %>% pivot_longer(cols = a:h, values_to = "themes", values_drop_na = TRUE)



  s[s==""]<-NA
  s$name <- NULL

  text_cleaning_list <- append(text_cleaning_list, list(s))
}

names(text_cleaning_list) <- filenames_docs  # Define names of df's
