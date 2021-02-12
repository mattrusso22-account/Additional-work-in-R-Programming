library(tidyverse)
library(tidytext)
library(igraph)
library(ggraph)
library(quanteda)
library(readr)
library(readr)
ctweets <- read_delim("C:/Users/Mvpie/Downloads/ctweets.csv", 
                      ";", escape_double = FALSE, trim_ws = TRUE)
View(ctweets)

count_trigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
    separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
   filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
          !word3 %in% stop_words$word) %>%
    count(word1, word2, word3, sort = TRUE)
}

count_trigrams(ctweets) 
ctweet_tri <- count_trigrams(ctweets) 
View(ctweet_tri)

ctweet_thirty <- count_trigrams(ctweets) %>%
  top_n(30) 
  
view(ctweet_thirty)

count_trigrams_2 <- function(dataset) {
  dataset %>%
    unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
    separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
    #filter(!word1 %in% stop_words$word,
     #      !word2 %in% stop_words$word,
      #     !word3 %in% stop_words$word) %>%
    count(word1, word2, word3, sort = TRUE)
}

count_trigrams_2(ctweets)
ctweet_tri_2 <- count_trigrams_2(ctweets)

ctweet_tri_2 %>%
  filter(word1 == "greatest") %>%
  count(word1, word2, word3, sort = TRUE) %>%
  top_n(10)
