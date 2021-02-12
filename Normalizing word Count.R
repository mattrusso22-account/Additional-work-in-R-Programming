#New variable to get word count 

christ_source_words <- LastChristmasReviews_1_ %>%
  unnest_tokens(word,text) %>%
  group_by(source) %>%
  mutate(word_cnt = n())

#Add new variable to add to previous script 
#New column shows Sentiment difference/by the total number of words in the review

christ_source_sent_bing_2 <- LastChristmasReviews_1_ %>%
  unnest_tokens(word,text) %>%
  group_by(source) %>%
  inner_join(get_sentiments("bing")) %>%
  count(source,metacritic,sentiment) %>%
  spread(sentiment,n,fill=0) %>%
  mutate(diff = positive - negative) %>%
  inner_join(christ_source_words) %>%
  mutate(diff/word_cnt)


#Calculates the correlation between the metacritic score and the new variable

cor(christ_source_sent_bing_2[,c("metacritic", "diff/word_cnt")],use="complete.obs")

  