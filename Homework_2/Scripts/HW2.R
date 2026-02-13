library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(forcats)
library(tibble)
library(scales)

file_a <- "texts/A07594__Circle_of_Commerce.txt"
file_b <- "texts/B14801__Free_Trade.txt"

text_a <- read_file(file_a)
text_b <- read_file(file_b)

texts <- tibble(
  doc_title = c("Text A", "Text B"),
  text = c(text_a, text_b)
)
texts

data("stop_words")

custom_stopwords <- tibble(
  word = c(
    "vnto", "haue", "doo", "hath", "bee", "ye", "thee","ſo","hee"
  )
)

all_stopwords <- bind_rows(stop_words, custom_stopwords) %>%
  distinct(word)

word_counts <- texts %>%
  unnest_tokens(word, text) %>% 
  mutate(word = str_to_lower(word)) %>%
  anti_join(all_stopwords, by = "word") %>%
  count(doc_title, word, sort = TRUE)
word_counts

doc_lenghts <- word_counts %>%
  group_by(doc_title) %>%
  summarise(total_words = sum(n))
doc_lenghts

bing_lexicon <- get_sentiments("bing")

sentiment_words <- word_counts %>%
  inner_join(bing_lexicon, by = "word")
sentiment_words

sentiment_totals <- sentiment_words %>%
  group_by(doc_title) %>%
  summarise(
    positive = sum(n[sentiment == "positive"], na.rm = TRUE),
    negative = sum(n[sentiment == "negative"], na.rm = TRUE),
    net_sentiment = positive - negative
  )
sentiment_totals

word_tfidf <- word_counts %>%
  bind_tf_idf(word, doc_title, n)
word_tfidf

tfidf_sentiment <- word_tfidf %>%
  inner_join(bing_lexicon, by = "word")
tfidf_sentiment

tfidf_sentiment_totals <- tfidf_sentiment %>%
  group_by(doc_title) %>%
  summarise(
    positive_tfidf = sum(tf_idf[sentiment == "positive"], na.rm = TRUE),
    negative_tfidf = sum(tf_idf[sentiment == "negative"], na.rm = TRUE),
    net_tfidf_sentiment = positive_tfidf - negative_tfidf
  )
tfidf_sentiment_totals

final_comparison <- sentiment_totals %>%
  left_join(tfidf_sentiment_totals, by = "doc_title")
final_comparison

write_csv(final_comparison, "sentiment_comparison.csv")

top_drivers <- tfidf_sentiment %>%
  mutate(
    sentiment_contribution = ifelse(sentiment == "positive", tf_idf, -tf_idf)
  ) %>%
  group_by(doc_title) %>%
  slice_max(abs(sentiment_contribution), n = 10) %>%
  ungroup() %>%
  arrange(doc_title, desc(abs(sentiment_contribution)))

print("Top 10 sentiment-driving words by TF-IDF for each document:")
print(top_drivers %>% select(doc_title, word, n, tf_idf, sentiment, sentiment_contribution))