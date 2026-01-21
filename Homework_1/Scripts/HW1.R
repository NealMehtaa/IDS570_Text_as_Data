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

# Read the raw text files into R
text_a <- read_file(file_a)
text_b <- read_file(file_b)

# Combine into a tibble for tidytext workflows
texts <- tibble(
  doc_title = c("Text A", "Text B"),
  text = c(text_a, text_b)
)

texts

tokens_pre_stop <- texts %>%
  unnest_tokens(word, text) %>%
  mutate(word = str_to_lower(word))

corpus_diagnostics <- texts %>%
  transmute(
    doc_title,
    n_chars = nchar(text)
  ) %>%
  left_join(
    tokens_pre_stop %>%
      group_by(doc_title) %>%
      summarise(
        n_word_tokens = n(),
        n_word_types = n_distinct(word),
        .groups = "drop"
      ),
    by = "doc_title"
  )

corpus_diagnostics

# Start with tidytext's built-in stopword list
data("stop_words")

# Add our own project-specific stopwords (you can, and will, expand this list later)
custom_stopwords <- tibble(
  word = c(
    "vnto", "haue", "doo", "hath", "bee", "ye", "thee"
  )
)

all_stopwords <- bind_rows(stop_words, custom_stopwords) %>%
  distinct(word)

all_stopwords %>% slice(1:10)
word_counts <- texts %>%
  unnest_tokens(word, text) %>%
  mutate(word = str_to_lower(word)) %>%
  anti_join(all_stopwords, by = "word") %>%
  count(doc_title, word, sort = TRUE)

word_counts
plot_n_words <- 20  # you can change this as needed

# Select the most frequent words overall
word_comparison_tbl <- word_counts %>%
  pivot_wider(
    names_from = doc_title,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(max_n = pmax(`Text A`, `Text B`)) %>%
  arrange(desc(max_n))

doc_lengths <- word_counts %>%
  group_by(doc_title) %>%
  summarise(total_words = sum(n), .groups = "drop")

word_counts_normalized <- word_counts %>%
  left_join(doc_lengths, by = "doc_title") %>%
  mutate(relative_freq = n / total_words)

trade_compare <- word_counts_normalized %>%
  filter(word == "trade") %>%
  select(doc_title, n, total_words, relative_freq)

top_words <- word_comparison_tbl %>%
  slice_head(n = plot_n_words) %>%
  pull(word)

word_plot_data <- word_counts_normalized %>%
  filter(word %in% top_words) %>%
  select(doc_title, word, relative_freq) %>%
  pivot_wider(
    names_from = doc_title,
    values_from = relative_freq,
    values_fill = 0
  ) %>%
  pivot_longer(
    cols = c(`Text A`, `Text B`),
    names_to = "doc_title",
    values_to = "relative_freq"
  ) %>%
  mutate(word = fct_reorder(word, relative_freq, .fun = max))

ggplot(word_plot_data, aes(x = relative_freq, y = word)) + #black magic happens thanks to ggplot
  geom_col() +
  facet_wrap(~ doc_title, scales = "free_x") +
  scale_x_continuous(
    limits = c(0, 0.03),
    breaks = seq(0, 0.03, by = 0.005),
    labels = scales::number_format(accuracy = 0.01)
  ) +
  labs(
    title = "Most frequent words (normalized; stopwords removed)",
    subtitle = paste0(
      "Top ", plot_n_words,
      " words by maximum frequency across both texts"
    ),
    x = "Relative frequency",
    y = NULL
  ) +
  theme_minimal()
bigrams <- texts %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams
bigrams_separated <- bigrams %>%
  separate(bigram, into = c("word1", "word2"), sep = " ")

bigrams_separated
bigrams_filtered <- bigrams_separated %>%
  filter(
    !word1 %in% all_stopwords$word,
    !word2 %in% all_stopwords$word
  )

bigrams_filtered
bigram_counts <- bigrams_filtered %>%
  count(doc_title, word1, word2, sort = TRUE)

bigram_counts
bigram_counts <- bigram_counts %>%
  unite(bigram, word1, word2, sep = " ")

bigram_counts
bigram_relative <- bigram_counts %>%
  group_by(doc_title) %>%
  mutate(
    total_bigrams = sum(n),
    proportion = n / total_bigrams
  ) %>%
  ungroup()

bigram_wide <- bigram_relative %>%
  select(doc_title, bigram, proportion) %>%
  pivot_wider(
    names_from = doc_title,
    values_from = proportion,
    values_fill = 0
  )

bigram_wide
bigram_diff <- bigram_wide %>%
  mutate(
    diff = `Text A` - `Text B`
  ) %>%
  arrange(desc(abs(diff)))

bigram_diff %>% slice(1:20)
