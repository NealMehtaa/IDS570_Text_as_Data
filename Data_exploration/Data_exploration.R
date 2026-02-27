if (basename(getwd()) != "Data_exploration") {
  if (dir.exists("Data_exploration")) setwd("Data_exploration")
}

output_dir <- "Data_exploration_ouput"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

library(dplyr)
library(tidyr)
library(tidytext)
library(tibble)
library(readr)
library(tidyverse)
library(purrr)
library(quanteda)
library(quanteda.textstats)
library(stringr)
library(ggplot2)
library(forcats)
library(udpipe)

#Step 0: Load texts and normalization

text_dir <- "Text_Files"
all_files <- list.files(text_dir, pattern = "\\.txt$", full.names = TRUE)
doc_ids <- tools::file_path_sans_ext(basename(all_files))
raw_texts <- setNames(
  sapply(all_files, function(f) paste(readLines(f, warn = FALSE), collapse = " ")),
  doc_ids
)

cat("Loaded", length(raw_texts), "documents.\n")

raw_texts <- str_replace_all(raw_texts, "\u017f", "s")
names(raw_texts) <- doc_ids

raw_texts <- str_replace_all(raw_texts, "[●▪◊]", "")
names(raw_texts) <- doc_ids

cat("Word counts after normalization:\n")
print(sapply(str_split(raw_texts, "\\s+"), length))

em_stopwords <- c(
  "art", "bee", "canst", "didst", "doe", "doth", "hadst", "hast", 
  "hath", "haue", "hee", "hence", "herein", "hereof", "saith", 
  "shalt", "shouldst", "thee", "thence", "therein", "thereof", 
  "thereto", "thine", "thou", "thy", "vnto", "vpon", "wee", 
  "whereby", "wherein", "whereof", "whereto", "wilt", "wouldst", 
  "ye", "yt"
)

artefacts <- c("ll", "ss", "arg", "answ", "viz", "ibid", "idem", "pag", "dly", "ndly", "rdly", "thly")

corp <- corpus(
  data.frame(
    doc_id = names(raw_texts),
    text   = unname(raw_texts),
    stringsAsFactors = FALSE
  ),
  docid_field = "doc_id",
  text_field  = "text"
)

toks <- tokens(
  corp,
  remove_punct   = TRUE,
  remove_numbers = TRUE,
  remove_symbols = TRUE
) %>%
  tokens_tolower() %>%
  tokens_remove(c(stopwords("en"), em_stopwords, artefacts)) %>%
  tokens_remove(
    pattern = c("^.{1,3}$", "^[0-9]", "[<>]"),
    valuetype = "regex"
  )

dfm_counts <- dfm(toks)
cat("\nDFM dimensions:", dim(dfm_counts), "(docs x features)\n")

dfm_tfidf <- dfm_tfidf(dfm_counts)
n_top <- 15L

tfidf_by_doc <- convert(dfm_tfidf, to = "data.frame") %>%
  pivot_longer(-doc_id, names_to = "term", values_to = "tfidf") %>%
  filter(tfidf > 0) %>%
  group_by(doc_id) %>%
  slice_max(order_by = tfidf, n = n_top) %>%
  arrange(doc_id, desc(tfidf)) %>%
  ungroup()

cat("\n--- Top", n_top, "TF-IDF terms per document ---\n")
print(tfidf_by_doc, n = Inf)

tfidf_summary_table <- tfidf_by_doc %>%
  mutate(tfidf = round(tfidf, 4)) %>%
  group_by(doc_id) %>%
  summarise(top_terms = paste(term, collapse = ", "), .groups = "drop")
cat("\n--- TF-IDF summary (one row per document) ---\n")
print(tfidf_summary_table, n = Inf)

top_terms <- tfidf_by_doc %>%
  group_by(doc_id) %>%
  slice_max(tfidf, n = n_top, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, tfidf, doc_id))

p_lollipop <- ggplot(top_terms, aes(x = tfidf, y = term)) +
  geom_segment(aes(x = 0, xend = tfidf, yend = term), linewidth = 0.6, alpha = 0.6) +
  geom_point(size = 1.8) +
  facet_wrap(~ doc_id, scales = "free", ncol = 4) +
  scale_y_reordered() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "Top TF–IDF Terms by Document",
    subtitle = sprintf("Top %d terms per document", n_top),
    x = "TF–IDF weight",
    y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 9),
    plot.title = element_text(face = "bold")
  )

p_lollipop
n_docs <- n_distinct(tfidf_by_doc$doc_id)
ggsave(
  file.path(output_dir, "tfidf_lollipop.png"),
  plot = p_lollipop,
  width = 14, height = ceiling(n_docs / 4) * 2.8, dpi = 150, limitsize = FALSE
)
cat("Saved tfidf_lollipop.png to", output_dir, "\n")

min_termfreq <- 7L
dfm_trimmed <- dfm_trim(dfm_counts, min_termfreq = min_termfreq)
cat("\nTrimmed DFM (min_termfreq =", min_termfreq, "):", dim(dfm_trimmed), "\n")

sim_r <- textstat_simil(dfm_trimmed, margin = "documents", method = "correlation")
r_mat <- round(as.matrix(sim_r), 3)

cat("\n--- Pearson correlation matrix ---\n")
print(r_mat[1:5, 1:5])

r_pairs <- as.data.frame(as.table(r_mat)) %>%
  rename(doc_i = Var1, doc_j = Var2, r = Freq) %>%
  mutate(
    doc_i = as.character(doc_i),
    doc_j = as.character(doc_j)
  ) %>%
  filter(doc_i < doc_j)

cat("\n--- Two most similar document pairs ---\n")
print(slice_max(r_pairs, order_by = r, n = 2))
cat("\n--- Two least similar document pairs ---\n")
print(slice_min(r_pairs, order_by = r, n = 2))

max_label <- 14L
short_labels <- str_trunc(rownames(r_mat), max_label, ellipsis = "")
r_plot <- r_mat
rownames(r_plot) <- short_labels
colnames(r_plot) <- short_labels

heat_df <- as.data.frame(r_plot) %>%
  rownames_to_column("doc_i") %>%
  pivot_longer(-doc_i, names_to = "doc_j", values_to = "r")
heat_df$doc_i <- factor(heat_df$doc_i, levels = short_labels)
heat_df$doc_j <- factor(heat_df$doc_j, levels = short_labels)

p_heat <- ggplot(heat_df, aes(x = doc_j, y = doc_i, fill = r)) +
  geom_tile(color = "white", linewidth = 0.3) +
  coord_fixed() +
  scale_fill_gradient2(
    low = "steelblue", mid = "white", high = "firebrick",
    midpoint = 0, limits = c(-1, 1), name = "Pearson r"
  ) +
  labs(
    title = "Pairwise Pearson Correlation Between Documents",
    subtitle = paste("DFM trimmed to min_termfreq =", min_termfreq),
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold")
  )
ggsave(
  file.path(output_dir, "pearson_heatmap.png"),
  plot = p_heat, width = 8, height = 7, dpi = 150
)
cat("Saved pearson_heatmap.png to", output_dir, "\n")

top_pair <- slice_max(r_pairs, order_by = r, n = 1)
bot_pair <- slice_min(r_pairs, order_by = r, n = 1)
if (nrow(top_pair) == 0 || nrow(bot_pair) == 0) {
  doc_names <- rownames(r_mat)
  doc_a <- doc_names[1]
  doc_b <- doc_names[2]
} else {
  doc_a <- top_pair$doc_i[1]
  doc_b <- bot_pair$doc_i[1]
  if (doc_a == doc_b) doc_b <- bot_pair$doc_j[1]
}
syn_docs <- c(doc_a, doc_b)
text_a <- unname(raw_texts[names(raw_texts) == syn_docs[1]])[1]
text_b <- unname(raw_texts[names(raw_texts) == syn_docs[2]])[1]
syn_df_input <- tibble(doc_id = syn_docs, text = c(text_a, text_b))
cat("\n--- Syntactic complexity: comparing", syn_docs[1], "and", syn_docs[2], "---\n")

ud_model_file <- udpipe_download_model(language = "english-ewt", model_dir = Sys.getenv("UD_MODEL_DIR", "."))
ud_model <- udpipe_load_model(ud_model_file$file_model)

anno_list <- syn_df_input %>%
  mutate(anno = map2(text, doc_id, ~ udpipe_annotate(ud_model, x = .x, doc_id = .y) %>% as.data.frame())) %>%
  pull(anno)
anno <- bind_rows(anno_list) %>%
  select(doc_id, sentence_id, token_id, token, lemma, upos, feats, head_token_id, dep_rel)

anno <- anno %>%
  mutate(
    is_word = upos != "PUNCT",
    is_clause = (upos %in% c("VERB", "AUX")) & str_detect(coalesce(feats, ""), "VerbForm=Fin"),
    is_dep_clause = dep_rel %in% c("advcl", "ccomp", "xcomp", "acl", "acl:relcl"),
    is_coord = dep_rel %in% c("conj", "cc"),
    is_complex_nominal = dep_rel %in% c("amod", "nmod", "compound", "appos")
  )


sent_stats <- anno %>%
  filter(is_word) %>%
  group_by(doc_id, sentence_id) %>%
  summarise(
    words = n(),
    clauses = sum(is_clause),
    dep_clauses = sum(is_dep_clause),
    .groups = "drop"
  )

# MLS
mls_tbl <- sent_stats %>%
  group_by(doc_id) %>%
  summarise(MLS = mean(words), .groups = "drop")

# C/S
clausal_tbl <- sent_stats %>%
  group_by(doc_id) %>%
  summarise(
    sentences = n(),
    clauses = sum(clauses),
    C_per_S = clauses / sentences,
    .groups = "drop"
  )

# DC/C, DC/S
subord_tbl <- sent_stats %>%
  group_by(doc_id) %>%
  summarise(
    clauses = sum(clauses),
    dep_clauses = sum(dep_clauses),
    sentences = n(),
    DC_per_C = dep_clauses / pmax(clauses, 1),
    DC_per_S = dep_clauses / sentences,
    .groups = "drop"
  )


coord_tbl <- anno %>%
  group_by(doc_id) %>%
  summarise(
    coord_relations = sum(is_coord),
    clauses = sum(is_clause),
    sentences = n_distinct(sentence_id),
    Coord_per_C = coord_relations / pmax(clauses, 1),
    Coord_per_S = coord_relations / sentences,
    .groups = "drop"
  )

nominal_tbl <- anno %>%
  group_by(doc_id) %>%
  summarise(
    complex_nominals = sum(is_complex_nominal),
    clauses = sum(is_clause),
    sentences = n_distinct(sentence_id),
    CN_per_C = complex_nominals / pmax(clauses, 1),
    CN_per_S = complex_nominals / sentences,
    .groups = "drop"
  )

syntax_summary <- mls_tbl %>%
  left_join(clausal_tbl %>% select(doc_id, C_per_S), by = "doc_id") %>%
  left_join(subord_tbl %>% select(doc_id, DC_per_C, DC_per_S), by = "doc_id") %>%
  left_join(coord_tbl %>% select(doc_id, Coord_per_C, Coord_per_S), by = "doc_id") %>%
  left_join(nominal_tbl %>% select(doc_id, CN_per_C, CN_per_S), by = "doc_id") %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

cat("\n--- Syntactic complexity summary ---\n")
print(syntax_summary)


syntax_long <- syntax_summary %>%
  pivot_longer(-doc_id, names_to = "Measure", values_to = "Value") %>%
  mutate(
    Category = case_when(
      Measure == "MLS" ~ "Sentence length",
      Measure == "C_per_S" ~ "Clausal density",
      Measure %in% c("DC_per_C", "DC_per_S") ~ "Subordination",
      Measure %in% c("Coord_per_C", "Coord_per_S") ~ "Coordination",
      Measure %in% c("CN_per_C", "CN_per_S") ~ "Phrasal complexity",
      TRUE ~ "Other"
    )
  )

p_syntax <- ggplot(syntax_long, aes(x = Measure, y = Value, fill = doc_id)) +
  geom_col(position = "dodge", width = 0.7) +
  facet_wrap(~ Category, scales = "free", ncol = 2) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Syntactic Complexity",
    x = NULL, y = "Value", fill = "Document"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")
ggsave(
  file.path(output_dir, "syntax_complexity.png"),
  plot = p_syntax, width = 10, height = 6, dpi = 150
)
cat("Saved syntax_complexity.png to", output_dir, "\n")
sent_with_dc <- sent_stats %>%
  group_by(doc_id) %>%
  slice_max(order_by = dep_clauses, n = 2) %>%
  ungroup()

get_sentence_text <- function(ann, doc, sent_id) {
  ann %>% filter(doc_id == doc, sentence_id == sent_id) %>%
    arrange(as.integer(token_id)) %>%
    pull(token) %>% paste(collapse = " ")
}
ex_sentences <- sent_with_dc %>%
  rowwise() %>%
  mutate(
    sentence_text = get_sentence_text(anno, doc_id, sentence_id)
  ) %>%
  ungroup() %>%
  select(doc_id, sentence_id, words, clauses, dep_clauses, sentence_text)
cat("\n--- Example sentences (high subordination) ---\n")
print(ex_sentences, n = Inf)


write_csv(tfidf_summary_table, file.path(output_dir, "tfidf_summary_table.csv"))
write_csv(syntax_summary, file.path(output_dir, "syntax_summary_table.csv"))
write_csv(
  r_pairs %>% arrange(desc(r)),
  file.path(output_dir, "pearson_pairs_ranked.csv")
)
cat(
  "Saved tfidf_summary_table.csv, syntax_summary_table.csv, pearson_pairs_ranked.csv to",
  output_dir, "\n"
)

cat("\n--- Key outputs for report ---\n")
cat("TF-IDF: tfidf_summary_table.csv, tfidf_lollipop.png (in ", output_dir, ")\n", sep = "")
cat("Pearson: pearson_heatmap.png, pearson_pairs_ranked.csv (in ", output_dir, ")\n", sep = "")
cat("Syntactic: syntax_summary_table.csv, syntax_complexity.png (in ", output_dir, "); example sentences above.\n", sep = "")
