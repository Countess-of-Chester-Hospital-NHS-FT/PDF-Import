library(tidyverse)
library(janitor)
library(pdftools)
library(fuzzyjoin)
library(stringi)
library(tidytext)

theme_set(theme_bw())

################### Import 1 file and derive subheadings / sections ############
file <- "job_desc.pdf"

dat <- pdf_data(file) 

# takes the data frame for each page and combines to single data frame for whole doc
document <- tibble()
for (i in seq_along(dat)){
  page <- as_tibble(dat[[i]]) |>
    mutate(page = i)
  document <- document |>
    bind_rows(page)
}

# makes the data frame one row per line, rather than 1 row per word
document_lines <- document |>
  filter(page > 1) |>
  group_by(page, y) |>
  mutate(line = str_flatten(text, collapse = " ")) |>
  slice_head(n=1) |>
  ungroup() |>
  mutate(lead_y = lead(y),
         y_diff = lead_y - y,
         is_subheading = if_else(height > 10 & y_diff > 26, 1, 0), # logic to identify subheadings
         line_id = row_number()) |>
  select(line_id, page, y, lead_y, y_diff, height, line, is_subheading)

# extracts a table of subheadings with start and end of each section
document_headings <- document_lines |>
  filter(is_subheading == 1) |>
  mutate(end_of_section = lead(line_id),
         end_of_section = if_else(is.na(end_of_section), nrow(document_lines) + 1, end_of_section)) |>
  rename(heading = line) |>
  select(line_id, end_of_section, heading)

# labels each line with the section it is part of and combines the section into single string
document_sectioned <- document_lines |>
  fuzzy_left_join(document_headings,
                  by = c("line_id" = "line_id", "line_id" = "end_of_section"),
                  match_fun = list(`>=`, `<`)
  ) |>
  rename(line_id = line_id.x) |>
  select(line_id, line, heading) |>
  filter(!line_id %in% document_headings$line_id) |> # removes heading rows
  group_by(heading) |>
  arrange(line_id) |>
  mutate(section_text = str_flatten(line, collapse = " ")) |> # one string per section
  slice_head(n=1)|>
  ungroup() |>
  arrange(line_id) |>
  mutate(section_id = row_number()) |>
  select(section_id, everything()) |>
  select(-line, -line_id)


## Now that document is in meaningful sections, can now pre-process for analysis

expanded_stop_words <- tibble(word = c("chester", "countess", "coch", "post", "holder", "bi"),
                              lexicon = "custom") |>
  bind_rows(stop_words)

# summarises each section
document_processed <- document_sectioned |>
  mutate(section_text = str_squish(section_text), # removes leading/trailing whitespace and single whitespace all internal
         lower_text = str_to_lower(section_text),
         nchar = nchar(section_text),
         word_count = str_count(section_text, "\\S+"), # regex is for non-whitespace substrings
         sentence_count = stri_count_boundaries(section_text, type = "sentence"),
         lexical_diversity = sapply(str_split(lower_text, "\\s+"), 
                                    function(words) { length(unique(words)) / length(words) })
  )

# adds a version of the text with stop words removed
document_processed <- document_processed |>
  select(section_id, section_text) |>
  unnest_tokens(output = word, input= section_text, strip_punct=T)|>
  anti_join(expanded_stop_words) |>
  filter(!str_detect(word, "^[0-9]+([.,][0-9]+)*$")) |> #drop numbers
  group_by(section_id) |>
  mutate(nostop = str_flatten(word, collapse = " ")) |>
  slice_head(n=1) |>
  ungroup() |>
  select(-word) |>
  left_join(document_processed, by = "section_id") |>
  select(section_id, heading, section_text, lower_text, nostop, nchar, word_count,
         sentence_count, lexical_diversity)

# tokenised words from relevant sections
tokenised_words <- document_processed |>
  filter(section_id < 8) |>
  unnest_tokens(output = word, input= nostop) |>
  select(section_id, heading, word)

# tf-idf scores from relevant sections
tf_idf <- tokenised_words |>
  count(section_id, heading, word, sort=T) |>
  bind_tf_idf(word, section_id, n) |>
  arrange(section_id, desc(tf_idf))

## Simple frequency chart for each section

### Frequency bar chart for each section 
tokenised_words |>
  group_by(heading) |>
  count(word, sort = TRUE) |>
  filter(n > 1) |>
  top_n(5) |>
  ungroup() |>
  ggplot(aes(reorder(word, n), n, fill = as_factor(heading))) +
  geom_bar(stat = "identity") +
  facet_wrap(~heading, scales = "free_y") +
  labs(x = NULL, y = "Word Frequency by Heading") +
  scale_fill_discrete(name = "Section ID") +
  coord_flip() +
  theme(legend.position = "none")

### TF-IDF bar chart for each section  - highlights differences between sections
tf_idf |>
  select(-n) |>
  group_by(heading) |>
  rename(n = tf_idf) |>
  top_n(5) |>
  ungroup() |>
  ggplot(aes(reorder(word, n), n, fill = as_factor(heading))) +
  geom_bar(stat = "identity") +
  facet_wrap(~heading, scales = "free_y") +
  labs(x = NULL, y = "Word tf-idf by Heading") +
  scale_fill_discrete(name = "Section ID") +
  coord_flip() +
  theme(legend.position = "none")

### Word cloud the whole document
library(wordcloud2)

cloud_words <- tokenised_words |>
  count(word, sort=TRUE) |>
  filter(n > 1)

set.seed(1235) # word cloud the same every time

# expects a dataframe with words or ngrams in one column and counts in another
wordcloud2(
  data = cloud_words,
  shape = 'circle'
)

## TF-IDF word cloud for a single section (communication)
cloud_words <- tf_idf |>
  select(-n) |>
  filter(section_id == 2) |>
  mutate(tf_idf = round(tf_idf*10000)) |>
  rename(n = tf_idf) |>
  filter(n > 1)

wordcloud2(
  data = cloud_words |> select(word, freq = n),
  shape = 'circle'
)
