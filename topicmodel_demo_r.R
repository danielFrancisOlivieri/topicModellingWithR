# CITATION: https://www.tidytextmining.com/topicmodeling.html

# INSTALL and LOAD required libraries
install.packages('gutenbergr')
library(gutenbergr)
install.packages('stringr')
library(stringr)
install.packages('ggplot2')
library(ggplot2)
install.packages('dplyr')
library(dplyr)
install.packages('tidytext')
library(tidytext)
install.packages('tidyr')
library(tidyr)
install.packages('topicmodels')
library(topicmodels)


# IDENTIFY novel titles to download 
titles <- c("Twenty Thousand Leagues under the Sea", "The War of the Worlds",
            "Pride and Prejudice", "Great Expectations")

# DOWNLOAD novels from Gutenberg
books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")

# ORGANIZE TEXT: divide into documents, each representing one chapter
by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

# split into words
by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)

# find document-word counts
word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()

word_counts

# RUN LDA on chapters
chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n)

chapters_dtm

# create a 4 topic LDA model
chapters_lda <- LDA(chapters_dtm, k = 4, control = list(seed = 1234))
chapters_lda

# look at the per-topic-per-word probabilities
chapter_topics <- tidy(chapters_lda, matrix = "beta")
chapter_topics

# look at the top n terms within each topic
top_terms <- chapter_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

# visualize our top 5 terms within each topic
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


