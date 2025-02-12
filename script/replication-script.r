# ============================================================================== #
# File-Name: debate_sentiment_analysis.R
# Authors: Irene Chen, Tian Tong
# Course: PPOL-6801 Text-As-Data
# Topics: Replication of "Measuring Emotion in Parliamentary Debates with Automated Textual Analysis"
# Paper Authors: Rheault et al. (2016)
# ============================================================================== #

# About the Paper:
# This paper analyzes sentiment in UK parliamentary debates (1909-2013) to study how
# economic conditions influence political discourse. The authors developed a custom
# lexicon for parliamentary language and found that negative economic indicators
# (unemployment, inflation) correlate with more negative parliamentary discourse.

# This Replication:
# - Focuses on 1973-1977 period (marked by recession and high social unrest)
# - Uses authors' pre-computed lexicon rather than building from word vectors
# - Compares results with general-purpose AFINN lexicon
# - Analyzes relationship between sentiment and economic indicators

# Required Data:
# 1. uk_hansard_1973_1977.csv: Raw debate text
# 2. emotion-final-y.csv: Yearly aggregated data
# 3. emotion-final-q.csv: Quarterly aggregated data
# 4. lexicon-polarity.csv: Authors' domain-specific lexicon

# ============================================================================== #
#  1.0 GETTING STARTED
# ============================================================================== #

# Clear workspace
rm(list=ls())

# Load required libraries
library(tidyverse)
library(lubridate)
library(stringr)
library(scales)
library(tidytext)
library(ggplot2)
library(gridExtra)
library(quanteda)
library(quanteda.sentiment)

# ============================================================================== #
#  2.0 DATA LOADING
# ============================================================================== #

# Read the main datasets
hansard <- read.csv("uk_hansard_1973_1977.csv", 
                   sep = "\t",
                   quote = "\"",
                   stringsAsFactors = FALSE,
                   encoding = "UTF-8")

# Read preprocessed data from authors
yearly_data <- read.csv("emotion-final-y.csv")
quarterly_data <- read.csv("emotion-final-q.csv")

# Filter to study period
yearly_subset <- yearly_data %>%
  filter(year >= 1973 & year <= 1977)
quarterly_subset <- quarterly_data %>%
  filter(year >= 1973 & year <= 1977)

# ============================================================================== #
#  3.0 TEXT PREPROCESSING
# ============================================================================== #

# Define preprocessing function
preprocess_speech <- function(text) {
    text <- str_replace_all(text, "\\n", " ")
    text <- str_squish(text)
    return(text)
}

# Clean dataset
hansard_clean <- hansard %>%
    select(-person_id) %>%
    filter(!is.na(text)) %>%
    mutate(
        text_processed = preprocess_speech(text),
        date = str_extract(speech_id, "[0-9]{4}-[0-9]{2}-[0-9]{2}"),
        year = str_extract(speech_id, "[0-9]{4}")
    )

# ============================================================================== #
#  4.0 SENTIMENT ANALYSIS 
# ============================================================================== #
# Read authors' lexicon
lexicon <- read.csv("lexicon-polarity.csv", stringsAsFactors = FALSE)

# Define polarity computation function
compute_polarity <- function(text, lexicon) {
    words <- unlist(strsplit(tolower(text), "\\s+"))
    matches <- match(words, lexicon$lemma)
    matched_scores <- lexicon$polarity[matches[!is.na(matches)]]
    if(length(matched_scores) > 0) {
        return(mean(matched_scores, na.rm = TRUE))
    } else {
        return(NA)
    }
}

# Calculate polarity for each speech
polarities <- vector("numeric", nrow(hansard_clean))
for(i in 1:nrow(hansard_clean)) {
    polarities[i] <- compute_polarity(hansard_clean$text_processed[i], lexicon)
}
hansard_clean$polarity <- polarities

# ============================================================================== #
#  5.0 TEMPORAL AGGREGATION 
# ============================================================================== #
# Define rescaling function
rescale <- function(x) {2/(max(x) - min(x))*(x - min(x)) - 1}

# Aggregate by quarter
our_quarterly <- hansard_clean %>%
    mutate(
        quarter = ceiling(as.numeric(substr(date, 6, 7))/3),
        year_quarter = paste0(year, "-Q", quarter)
    ) %>%
    group_by(year_quarter) %>%
    summarize(
        avg_polarity = mean(polarity, na.rm = TRUE),
        n_speeches = n(),
        sd_polarity = sd(polarity, na.rm = TRUE),
        .groups = 'drop'
    ) %>%
    arrange(year_quarter) %>%
    mutate(
        polarity_diff = c(NA, diff(avg_polarity)),
        polarity_rescaled = rescale(avg_polarity)
    )

# ============================================================================== #
#  6.0 ALTERNATIVE LEXICONS 
# ============================================================================== #

# 6.1 AFINN Lexicon ----------------- #

afinn_sentiment <- get_sentiments("afinn")

compute_afinn_sentiment <- function(text) {
    words <- tibble(word = unlist(strsplit(tolower(text), "\\s+")))
    matched_words <- words %>%
        inner_join(afinn_sentiment, by = "word")
    if(nrow(matched_words) > 0) {
        return(mean(matched_words$value, na.rm = TRUE))
    } else {
        return(NA)
    }
}

# Apply AFINN
hansard_clean$afinn_score <- sapply(hansard_clean$text_processed, compute_afinn_sentiment)

# 6.2 LSD2015 Lexicon ----------------- #

corpus_hansard <- corpus(hansard_clean$text_processed)
liwc_scores <- textstat_polarity(corpus_hansard, dictionary = data_dictionary_LSD2015)
hansard_clean$liwc_score <- liwc_scores$sentiment

# 6.3 Bing Lexicon ----------------- #

bing_sentiment <- get_sentiments("bing")

compute_bing_sentiment <- function(text) {
    words <- tibble(word = unlist(strsplit(tolower(text), "\\s+"))) %>%
        inner_join(bing_sentiment, by = "word")
    if(nrow(words) > 0) {
        score <- sum(words$sentiment == "positive") - sum(words$sentiment == "negative")
        return(score/nrow(words))
    } else {
        return(NA)
    }
}

hansard_clean$bing_score <- sapply(hansard_clean$text_processed, compute_bing_sentiment)

# ============================================================================== #
#  7.0 VISUALIZATION 
# ============================================================================== #
# Compare different lexicons
ggplot(comparison_lexicons) +
    geom_line(aes(x = year_quarter, y = scale(polarity_rescaled), 
                  color = "Our Lexicon"), group = 1) +
    geom_line(aes(x = year_quarter, y = scale(avg_afinn), 
                  color = "AFINN"), group = 1) +
    geom_line(aes(x = year_quarter, y = scale(misery), 
                  color = "Misery Index"), group = 1) +
    annotate("rect", 
            xmin = "1973-Q3",
            xmax = "1975-Q2",
            ymin = -Inf, ymax = Inf,
            alpha = 0.2, fill = "gray") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Sentiment and Economic Indicators",
         subtitle = "Gray band indicates recession period (1973-Q3 to 1975-Q2)",
         y = "Standardized Score",
         color = "Measure Type") +
    scale_color_manual(values = c("Our Lexicon" = "blue", 
                                 "AFINN" = "red",
                                 "Misery Index" = "darkgreen"))
