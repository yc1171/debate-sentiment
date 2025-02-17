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
# - Compares results with general-purpose AFINN lexicon, LSD2015, Bing
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

# Compare with authors' scores
comparison <- our_quarterly %>%
  mutate(
    year = as.numeric(substr(year_quarter, 1, 4)),
    quarter = as.numeric(gsub(".*Q", "", year_quarter))
  ) %>%
  left_join(
    select(quarterly_subset, year, quarter, author_polar = polar),
    by = c("year", "quarter")
  )

# Plot comparing our rescaled scores with authors'
ggplot(comparison) +
  # Add recession shading
  annotate("rect",
           xmin = "1973-Q3",
           xmax = "1975-Q2",
           ymin = -Inf, ymax = Inf,
           alpha = 0.2, fill = "gray") +
  # Sentiment lines with group=1
  geom_line(aes(x = year_quarter, y = polarity_rescaled, 
                color = "Our Sentiment", group = 1)) +
  geom_line(aes(x = year_quarter, y = author_polar, 
                color = "Authors' Scores", group = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Sentiment Over Time",
       subtitle = "Gray band indicates recession period (1973-Q3 to 1975-Q2)",
       y = "Polarity Score",
       color = "Measure Type") +
  scale_color_manual(values = c("Our Sentiment" = "#002A48", 
                                "Authors' Scores" = "#8B0000"))

# join economic data (Misery Index)
comparison_with_econ <- comparison %>%
  left_join(
    select(quarterly_subset, year, quarter, misery, unemp, ldisp),
    by = c("year", "quarter")
  )

# Plot our results against Misery Index
ggplot(comparison_with_econ) +
  # Add recession shading
  annotate("rect",
           xmin = "1973-Q3",
           xmax = "1975-Q2",
           ymin = -Inf, ymax = Inf,
           alpha = 0.2, fill = "gray") +
  geom_line(aes(x = year_quarter, y = polarity_rescaled, 
                group = 1, color = "Our Sentiment")) +
  geom_line(aes(x = year_quarter, y = scale(misery)[,1], 
                group = 1, color = "Misery Index")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Sentiment vs Misery Index",
       subtitle = "Gray band indicates recession period (1973-Q3 to 1975-Q2)",
       y = "Standardized Score",
       color = "Measure Type") +
  scale_color_manual(values = c("Our Sentiment" = "#002A48", 
                                "Misery Index" = "#8B4513"))

# ============================================================================== #
#  6.0 ALTERNATIVE LEXICONS 
# ============================================================================== #

# 6.1 AFINN Lexicon ----------------- #

# Get AFINN lexicon and examine structure
afinn_sentiment <- get_sentiments("afinn")
print("AFINN lexicon preview:")
head(afinn_sentiment, 10)
print("\nAFINN stats:")
print(paste("Number of words:", nrow(afinn_sentiment)))

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

# Apply AFINN and aggregate by quarter
hansard_clean$afinn_score <- sapply(hansard_clean$text_processed, compute_afinn_sentiment)

afinn_quarterly <- hansard_clean %>%
  mutate(
    quarter = ceiling(as.numeric(substr(date, 6, 7))/3),
    year_quarter = paste0(year, "-Q", quarter)
  ) %>%
  group_by(year_quarter) %>%
  summarize(
    avg_afinn = mean(afinn_score, na.rm = TRUE),
    n_speeches = n(),
    sd_afinn = sd(afinn_score, na.rm = TRUE),
    .groups = 'drop'
  )


# Compare with original scores and economic indicators
comparison_lexicons <- comparison %>%
  left_join(afinn_quarterly, by = "year_quarter") %>%
  left_join(
    select(quarterly_subset, year, quarter, misery),
    by = c("year", "quarter")
  )

# Analyze sentiment divergence
analyze_sentiment_comparison <- function(text, custom_lexicon, afinn_sentiment) {
  words <- unlist(strsplit(tolower(text), "\\s+"))
  
  # Find matches in custom lexicon
  custom_matches <- words[words %in% custom_lexicon$lemma]
  custom_scores <- sapply(custom_matches, function(w) {
    score <- custom_lexicon$polarity[custom_lexicon$lemma == w]
    paste(w, ":", round(score, 3))
  })
  
  # Find matches in AFINN
  afinn_matches <- words[words %in% afinn_sentiment$word]
  afinn_scores <- sapply(afinn_matches, function(w) {
    score <- afinn_sentiment$value[afinn_sentiment$word == w]
    paste(w, ":", score)
  })
  
  # Calculate overall scores
  custom_score <- mean(sapply(custom_matches, function(w) 
    custom_lexicon$polarity[custom_lexicon$lemma == w]), na.rm = TRUE)
  afinn_score <- mean(sapply(afinn_matches, function(w) 
    afinn_sentiment$value[afinn_sentiment$word == w]), na.rm = TRUE)
  
  # Print results
  cat("\nText:", substr(text, 1, 200), "...\n")
  cat("\nCustom Lexicon matches:\n")
  if(length(custom_scores) > 0) {
    cat(paste(custom_scores, collapse = "\n"), "\n")
  } else {
    cat("No matches found\n")
  }
  cat("Overall custom score:", round(custom_score, 3), "\n")
  
  cat("\nAFINN Lexicon matches:\n")
  if(length(afinn_scores) > 0) {
    cat(paste(afinn_scores, collapse = "\n"), "\n")
  } else {
    cat("No matches found\n")
  }
  cat("Overall AFINN score:", round(afinn_score, 3), "\n")
  cat("\n", rep("-", 80), "\n")
}


# 6.2 LSD2015 Lexicon ----------------- #

library(quanteda)
library(quanteda.sentiment)

# Create corpus and calculate LSD sentiment
corpus_hansard <- corpus(hansard_clean$text_processed)
liwc_scores <- textstat_polarity(corpus_hansard, dictionary = data_dictionary_LSD2015)
hansard_clean$liwc_score <- liwc_scores$sentiment

# Aggregate by quarter
liwc_quarterly <- hansard_clean %>%
  mutate(
    quarter = ceiling(as.numeric(substr(date, 6, 7))/3),
    year_quarter = paste0(year, "-Q", quarter)
  ) %>%
  group_by(year_quarter) %>%
  summarize(
    avg_liwc = mean(liwc_score, na.rm = TRUE),
    .groups = 'drop'
  )

# Join with comparison data
comparison_with_liwc <- comparison_with_econ %>%
  left_join(liwc_quarterly, by = "year_quarter")


# 6.3 Bing Lexicon ----------------- #

library(tidytext)
library(tidyr)

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

# Calculate Bing scores and aggregate
hansard_clean$bing_score <- sapply(hansard_clean$text_processed, compute_bing_sentiment)

bing_quarterly <- hansard_clean %>%
  mutate(
    quarter = ceiling(as.numeric(substr(date, 6, 7))/3),
    year_quarter = paste0(year, "-Q", quarter)
  ) %>%
  group_by(year_quarter) %>%
  summarize(
    avg_bing = mean(bing_score, na.rm = TRUE),
    .groups = 'drop'
  )

# Join with comparison data
comparison_with_bing <- comparison_with_econ %>%
  left_join(bing_quarterly, by = "year_quarter")


# 6.4 Comparison Across Lexicons ----------------- #

# Function to analyze discrepancies between lexicons
analyze_discrepant_texts <- function(data, lexicon, type="LSD"){
  score_col <- if(type=="LSD") "liwc_score" else "bing_score"
  
  discrepant_texts <- data %>%
    filter(!is.na(!!sym(score_col)) & !is.na(polarity)) %>%
    mutate(
      sentiment_diff = abs(scale(!!sym(score_col))[1] - scale(polarity)[1]),
      comparison_score = !!sym(score_col)
    ) %>%
    arrange(desc(sentiment_diff)) %>%
    select(text_processed, polarity, comparison_score) %>%
    head(10)
  
  for(i in 1:nrow(discrepant_texts)) {
    cat("\nAnalyzing Text", i, "\n")
    cat("Full Text:", discrepant_texts$text_processed[i], "\n\n")
    cat("Custom Score:", round(discrepant_texts$polarity[i], 3), "\n")
    cat(type, "Score:", round(discrepant_texts$comparison_score[i], 3), "\n\n")
    
    words <- unlist(strsplit(tolower(discrepant_texts$text_processed[i]), "\\s+"))
    
    custom_matches <- words[words %in% lexicon$lemma]
    custom_scores <- sapply(custom_matches, function(w) {
      score <- lexicon$polarity[lexicon$lemma == w]
      paste(w, ":", round(score, 3))
    })
    
    if(type == "LSD") {
      comp_matches <- words[words %in% names(data_dictionary_LSD2015)]
      comp_scores <- sapply(comp_matches, function(w) {
        score <- ifelse(data_dictionary_LSD2015[[w]] == "positive", 1, -1)
        paste(w, ":", score)
      })
    } else {
      comp_matches <- words[words %in% bing_sentiment$word]
      comp_scores <- sapply(comp_matches, function(w) {
        sentiment <- bing_sentiment$sentiment[bing_sentiment$word == w]
        score <- ifelse(sentiment == "positive", 1, -1)
        paste(w, ":", score)
      })
    }
    
    cat("Custom Lexicon matches:\n")
    if(length(custom_scores) > 0) {
      cat(paste(custom_scores, collapse = "\n"), "\n")
    } else {
      cat("No matches found\n")
    }
    
    cat("\n", type, "Lexicon matches:\n")
    if(length(comp_scores) > 0) {
      cat(paste(comp_scores, collapse = "\n"), "\n")
    } else {
      cat("No matches found\n")
    }
    
    cat("\n", rep("-", 80), "\n")
  }
}

# Analyze the most discrepent texts with LSD
analyze_discrepant_texts(hansard_clean, lexicon, "LSD")

# Analyze the most discrepent texts with Bing
analyze_discrepant_texts(hansard_clean, lexicon, "Bing")

# ============================================================================== #
#  7.0 VISUALIZATION FOR ALTERNATIVE LEXICONS
# ============================================================================== #

# Comparison with AFINN results (along with Misery Index)
ggplot(comparison_lexicons) +
  # Sentiment lines
  geom_line(aes(x = year_quarter, y = scale(polarity_rescaled), 
                color = "Our Lexicon"), group = 1) +
  geom_line(aes(x = year_quarter, y = scale(avg_afinn), 
                color = "AFINN"), group = 1) +
  geom_line(aes(x = year_quarter, y = scale(misery), 
                color = "Misery Index"), group = 1) +
  # Add recession period marker
  annotate("rect", 
           xmin = which(comparison_lexicons$year_quarter == "1973-Q3"),
           xmax = which(comparison_lexicons$year_quarter == "1975-Q2"),
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


# Comparison with LSD2015 results (with Misery Index)
ggplot(comparison_with_liwc) +
  annotate("rect",
           xmin = "1973-Q3",
           xmax = "1975-Q2",
           ymin = -Inf, ymax = Inf,
           alpha = 0.2, fill = "gray") +
  geom_line(aes(x = year_quarter, y = polarity_rescaled, 
                group = 1, color = "Our Lexicon")) +
  geom_line(aes(x = year_quarter, y = scale(avg_liwc), 
                group = 1, color = "LSD2015")) +
  geom_line(aes(x = year_quarter, y = scale(misery),
                group = 1, color = "Misery Index")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "LSD2015 Analysis with Economic Context",
       subtitle = "Gray band indicates recession period (1973-Q3 to 1975-Q2)",
       y = "Standardized Score",
       color = "Measure Type") +
  scale_color_manual(values = c("Our Lexicon" = "#013D80", 
                                "LSD2015" = "#6B1082",
                                "Misery Index" = "#006400"))

comparison_with_bing <- comparison_with_econ %>%
  left_join(bing_quarterly, by = "year_quarter")


# Comparison with BING results (with Misery Index)
ggplot(comparison_with_bing) +
  annotate("rect",
           xmin = "1973-Q3",
           xmax = "1975-Q2",
           ymin = -Inf, ymax = Inf,
           alpha = 0.2, fill = "gray") +
  geom_line(aes(x = year_quarter, y = polarity_rescaled, 
                group = 1, color = "Our Lexicon")) +
  geom_line(aes(x = year_quarter, y = scale(avg_bing), 
                group = 1, color = "Bing")) +
  geom_line(aes(x = year_quarter, y = scale(misery),
                group = 1, color = "Misery Index")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Sentiment Measures and Economic Context",
       subtitle = "Gray band indicates recession period (1973-Q3 to 1975-Q2)",
       y = "Standardized Score",
       color = "Measure Type") +
  scale_color_manual(values = c("Our Lexicon" = "#021B48", 
                                "Bing" = "#7B0082",
                                "Misery Index" = "#006400"))