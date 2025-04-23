# Loading Packages
library(tidyverse)
library(stopwords)

# Loading Text 
raw_text <- readLines("The Sorrows of Young Werther.txt", encoding = "UFT-8")

# All lines in one string
text_unified <- paste(raw_text, collapse = " ")


# All Lowercase 
text_lower <- tolower(text_unified)

# Remove Punctuation 
text_no_punct <- str_replace_all(text_lower, "[[:punct:]]", "")

# Word division 
words <- unlist(str_split(text_no_punct, "\\s+"))

# Removing stop words
stopwords <- stopwords("en")

no_stopwords <- words[!words %in% stopwords]

# Possible empty "chains"
no_stopwords <- no_stopwords[no_stopwords != ""]

# Word Frequency Calculation
word_freq <- table(no_stopwords)
df_freq <- as.data.frame(word_freq)
colnames(df_freq) <- c("Words", "Frequency")

# Order by Frequency
df_ordered <- df_freq[order(df_freq$Frequency, decreasing = TRUE), ]


# Visualization of top repeated words

top_words <- 25 #Could be any number you like

ggplot(df_ordered[1:top_words, ], aes(x = reorder(Words, Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "lightcoral") +
  coord_flip() +
  labs(title = paste("Top", top_words, "Most Frequent Words in 'The Sorrows of Young Werther'"),
       x = "Word",
       y = "Frequency") +
  theme_minimal()
