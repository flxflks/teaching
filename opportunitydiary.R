setwd("C:/Users/KAYSERS/OneDrive - Aescra Emlyon Business School/Research/4. PhD EM Lyon/Teaching/Data")
diary <- read.csv2("opportunitydiary.csv", na.strings = c("","NA"))

diary$Source <- as.factor(diary$Source)
diary$Type.of.business <- as.factor(diary$Type.of.business)
diary$Number.of.words <- as.factor(diary$Number..of.words)

library(tidyverse)
library(viridis)
library(ggplot2)
library(stringr)
library(tm)
library(wordcloud)
library(wordcloud2)

diary <- diary[!is.na(diary$Opportunity),]
diary <- diary %>% drop_na(Opportunity)

# Source
ggplot(diary, aes(fct_infreq(Source), fill = Source)) +
  geom_bar() + 
  scale_color_viridis(option = "D") + 
  guides(x = guide_axis(angle =-45), fill = "none") +
  labs(x = "")

# Type of business
ggplot(diary, aes(fct_infreq(Type.of.business), fill = Type.of.business)) +
  geom_bar() + 
  scale_color_viridis(option = "D") + 
  guides(x = guide_axis(angle =-45), fill = "none") +
  labs(x = "")

# Word count
# Use stringr in the next version to count the number of words
ggplot(diary, aes(Number..of.words)) + 
  geom_histogram(aes(y=..density..), colour="white", fill="#FF0000") + 
  geom_density(alpha=.5, fill="#FF0000", colour = "darkgrey")

# Word cloud 
docs <- Corpus(VectorSource(diary$Business.description))

docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

set.seed(1234) 
# for reproducibility 
wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))


wordcloud2(data=df, size=1.6, color='random-dark')
brewer.pal
