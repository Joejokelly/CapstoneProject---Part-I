library(caTools)
library(dplyr)

library(ggplot2)
library(NLP)
library(rJava)
library(knitr)

library(stringr)
library(wordcloud)
library(tm)
library(RWeka)

#getwd()

setwd("~/Documents/1.0 - Joe Personal/3.0 - R Programming/1.26 - SwiftKey Project/final")
getwd()


en_blogs <- readLines(paste("en_US/en_US.blogs.txt", sep=""))
en_news <- readLines(paste("en_US/en_US.news.txt",sep=""))
en_twitter <- readLines(paste("en_US/en_US.twitter.txt",sep=""))

data_all <- c(en_blogs,en_news,en_twitter)

set.seed(311344)
en_blogs_subset    <- sample(en_blogs,1000)
en_news_subset <-  sample(en_news,1000)
en_twitter_subset    <- sample(en_twitter,1000)

sampleData <- c(en_blogs_subset, en_twitter_subset ,en_twitter_subset)

writeLines(sampleData, "sampleData.txt")

rm(en_blogs_subset)
rm(en_news_subset)
rm(en_twitter_subset)

sampleData <- readLines("sampleData.txt", encoding="UTF-8")
data_corpus <- VCorpus(VectorSource(sampleData))

## Remove space, punctuation, numbers, whitespace, stopwords and change to lowercase
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
data_corpus <- tm_map(corpus, toSpace, "\"|/|@|\\|")
data_corpus <- tm_map(corpus, toSpace, "[^[:graph:]]")
data_corpus <- tm_map(corpus, content_transformer(tolower))
data_corpus <- tm_map(corpus, removePunctuation)
data_corpus <- tm_map(corpus, removeNumbers)
data_corpus <- tm_map(corpus, stripWhitespace)
data_corpus <- tm_map(corpus, stemDocument)
data_corpus <- tm_map(corpus, PlainTextDocument)
data_corpus <- tm_map(corpus, removeWords, stopwords("english"))

wordcloud(data_corpus, min.freq=5, max.words=101, random.order=TRUE,
          rot.per=0.5, colors=brewer.pal(8, "Set2"), use.r.layout=FALSE)

#1.0

uniGramToken <- data.frame(table(NGramTokenizer(corpus.dataframe, Weka_control(min = 1, max = 1))))
unigram <- uniGramToken[order(uniGramToken$Freq, decreasing = TRUE),]

par(mfrow = c(1, 1))
par(mar=c(5,4,2,0))
barplot(unigram[1:30,2], 
        names.arg=unigram[1:30,1], 
        col = "light blue", 
        main="Most commonly used Words (Top 30)", 
        las=2, 
        ylab = "Frequency")


#2.0
biGramToken <- data.frame(table(NGramTokenizer(corpus.dataframe, Weka_control(min = 2, max = 2))))
bigram  <- biGramToken[order(biGramToken$Freq, decreasing = TRUE),]

par(mar=c(8.5,4,2,1))
barplot(bigram[1:30,2], 
        names.arg=bigram[1:30,1], 
        col = "pink", 
        main="Most commonly used two word combinations (Top 30)", 
        las=2, 
        ylab = "Frequency")

#3.0
triGramToken <- data.frame(table(NGramTokenizer(corpus.dataframe, Weka_control(min = 3, max = 3))))
trigram <- triGramToken[order(triGramToken$Freq, decreasing = TRUE),]

par(mar=c(8.5,4,2,1))
barplot(trigram[1:30,2], 
        names.arg=trigram[1:30,1], 
        col = "green", 
        main="Most commonly used three word combinations (Top 30)", 
        las=2, 
        ylab = "Frequency")

