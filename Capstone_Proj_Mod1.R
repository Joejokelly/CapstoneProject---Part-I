library(rJava)
library(knitr)
library(caTools)
library(dplyr)

library(ggplot2)
library(NLP)
#library(qdap)
#library(igraph)

library(stringr)
library(wordcloud)
library(tm)
library(RWeka)

#library(qdap)
#library(quanteda)
#library(tm)

getwd()

setwd("~/Documents/1.0 - Joe Personal/3.0 - R Programming/1.26 - SwiftKey Project/final")

#en_blogs <- readLines(paste("E:/Data sciences specialisation/10.capstone/Coursera-SwiftKey/final/en_US/en_US.blogs.txt",sep=""))

en_blogs <- readLines(paste("en_US/en_US.blogs.txt", sep=""))

#setwd("~/Documents/1.0 - Joe Personal/3.0 - R Programming/1.26 - SwiftKey Project/final")

en_news <- readLines(paste("en_US/en_US.news.txt",sep=""))

en_twitter <- readLines(paste("en_US/en_US.twitter.txt",sep=""))

data_all <- c(en_blogs,en_news,en_twitter)

set.seed(311344)
en_blogs_subset    <- sample(en_blogs,1000)
en_news_subset <-  sample(en_news,1000)
en_twitter_subset    <- sample(en_twitter,1000)

sampleData <- c(en_blogs_subset, en_twitter_subset ,en_twitter_subset)

writeLines(sampleData, "sampleData.txt")

#rm(twitter,news,blogs,sampleTwitter,sampleNews,sampleBlogs)
 
rm(en_blogs_subset)
rm(en_news_subset)
rm(en_twitter_subset)

sampleData <- readLines("sampleData.txt", encoding="UTF-8")
corpus <- VCorpus(VectorSource(sampleData))
#Processing the dataset.

## Remove space, punctuation, numbers, whitespace, stopwords and change to lowercase
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus <- tm_map(corpus, toSpace, "\"|/|@|\\|")
corpus <- tm_map(corpus, toSpace, "[^[:graph:]]")
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removeWords, stopwords("english"))

wordcloud(corpus, min.freq=5, max.words=101, random.order=TRUE,
          rot.per=0.5, colors=brewer.pal(8, "Set2"), use.r.layout=FALSE)


corpus.dataframe <- data.frame(text = unlist(sapply(corpus, '[', 'content')), stringsAsFactors = F)

uniGramToken <- data.frame(table(NGramTokenizer(corpus.dataframe, Weka_control(min = 1, max = 1))))
biGramToken <- data.frame(table(NGramTokenizer(corpus.dataframe, Weka_control(min = 2, max = 2))))
triGramToken <- data.frame(table(NGramTokenizer(corpus.dataframe, Weka_control(min = 3, max = 3))))

#order by decreasing frequency
unigram <- uniGramToken[order(uniGramToken$Freq, decreasing = TRUE),]
bigram  <- biGramToken[order(biGramToken$Freq, decreasing = TRUE),]
trigram <- triGramToken[order(triGramToken$Freq, decreasing = TRUE),]


#1.0

par(mfrow = c(1, 1))
par(mar=c(5,4,2,0))
barplot(unigram[1:30,2], 
        names.arg=unigram[1:30,1], 
        col = "red", 
        main="Most commonly used Words (Top 30)", 
        las=2, 
        ylab = "Frequency")


#2.0

par(mar=c(8.5,4,2,1))
barplot(bigram[1:30,2], 
        names.arg=bigram[1:30,1], 
        col = "blue", 
        main="Most commonly used two word combinations (Top 30)", 
        las=2, 
        ylab = "Frequency")


#3.0

par(mar=c(8.5,4,2,1))
barplot(trigram[1:30,2], 
        names.arg=trigram[1:30,1], 
        col = "green", 
        main="Most commonly used three word combinations (Top 30)", 
        las=2, 
        ylab = "Frequency")




