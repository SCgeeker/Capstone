# Use text mining package
library(tm)
library(NLP)
library(openNLP)
library(tau)
library(stringi)
library(RWeka)
library(dplyr)

# Sample test data
# Import raw documents

con1 <- unz("./data/Swiftkey.zip","final/en_US/en_US.blogs.txt")
con2 <- unz("./data/Swiftkey.zip","final/en_US/en_US.news.txt")
con3 <- unz("./data/Swiftkey.zip","final/en_US/en_US.twitter.txt")

blogs.docs <- readLines(con1, encoding="UTF-8", skipNul = TRUE)
news.docs <- readLines(con2, encoding="UTF-8", skipNul = TRUE)
twitter.docs <- readLines(con3, encoding="UTF-8", skipNul = TRUE)
close(con1)
close(con2)
close(con3)
rm(con1, con2, con3)

# Sampling the docs for training set
{
set.seed(12345)
blogs.test.seq <- sample( -sample(1:length(blogs.docs), length(blogs.docs)*.1), .001)
news.seq <-  sample(1:length(news.docs), length(news.docs)*.1)
twitter.seq <-  sample(1:length(twitter.docs), length(twitter.docs)*.1)
}

# sampling the docs for test set
{
set.seed(12345)
blogs.test.seq <- 1:length(blogs.docs)
blogs.test.seq <- sample(blogs.test.seq[! 1:length(blogs.docs) %in% blogs.seq], length(blogs.docs)*.01)
news.test.seq <- 1:length(news.docs)
news.test.seq <- sample(news.test.seq[! 1:length(news.docs) %in% news.seq], length(news.docs)*.01)
twitter.test.seq <- 1:length(twitter.docs)
twitter.test.seq <- sample(twitter.test.seq[! 1:length(twitter.docs) %in% twitter.seq], length(twitter.docs)*.01)
}


Test.doc <- c(blogs.docs[blogs.test.seq], 
              news.docs[news.test.seq], 
              twitter.docs[twitter.test.seq])
rm(blogs.docs, news.docs, twitter.docs, blogs.seq, news.seq, twitter.seq, blogs.test.seq, news.test.seq, twitter.test.seq)

