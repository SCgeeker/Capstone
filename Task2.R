# Set up language environment
## Sys.setlocale("LC_TIME",locale = "en_US.UTF-8")

# Use text mining package
library(tm)
library(tau)
library(stringi)

# Import the raw data

con1 <- unz("./data/Swiftkey.zip","final/en_US/en_US.blogs.txt")
con2 <- unz("./data/Swiftkey.zip","final/en_US/en_US.news.txt")
con3 <- unz("./data/Swiftkey.zip","final/en_US/en_US.twitter.txt")

#blogs.length <- length(readLines(con1))
#news.length <- length(readLines(con2))
#twitter.length <- length(readLines(con3))

blogs.docs <- readLines(con1, encoding = "UTF-8")
news.docs <- readLines(con2, encoding = "UTF-8")
twitter.docs <- readLines(con3, encoding = "UTF-8")
close(con1)
close(con2)
close(con3)
rm(con1, con2, con3)

# 

# Clean data
blogs.docs <- iconv(blogs.docs, "UTF-8", "ASCII", "?")
blogs.docs <- tolower(blogs.docs)
blogs.docs <- removePunctuation(blogs.docs)
blogs.docs <- removeWords(blogs.docs, stopwords("english"))
blogs.docs <- stripWhitespace(blogs.docs)

news.docs <- iconv(news.docs, "UTF-8", "ASCII", "?")
news.docs <- tolower(news.docs)
news.docs <- removePunctuation(news.docs)
news.docs <- removeWords(news.docs, stopwords("english"))
news.docs <- stripWhitespace(news.docs)

twitter.docs <- iconv(twitter.docs, "UTF-8", "ASCII", "?")
twitter.docs <- tolower(twitter.docs)
twitter.docs <- removePunctuation(twitter.docs)
twitter.docs <- removeWords(twitter.docs, stopwords("english"))
twitter.docs <- stripWhitespace(twitter.docs)

# Select the samples for Corpus
## I have to consider the number of characters in a document!
hist( log10( sort( nchar(blogs.docs), decreasing = TRUE) ) )
hist( log10( sort( nchar(news.docs), decreasing = TRUE) ) )
hist( log10( sort( nchar(twitter.docs), decreasing = TRUE) ) )

set.seed(100)
(blogs.corpus <-Corpus(VectorSource(blogs.docs[sample(1:length(blogs.docs), round(length(blogs.docs)*.3, digits=0) )]), readerControl = list(language = "en_US") ))
set.seed(100)
(news.corpus <-Corpus(VectorSource(news.docs[sample(1:length(news.docs), round(length(news.docs)*.1, digits=0) )]), readerControl = list(language = "en_US") ))
set.seed(100)
(twitter.corpus <-Corpus(VectorSource(twitter.docs[sample(1:length(twitter.docs), round(length(twitter.docs)*.1, digits=0) )]), readerControl = list(language = "en_US") ))


# Create Term-Document Matrices
blogs.dtm <- DocumentTermMatrix(blogs.corpus)


#gsub( "[.|,|?|:]", "", readLines(con2, 2) )


######################

zipfile <- "./data/Swiftkey.zip"
#zipfile <- tempfile()
#files <- 
#zip(zipfile, fileurl)
#zipfile <- paste0(zipfile, ".zip")
Swifty.corpus <- Corpus(ZipSource(zipfile, recursive = TRUE))
file.remove(zipfile)
