# Set up language environment
## Sys.setlocale("LC_TIME",locale = "en_US.UTF-8")

# Use text mining package
#library(tm)
library(tau)
library(stringi)
library(ngram)

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


# Clean text

Clean_Docs <- function(docs) {
    docs.cl <- iconv(docs, "UTF-8", "ASCII", "")
    docs.cl <- removePunctuation(docs.cl)
    docs.cl <- stripWhitespace(docs.cl)
}

blogs.docs <- unlist( lapply(blogs.docs, Clean_Docs) )
news.docs <- unlist( lapply(news.docs, Clean_Docs) )
twitter.docs <- unlist( lapply(twitter.docs, Clean_Docs) )

#blogs.docs <- iconv(blogs.docs, "UTF-8", "ASCII", "?")
#blogs.docs <- tolower(blogs.docs)
#blogs.docs <- removePunctuation(blogs.docs)
#blogs.docs <- removeWords(blogs.docs, stopwords("english"))
#blogs.docs <- stripWhitespace(blogs.docs)
#
#news.docs <- iconv(news.docs, "UTF-8", "ASCII", "?")
#news.docs <- tolower(news.docs)
#news.docs <- removePunctuation(news.docs)
#news.docs <- removeWords(news.docs, stopwords("english"))
#news.docs <- stripWhitespace(news.docs)
#
#twitter.docs <- iconv(twitter.docs, "UTF-8", "ASCII", "?")
#twitter.docs <- tolower(twitter.docs)
#twitter.docs <- removePunctuation(twitter.docs)
#twitter.docs <- removeWords(twitter.docs, stopwords("english"))
#witter.docs <- stripWhitespace(twitter.docs)

load("../Capstone_Cleaned.RData")

# Split words in every document
blogs.tokens <- strsplit(blogs.docs, " ")
news.tokens <- strsplit(news.docs, " ")
twitter.tokens <- strsplit(twitter.docs, " ")


# Explore the distributions of single word
blogs.tokensdist <- log10( table( unlist(blogs.tokens) ) )
hist( blogs.tokensdist[blogs.tokensdist > 0] )

news.tokensdist <- log10( table( unlist(news.tokens) ) )
hist( news.tokensdist[news.tokensdist > 0] )

twitter.tokensdist <- log10( table( unlist(twitter.tokens) ) )
hist( twitter.tokensdist[twitter.tokensdist > 0] )

# 

sort( table( unlist(blogs.tokens) ), decreasing = TRUE)[1:100]

# summarize 2-gram
## Erase one-word docs
blogs.docs <- blogs.docs[grep(" ", blogs.docs)]
blogs.tokens <- blogs.docs[grep(" ", blogs.docs)]

blogs.docs.2gram <- list()
i = 1
for(i in 1:length(blogs.docs)){
    blogs.docs.2gram[i] <- list(get.ngrams( ngram(blogs.docs[i], n = 2) ))
    i = i + 1
}


# Select the samples for build corpus
## I have to consider the number of characters in a document!



## Explore the information density
hist( log10( sort( nchar(blogs.docs), decreasing = TRUE) ), main = "Blog documents", ylab = "Number of documents", xlab = "Logged length of blog document")
abline(v = round ( log10( summary( nchar(blogs.docs) )[3] ), digits=0 ), col="red")
hist( log10( sort( nchar(news.docs), decreasing = TRUE) ), main = "News documents", ylab = "Number of documents", xlab = "Logged length of news document")
abline(v = round ( log10( summary( nchar(news.docs) )[3] ), digits=0 ), col="red")
hist( ( sort( nchar(twitter.docs), decreasing = TRUE) ), main = "Twitter documents", ylab = "Number of documents", xlab = "Length of news document" )
abline(v = round ( summary( nchar(twitter.docs) )[2], digits=0 ), col="red")
## blogs had > 100 characters; news had > 100 characters; twitter <= 24 characters

## Generate corpus
(blogs.corpus <-Corpus(VectorSource(blogs.docs[nchar(blogs.docs) > 100]), readerControl = list(language = "en_US") ))
(news.corpus <-Corpus(VectorSource(news.docs[nchar(news.docs) > 100]), readerControl = list(language = "en_US") ))
(twitter.corpus <-Corpus(VectorSource(twitter.docs[nchar(twitter.docs) <= 24]), readerControl = list(language = "en_US") ))


# Create Term-Document Matrices
blogs.dtm <- DocumentTermMatrix(blogs.corpus)
blogs.dtm <- DocumentTermMatrix(blogs.docs)

blogs.dtm2 <- removeSparseTerms(blogs.dtm, 0.7)

#gsub( "[.|,|?|:]", "", readLines(con2, 2) )


######################

zipfile <- "./data/Swiftkey.zip"
#zipfile <- tempfile()
#files <- 
#zip(zipfile, fileurl)
#zipfile <- paste0(zipfile, ".zip")
Swifty.corpus <- Corpus(ZipSource(zipfile, recursive = TRUE))
file.remove(zipfile)

docs.cl <- removeWords(docs.cl, stopwords("english"))
docs.cl <- tolower(docs.cl)
