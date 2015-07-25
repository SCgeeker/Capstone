# Set up language environment
## Sys.setlocale("LC_TIME",locale = "en_US.UTF-8")

# Use text mining package
#library(tm)
library(tau)
library(stringi)
#library(ngram)

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

# Erase docs have less than 4 words
blogs.docs <- unlist( lapply(blogs.docs, Clean_Docs) )
blogs.docs <- blogs.docs[(stri_count_fixed(blogs.docs, " "))>2]
news.docs <- unlist( lapply(news.docs, Clean_Docs) )
news.docs <- news.docs[(stri_count_fixed(news.docs, " "))>2]
twitter.docs <- unlist( lapply(twitter.docs, Clean_Docs) )
twitter.docs <- twitter.docs[(stri_count_fixed(twitter.docs, " "))>2]

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

# load("../Capstone_Cleaned.RData")

# Split words in every document
##blogs.tokens <- strsplit(blogs.docs, " ")
##news.tokens <- strsplit(news.docs, " ")
##twitter.tokens <- strsplit(twitter.docs, " ")


# Explore the distributions of word frequencies
blogs.words <- textcnt(blogs.docs, method = "string", n = 1L)
##summary(log10(blogs.words)[log10(blogs.words) > 1])
##round(max(log10(blogs.words) ), digit=0 )
blogs.dist <- hist( log10(blogs.words), breaks = 0:(round(max(log10(blogs.words) ), digit=0 ) +1 ), xlab = "Word Frequencies (log10)", ylab="Counts in blogs", main = "Distribution of Log Word Frequency")
text(blogs.dist$mids, blogs.dist$counts + 10000, round(blogs.dist$density, digits = 2) )
abline( v =summary(log10(blogs.words))[3:4], col = c("red","blue"))

news.words <- textcnt(news.docs, method = "string", n = 1L)
##summary(log10(news.words)[log10(news.words) > 1])
news.dist <- hist( log10(news.words), breaks = 0:(round(max(log10(news.words) ), digit=0 ) +1 ), xlab = "Word Frequencies (log10)", ylab="Counts in news", main = "Distribution of Log Word Frequency")
text(news.dist$mids, news.dist$counts + 10000, round(news.dist$density, digits = 2) )
abline( v =summary(log10(news.words))[3:4], col = c("red","blue"))

twitter.words <- textcnt(twitter.docs, method = "string", n = 1L)
## summary(log10(twitter.words)[log10(twitter.words) > 1])
twitter.dist <- hist( log10(twitter.words), breaks = 0:(round(max(log10(twitter.words) ), digit=0 ) + 1), xlab = "Word Frequencies (log10)", ylab="Counts in twitter", main = "Distribution of Log Word Frequency")
text(twitter.dist$mids, twitter.dist$counts + 10000, round(twitter.dist$density, digits = 2) )
abline( v =summary(log10(twitter.words))[3:4], col = c("red","blue"))

##old_objs <- ls()
US_docs <- c(blogs.docs, news.docs, twitter.docs)
rm(blogs.docs, news.docs, twitter.docs, blogs.words, news.words, twitter.words, blogs.dist, news.dist, twitter.dist)
US_words <- textcnt(US_docs, method = "string", n = 1L)
US_dist <- hist( log10(US_words), breaks = 0:(round(max(log10(US_words) ), digit=0 ) + 1), xlab = "US Word Frequencies (log10)", ylab="Counts in US docs", main = "Distribution of Log Word Frequency")
text(US_dist$mids, US_dist$counts + 30000, round(US_dist$density, digits = 2) )
abline( v =summary(log10(US_words))[3:4], col = c("red","blue"))



# Find the cut line
summary( stri_count_fixed(US_docs, " ") )

# Separate all docs to short (< 16 words) and long (>= 16 words)
US_short_docs <- US_docs[stri_count_fixed(US_docs, " ") < 16] 
US_long_docs <- US_docs[stri_count_fixed(US_docs, " ") >= 16]


US_short_words <- textcnt(US_short_docs, method = "string", n = 1L)
US_short_dist <- hist( log10(US_short_words), xlab = "US Word Frequencies (log10)", ylab="Counts in US docs", main = "Distribution of Log Word Frequency")
text(US_short_dist$mids, US_short_dist$counts + 30000, round(US_short_dist$density, digits = 2) )
abline( v =summary(log10(US_words))[3:4], col = c("red","blue"))



#############################################################

# Extract 10% sample docs from three sources
##sample.docs <- list()
sample.docs <- c(blogs.docs[sample(1:length(blogs.docs), length(blogs.docs)*.1)], news.docs[sample(1:length(news.docs), length(news.docs)*.1)], twitter.docs[sample(1:length(twitter.docs), length(twitter.docs)*.1)])

# Explore the 2-gram and 3-gram in sample docs
##textcnt(blogs.docs[1:5], method = "string", n = 1L)

sample.2grams <- textcnt(sample.docs, method = "string", n = 2L)
summary(sample.2grams)
sample.2grams.dist <- hist( log10(sample.2grams), xlab = "2-gram Frequencies", ylab="Counts in sample docs", main = "Distribution of Log 2-gram Frequency" )

##breaks = 0:(round(max(sample.2grams), digits=0) + 1), 

text(sample.2grams.dist$mids, sample.2grams.dist$counts + 70000, round(sample.2grams.dist$density, digits = 2) )

sample.3grams <- textcnt(sample.docs, method = "string", n = 3L)
summary(sample.3grams)
sample.3grams.dist <- hist( log10(sample.3grams), xlab = "3-gram Frequencies (log10)", ylab="Counts in sample docs", main = "Distribution of Log 3-gram Frequency" )
text(sample.3grams.dist$mids, sample.3grams.dist$counts + 70000, round(sample.3grams.dist$density, digits = 2) )

##, breaks = 0:(round(max(sample.3grams), digits=0) + 1)

##blogs.wordsdist <- log10( table( unlist(blogs.tokens) ) )
##news.tokensdist <- log10( table( unlist(news.tokens) ) )
##twitter.tokensdist <- log10( table( unlist(twitter.tokens) ) )
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
