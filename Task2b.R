library(koRpus)
library(tm)
library(tau)
library(stringi)
#set.kRp.env(lang="en")

load("../Capstone.RData")

## Sampling the docs for training set
{
set.seed(12345)
blogs.seq <- sample(1:length(blogs.docs), length(blogs.docs)*.1)
news.seq <-  sample(1:length(news.docs), length(news.docs)*.1)
twitter.seq <-  sample(1:length(twitter.docs), length(twitter.docs)*.1)
}

Training.doc <- c(blogs.docs[blogs.seq], news.docs[news.seq], twitter.docs[twitter.seq])

## Erase UTF-8 codes
Training.doc <- iconv(Training.doc, "UTF-8", "ASCII", "")
## Strip the white space in text
Training.doc <- stripWhitespace(Training.doc)

Training.uni <- textcnt(Training.doc, tolower = FALSE, method = "string", n = 1, decreasing = TRUE)
Training.unimodel <- Training.uni/length(Training.doc)


Training.bi <-  textcnt(Training.doc, tolower = FALSE, method = "string", n = 2, decreasing = TRUE)
Training.bi.df <- cbind(matrix( unlist( strsplit( names(Training.bi), " " ) ),nc=2, byrow=T), Training.bi)
colnames(Training.bi.df) <- c("frist", "second", "grams")


Training.tri <-  textcnt(Training.doc, tolower = FALSE, method = "string", n = 3, decreasing = TRUE)

#TEST <- tokenize(blogs.docs[1:5], format = "obj", ign.comp = NULL, sentc.end = NULL, lang = "en", stopwords=tm::stopwords("en"), stemmer=SnowballC::wordStem)

#TEST@TT.res

#sort( table(TEST@TT.res$stem), decreasing = TRUE)


## clean.raw = list("A-Z" = "a-z"), perl = TRUE,