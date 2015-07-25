library(koRpus)
library(tm)
library(tau)
library(stringi)
library(dplyr)
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
Training.unimodel <- log( Training.uni/length(Training.doc) )

# Smoothing unimodel
LAMBA <- .95
Training.unimodel.smooth <- log( LAMBA*Training.unimodel + (1 - LAMBA)*(1/length(Training.unimodel)) )
rm(Training.uni)
# Give prior probability to unknown words


# Append sentence boundries to each doc
Training.doc <- paste0(paste("si", Training.doc, " "), "se")
    
Training.bi <- textcnt(Training.doc, tolower = FALSE, method = "string", n = 2, decreasing = TRUE)
Training.bi <- Training.bi[-1]
Training.bi.df <- data.frame( cbind(matrix( unlist( strsplit( names(Training.bi), " " ) ),nc=2, byrow=T), Training.bi) )
colnames(Training.bi.df) <- c("gram1", "gram2", "c1")
Training.bi.df$gram1 <- as.factor(Training.bi.df$gram1)
Training.bi.df$gram2 <- as.factor(Training.bi.df$gram2)
Training.bi.df$c1 <- as.numeric(Training.bi.df$c1)

token1 <- with(tapply(c1, gram1, sum, na.rm = T), data = Training.bi.df)
## token1_sum <- token1[Training.bi.df$gram1]
type1 <- table(Training.bi.df$gram1)

Training.bi.df <- data.frame( 
    cbind(
        cbind( 
            Training.bi.df,
            p_unigram = token1[Training.bi.df$gram1]/sum(Training.bi.df$c1)
        ),
        p_bigrams = Training.bi.df$c1/token1[Training.bi.df$gram1]
    )
)

# make sure every word has type frequency and token freqnecy
sum( names(type1) == names(token1) )

# Smoothing Bimodel
LAMBA.bi <- 1 - token1/(token1+type1)

Training.bi.df <- data.frame( 
    cbind(
        Training.bi.df,
        p_smooth_bigrams = LAMBA.bi[Training.bi.df$gram1]*Training.bi.df$p_bigrams + (1 - LAMBA.bi[Training.bi.df$gram1])*Training.bi.df$p_unigram
    )
)
rm(token1, type1)

# Make up Trigrams
Training.tri <-  textcnt(Training.doc, tolower = FALSE, method = "string", n = 3, decreasing = TRUE)
Training.tri.df <- data.frame( cbind(matrix( unlist( strsplit( names(Training.tri), " " ) ),nc=3, byrow=T), Training.tri) )
colnames(Training.tri.df) <- c("gram1","gram2","gram3","c1")
Training.tri.df$gram1 <- as.factor(Training.tri.df$gram1)
Training.tri.df$gram2 <- as.factor(Training.tri.df$gram2)
Training.tri.df$gram3 <- as.factor(Training.tri.df$gram2)
Training.tri.df$c1 <- as.numeric(Training.tri.df$c1)
