library(koRpus)
library(tm)
library(NLP)
library(openNLP)
library(tau)
library(stringi)
library(dplyr)
#set.kRp.env(lang="en")

load("../Capstone_Docs.RData")

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



#Training.doc[Training.doc == " "] <- 0
#Training.doc[Training.doc == ""] <- 0

sent_token_annotator <- Maxent_Sent_Token_Annotator()
#Training.test <- as.String(Training.doc[4])

Training.sen <- NULL
for(i in 1:length(Training.doc)){
    if(nchar(as.String(Training.doc[i])) <= 1 ){}
    else{
        doc <- as.String(Training.doc[i])
        t <- annotate(doc, sent_token_annotator)
        Training.sen <- c(Training.sen, doc[t])
    }
    print(i)
}

# Erase Punctuation
Training.sen <- removePunctuation(Training.sen)

## Build unimodel
Training.uni <- textcnt(Training.sen, tolower = FALSE, method = "string", n = 1, decreasing = TRUE)
Training.unimodel <- Training.uni/length(Training.sen)

# Smoothing unimodel
LAMBA.uni <- .95
Training.unimodel.smooth <- log( LAMBA.uni*Training.unimodel + (1 - LAMBA.uni)*(1/length(Training.unimodel)) )
#rm(Training.uni, Training.unimodel)
# Give prior probability to unknown words


# Append sentence boundries to each doc
Training.sen1 <- paste0(paste("si", Training.sen, " "), "se")

Training.bi <- textcnt(Training.sen1, tolower = FALSE, method = "string", n = 2, decreasing = TRUE)
Training.bi <- Training.bi[-1] # Remove "se si"
Training.bi.df <- data.frame( cbind(matrix( unlist( strsplit( names(Training.bi), " " ) ),nc=2, byrow=T), Training.bi) )
colnames(Training.bi.df) <- c("prior1", "word", "c1")
Training.bi.df$prior1 <- as.factor(Training.bi.df$prior1)
Training.bi.df$word <- as.factor(Training.bi.df$word)
Training.bi.df$c1 <- as.numeric(Training.bi.df$c1)
rm(Training.bi)
Training.bi.df <- data.frame( 
    cbind(
        cbind( 
            Training.bi.df,
            p_prior = Training.unimodel[as.character(Training.bi.df$prior1)]
        ),
        p_bigrams = Training.bi.df$c1/Training.uni[as.character(Training.bi.df$prior1)]
    )
)

type1 <- table(Training.bi.df$prior1)

# make sure every word has type frequency and token freqnecy
##sum( names(type1) == names(token1) )

# Smoothing Bimodel
LAMBA.bi <- 1 - Training.uni[names(type1)]/(Training.uni[names(type1)]+type1)
rm(type1)

Training.bi.df <- data.frame( 
    cbind(
        Training.bi.df,
        p_smooth_bigrams = LAMBA.bi[as.character(Training.bi.df$prior1)]*Training.bi.df$p_bigrams + (1 - LAMBA.bi[as.character(Training.bi.df$prior1)])*Training.unimodel[as.character(Training.bi.df$word)]
    )
)
##colnames(Training.bi.df) <- c("prior1", "word", "c1", "p_prior","p_bigrams","p_smooth_bigrams")


# Build Trigrams
Training.tri <-  textcnt(Training.sen1, tolower = FALSE, method = "string", decreasing = TRUE)
Training.tri.df <- data.frame( cbind(matrix( unlist( strsplit( names(Training.tri), " " ) ),nc=3, byrow=T), Training.tri) )
colnames(Training.tri.df) <- c("prior1","prior2","word","c1")
Training.tri.df$prior1 <- as.factor(Training.tri.df$prior1)
Training.tri.df$prior2 <- as.factor(Training.tri.df$prior2)
Training.tri.df$word <- as.factor(Training.tri.df$word)
Training.tri.df$c1 <- as.numeric(Training.tri.df$c1)

p_prior <- Training.bi.df[as.character(paste(Training.tri.df$prior1,Training.tri.df$prior2)),]$p_bigrams

Training.tri.df <- data.frame( 
    cbind(
        cbind( 
            Training.tri.df,
            p_prior
        ),
        p_trigrams = Training.tri.df$c1/Training.bi.df[paste(as.character(Training.tri.df$prior1),as.character(Training.tri.df$prior2)),]$c1
    )
)

# make sure every word has type frequency and token freqnecy
# sum( names(type2) == names(token2) )
type2 <- table(as.character(paste(Training.tri.df$prior1,Training.tri.df$prior2) ) )


# Smoothing Trimodel
LAMBA.tri <- 1 - Training.bi.df[names(type2),]$c1/(Training.bi.df[names(type2),]$c1+type2)

Training.tri.df <- data.frame( 
    cbind(
        Training.tri.df,
        p_smooth_trigrams = LAMBA.tri[as.character(paste(Training.tri.df$prior1,Training.tri.df$prior2)]*Training.tri.df$p_trigrams + (1 - LAMBA.tri[as.character(paste(Training.tri.df$prior1,Training.tri.df$prior2)])*Training.unimodel[as.character(Training.tri.df$word)]
        )
        )
        rm(token2, type2)