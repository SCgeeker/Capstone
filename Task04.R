# Use text mining package
library(tm)
library(NLP)
library(openNLP)
library(tau)
library(stringi)
library(RWeka)
library(dplyr)

##options(mc.cores=1)

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
blogs.seq <- sample(1:length(blogs.docs), length(blogs.docs)*.1)
news.seq <-  sample(1:length(news.docs), length(news.docs)*.1)
twitter.seq <-  sample(1:length(twitter.docs), length(twitter.docs)*.1)
}

# docs_l <- sum(length(blogs.seq), length(news.seq), length(twitter.seq))
Training.doc <- c(blogs.docs[blogs.seq], news.docs[news.seq], twitter.docs[twitter.seq])
rm(blogs.docs, news.docs, twitter.docs, blogs.seq, news.seq, twitter.seq)

# Reshape Text
## Get below functions from http://stackoverflow.com/questions/18712878/r-break-corpus-into-sentences
convert_text_to_sentences <- function(text, lang = "en") {
    # Function to compute sentence annotations using the Apache OpenNLP Maxent sentence detector employing the default model for language 'en'. 
    sentence_token_annotator <- Maxent_Sent_Token_Annotator(language = lang)
    
    # Convert text to class String from package NLP
    text <- as.String(text)
    
    # Sentence boundaries in text
    sentence.boundaries <- annotate(text, sentence_token_annotator)
    
    # Extract sentences and mark boundries
    sentences <- paste(paste("si", text[sentence.boundaries]), "se")
    
    # return sentences
    return(sentences)
}

reshape_corpus <- function(current.text, FUN, ...) {
    # Extract the text from each document in the corpus and put into a list
    # Function 'content' in NLP recieve Corpus object and return the list.
    #text <- content(current.corpus)
    
    # Basically convert the text
    docs <- lapply(text, FUN, ...)
    docs <- as.vector(unlist(docs))
    return(docs)
    # Create a new corpus structure and return it
    # new.corpus <- Corpus(VectorSource(docs))
    # return(new.corpus)
}

# The inputed Corpus has to be the raw Corpus from the raw documents.
#Training.docs <- reshape_corpus(Training.doc[1], convert_text_to_sentences)
Training.docs <- convert_text_to_sentences(Training.doc)

# Build Corpus
TrainingCorpus <- VCorpus(VectorSource(Training.doc))

#rm(Training.doc)

save.image("../Capstone_Task4_ProcessingRawNgrams.RData")

# Clean Corpus
# I refined the cleaning process based the two reports:
# http://rpubs.com/haim_schneider/95081
# http://rpubs.com/edlewis4/95047

# import badwords.txt
#profanity <- readLines("badwords.txt")


rm(TrainingCorpus)

save.image("../Capstone_Task4_ProcessingRawNgrams.RData")

## Reserve the upper letters, numbers, and stop words. Because the language models will have the best predictions.
#TrainingCorpus <- tm_map(TrainingCorpus, tolower)
TrainingCorpus <- tm_map(TrainingCorpus, iconv, "UTF-8", "ASCII", "")
TrainingCorpus <- tm_map(TrainingCorpus, removePunctuation)
TrainingCorpus <- tm_map(TrainingCorpus, stripWhitespace)
#TrainingCorpus <- tm_map(TrainingCorpus, removeNumbers)                    
#TrainingCorpus <- tm_map(TrainingCorpus, removeWords,c(stopwords("english"),"the","you","and","for","that","with","your","have","be","this","are","can","but","what"))
#TrainingCorpus <- tm_map(TrainingCorpus, removeWords, profanity)

save.image("../Capstone_Task4_ProcessingRawNgrams.RData")


# Tokenization
unigram <- NULL
i = 1
while(i <= docs_l){
    unigram <- c(unigram, NGramTokenizer(TrainingCorpus[[i]], Weka_control(min = 1, max = 1)) )
    i = i+1
    print(i)
}
##unigram[grep("177", unigram )]
unigram.freq <- data.frame(table(unigram))
unigram.freq <- unigram.freq[order(unigram.freq$Freq, decreasing = TRUE),]

save.image("../Capstone_Task4_ProcessingRawNgrams.RData")

# set up unigram language model
docs_total <- length(Training.docs)
Vocab_Size <- sum(unigram.freq$Freq) # vocabulary size
Lamda_uni <- .05 # unknown word probability

# Estimate the probability of each unigram: Maximal Likelihood 
unigram.freq <- data.frame(unigram.freq, P.ml = unigram.freq$Freq/c(docs_total, rep(Vocab_Size,dim(unigram.freq)[1] - 1 )))

# Adjust unigram model
unigram.freq <- data.frame(unigram.freq, P.adj = ( Lamda_uni*unigram.freq$P.ml + (1 - Lamda_uni)*(1/Vocab_Size) )  )

save.image("../Capstone_Task5_ImprovedUnigramModel.RData")

## Prepare raw bi-gram language model

bigram <- paste(unigram[-length(unigram)], unigram[-1])
bigram <- bigram[-grep("s9 s0", bigram)]


save.image("../Capstone_Task4_ProcessingRawNgrams.RData")

## Prepare raw tri-gram language model

trigram <- paste(paste(unigram[1:(length(unigram) - 2)], unigram[2:(length(unigram) - 1)]), unigram[-c(1,2)])

trigram <- trigram[-grep("^s9 s0", trigram)]
trigram <- trigram[-grep("s9 s0$", trigram)]
trigram <- trigram[-intersect( grep("^s0 ", trigram), grep(" s9$", trigram) ) ]

save.image("../Capstone_Task4_ProcessingRawNgrams.RData")

trigram.freq <- data.frame(table(trigram))
trigram.freq <- trigram.freq[order(trigram.freq$Freq, decreasing = TRUE),]


rm(bigram, trigram)
save.image("../Capstone_Task4_ProcessingNgrams.RData")
