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
Training0001.docs <- convert_text_to_sentences(Training.doc[1:30000])
Training0002.docs <- convert_text_to_sentences(Training.doc[30001:60000])
Training0003.docs <- convert_text_to_sentences(Training.doc[60001:90000])
Training0004.docs <- convert_text_to_sentences(Training.doc[90001:120000])
Training0005.docs <- convert_text_to_sentences(Training.doc[120001:150000])
Training0006.docs <- convert_text_to_sentences(Training.doc[150001:180000])
Training0007.docs <- convert_text_to_sentences(Training.doc[180001:210000])
Training0008.docs <- convert_text_to_sentences(Training.doc[210001:240000])
Training0009.docs <- convert_text_to_sentences(Training.doc[240001:270000])
Training0010.docs <- convert_text_to_sentences(Training.doc[270001:300000])
Training0011.docs <- convert_text_to_sentences(Training.doc[300001:330000])
Training0012.docs <- convert_text_to_sentences(Training.doc[330001:360000])
Training0013.docs <- convert_text_to_sentences(Training.doc[360001:390000])
Training0014.docs <- convert_text_to_sentences(Training.doc[390001:420000])
Training0015.docs <- convert_text_to_sentences(Training.doc[420001:426966])

Training.docs <- c(Training0001.docs, Training0002.docs, Training0003.docs, Training0004.docs, Training0005.docs,
                   Training0006.docs, Training0007.docs, Training0008.docs, Training0009.docs, Training0010.docs,
                   Training0011.docs, Training0012.docs, Training0013.docs, Training0014.docs, Training0015.docs)
rm(Training0001.docs, Training0002.docs, Training0003.docs, Training0004.docs, Training0005.docs,
   Training0006.docs, Training0007.docs, Training0008.docs, Training0009.docs, Training0010.docs,
   Training0011.docs, Training0012.docs, Training0013.docs, Training0014.docs, Training0015.docs)

save.image("../Capstone_Task4_ProcessingRawDocs.RData")

Training.docs <- paste(paste("s0", Training.docs), "s9")

# Build Corpus
Training.Corpus <- VCorpus(VectorSource(Training.docs))

#rm(Training.doc)

save.image("../Capstone_Task4_ProcessingRawCorpus.RData")

# Clean Corpus
# I refined the cleaning process based the two reports:
# http://rpubs.com/haim_schneider/95081
# http://rpubs.com/edlewis4/95047

# import badwords.txt
#profanity <- readLines("badwords.txt")

rm(Training.doc)
## Reserve the upper letters, numbers, and stop words. Because the language models will have the best predictions.
#TrainingCorpus <- tm_map(TrainingCorpus, tolower)
Training.Corpus <- tm_map(Training.Corpus, iconv, "UTF-8", "ASCII", "")
Training.Corpus <- tm_map(Training.Corpus, removePunctuation)
Training.Corpus <- tm_map(Training.Corpus, stripWhitespace)
#TrainingCorpus <- tm_map(TrainingCorpus, removeNumbers)                    
#TrainingCorpus <- tm_map(TrainingCorpus, removeWords,c(stopwords("english"),"the","you","and","for","that","with","your","have","be","this","are","can","but","what"))
#TrainingCorpus <- tm_map(TrainingCorpus, removeWords, profanity)

save.image("../Capstone_Task4_ProcessingPreNgrams.RData")


# Tokenization
docs_l <- length(Training.docs)
unigram <- NULL
i = 1  # stop at 393264
while(i <= docs_l){
    unigram <- c(unigram, NGramTokenizer(Training.Corpus[[i]], Weka_control(min = 1, max = 1)) )
    i = i+1
    print(i)
}
unigram.freq <- data.frame(table(unigram))
unigram.freq <- unigram.freq[order(unigram.freq$Freq, decreasing = TRUE),]

save.image("../Capstone_Task4_ProcessingNgrams.RData")

 bigram <- NULL
i = 1
while(i <= docs_l){
    bigram <- c(bigram, NGramTokenizer(Training.Corpus[[i]], Weka_control(min = 2, max = 2)) )
    i = i+1
    print(i)
}
bigram.freq <- data.frame(table(bigram))
bigram.freq <- bigram.freq[order(bigram.freq$Freq, decreasing = TRUE),]

save.image("../Capstone_Task4_ProcessingNgrams.RData")

trigram <- NULL
i = 1
while(i <= docs_l){
    trigram <- c(trigram, NGramTokenizer(Training.Corpus[[i]], Weka_control(min = 3, max = 3)) )
    i = i+1
    print(i)
}
trigram.freq <- data.frame(table(trigram))
trigram.freq <- trigram.freq[order(trigram.freq$Freq, decreasing = TRUE),]

save.image("../Capstone_Task4_ProcessingNgrams.RData")


## Prepare raw tri-gram language model
trigram.freq <- data.frame(trigram.freq, pre_bigram = substr(trigram.freq$trigram, 1,unlist(stri_locate_all_words(trigram.freq$trigram))[seq(5, length(trigram.freq$trigram)*6, by = 6)]) )
##pre_bigram2 <- substr(trigram.freq$trigram, 1,unlist(stri_locate_all_words(trigram.freq$trigram))[seq(5, length(trigram.freq$trigram)*6, by = 6)])
pre_bigram.freq <- data.frame(table(trigram.freq$pre_bigram))
pre_bigram.freq <- pre_bigram.freq[order(pre_bigram.freq$Freq, decreasing = TRUE),]
names(pre_bigram.freq) <- c("pre_bigram", "Bi_freq")

trigram.dat <- full_join(trigram.freq, pre_bigram.freq)

trigram.dat <- data.frame(trigram.dat, Tri.P = trigram.dat$Freq/trigram.dat$Bi_freq)
trigram.dat$Tri.P[is.na(trigram.dat$Tri.P)] = 0
trigram.dat$Tri.P[trigram.dat$Tri.P > 1] = 1
