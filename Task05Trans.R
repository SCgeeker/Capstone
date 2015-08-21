# Use text mining package
library(tm)
library(NLP)
library(openNLP)
library(tau)
library(stringi)
library(RWeka)
library(dplyr)

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
    sentences <- paste(paste("s0", text[sentence.boundaries]), "s9")
    sentences <- iconv(sentences, "UTF-8", "ASCII", "")
    sentences <- removePunctuation(sentences)
    sentences <- stripWhitespace(sentences)
    
    # return sentences
    return(sentences)
}


# import the target text in the following line
Test.text <- convert_text_to_sentences(blogs.docs[blogs.test.seq])
Test.unigram <- unlist( stri_split_fixed(Test.text, " ") )
Test.uni.Type <- nlevels(as.factor(Test.unigram))

Test.bigram <- paste(Test.unigram[-length(Test.unigram)], Test.unigram[-1])
Test.bigram <- Test.bigram[-grep("s9 s0", Test.bigram)]
Test.bi.Type <- nlevels(as.factor(Test.bigram))

Test.trigram <- paste(paste(Test.unigram[1:(length(Test.unigram) - 2)], Test.unigram[2:(length(Test.unigram) - 1)]), Test.unigram[-c(1,2)])
Test.trigram <- Test.trigram[-grep("^s9 s0", Test.trigram)]
Test.trigram <- Test.trigram[-grep("s9 s0$", Test.trigram)]
#Test.trigram <- Test.trigram[-intersect( grep("^s0 ", Test.trigram), grep(" s9$", Test.trigram) ) ]
Test.tri.Type <- nlevels(as.factor(Test.trigram))


ptm <- proc.time()
Test.uni.H <- -1/Test.uni.Type * sum(log(unigram.model[Test.unigram]))
Test.uni.PP <- 2^Test.uni.H
proc.time() - ptm

ptm <- proc.time()
Test.bi.H <- -1/Test.bi.Type * sum(log(bigram.model[Test.bigram]))
Test.bi.PP <- 2^Test.bi.H
proc.time() - ptm

ptm <- proc.time()
Test.tri.H <- -1/Test.tri.Type * sum(log(trigram.model[Test.trigram]))
Test.tri.PP <- 2^Test.tri.H
proc.time() - ptm
