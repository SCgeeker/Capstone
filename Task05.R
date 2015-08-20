# Use text mining package
library(tm)
library(NLP)
library(openNLP)
library(tau)
library(stringi)
library(RWeka)
library(dplyr)

# set up unigram language model
unigram.freq <- data.frame(table(unigram))
unigram.freq <- unigram.freq[order(unigram.freq$Freq, decreasing = TRUE),]

docs_total <- length(Training.docs)
Vocab_Size <- sum(unigram.freq$Freq) # vocabulary size
Lamda_uni <- .05 # unknown word probability

# Estimate the probability of each unigram: Maximal Likelihood 
unigram.freq <- data.frame(unigram.freq, P.ml = unigram.freq$Freq/c(docs_total, rep(Vocab_Size,dim(unigram.freq)[1] - 1 )))

# Adjust unigram model
unigram.freq <- data.frame(unigram.freq, P.adj = ( Lamda_uni*unigram.freq$P.ml + (1 - Lamda_uni)*(1/Vocab_Size) )  )

save.image("../Capstone_Task5_ImprovedUnigramModel.RData")


# Set up bigram language model
bigram.freq <- data.frame(table(bigram))
bigram.freq <- bigram.freq[order(bigram.freq$Freq, decreasing = TRUE),]

bi_base <- matrix(unlist(strsplit(as.character(bigram.freq$bigram), " ")), ncol = 2, byrow = TRUE)  # separate bigram
bigram.table <- data.frame(bigram = bigram.freq$bigram, W1 = bi_base[,1], W0 = bi_base[,2], bi_Counts = bigram.freq$Freq)
rm(bigram.freq, bi_base)

W1_Counts <- tapply(bigram.table$bi_Counts, bigram.table$W1, sum) # Count the frequencies of W1 in isolated
bigram.table <- data.frame(bigram.table, W1_Counts = W1_Counts[bigram.table$W1], P.bi = bigram.table$bi_Counts/W1_Counts[bigram.table$W1]) # Count the raw proabilities of bigram

W1_TypeF <- table(bigram.table$W1)  # Count Type Frequency of W1
bigram.table <- data.frame(bigram.table, Lamda_bi = 1 - (W1_TypeF[bigram.table$W1]/(W1_TypeF[bigram.table$W1]+bigram.table$W1_Counts))) # Calculate Lamda for every W1

W0.P <- unigram.freq$P.ml
names(W0.P) <- unigram.freq$unigram
bigram.table <- data.frame(bigram.table, P.W0.ml = W0.P[bigram.table$W0])
rm(W0.P) # Get the estimated probability of W0

bigram.table <- with(data = bigram.table, data.frame(bigram.table, P.adj = Lamda_bi*P.bi + (1 - Lamda_bi)*P.W0.ml) ) # Calculate the smoothed probability of bigram
rm(W1_Counts, W1_TypeF)

save.image("../Capstone_Task5_ImprovedBigramModel.RData")

# Set up trigram language model
trigram.freq <- data.frame(table(trigram))
trigram.freq <- trigram.freq[order(trigram.freq$Freq, decreasing = TRUE),]

#trigram.freq <- trigram.freq[-grep("^ ",trigram.freq$trigram),] # Erase failed bigrams: initial space
#trigram.freq <- trigram.freq[-grep(" $",trigram.freq$trigram),] # Erase failed bigrams: end space

tri_base <- matrix(unlist(strsplit(as.character(trigram.freq$trigram), " ")), ncol = 3, byrow = TRUE)  # 
W1 <- paste(tri_base[,1],tri_base[,2])
trigram.table <- data.frame(trigram = trigram.freq$trigram, W1, W0 = tri_base[,3], tri_Counts = trigram.freq$Freq)
rm(W1,trigram.freq, tri_base)

W1_Counts <- tapply(trigram.table$tri_Counts, trigram.table$W1, sum) # Count the frequencies of W1 in isolated
trigram.table <- data.frame(trigram.table, W1_Counts = W1_Counts[trigram.table$W1], P.tri = trigram.table$tri_Counts/W1_Counts[trigram.table$W1]) # Count the raw proabilities of trigram

W1_TypeF <- table(trigram.table$W1)  # Count Type Frequency of W1
trigram.table <- data.frame(trigram.table, Lamda_tri = 1 - (W1_TypeF[trigram.table$W1]/(W1_TypeF[trigram.table$W1]+trigram.table$W1_Counts))) # Calculate Lamda for every W1

W0.P <- unigram.freq$P.ml
names(W0.P) <- unigram.freq$unigram
trigram.table <- data.frame(trigram.table, P.W0.ml = W0.P[trigram.table$W0])
rm(W0.P) # Get the estimated probability of W0

trigram.table <- with(data = trigram.table, data.frame(trigram.table, P.adj = Lamda_tri*P.tri + (1 - Lamda_tri)*P.W0.ml) ) # Calculate the smoothed probability of trigram
rm(trigram, W1_Counts, W1_TypeF)

save.image("../Capstone_Task5_ImprovedTrigramModel.RData")



