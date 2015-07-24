load("../Capstone_sample_ngrams.RData")

library(ngram)

# Test1 <- list()
# for(i in 1:1000){
#     Test1 <- cbind(Test1,  sapply(US_docs[i], ngram, n = 1) )
# }
# get.ngrams(Test1[[1]])
# 
# Test2 <- list()
# for(i in 1:1000){
#     Test2 <- cbind(Test2,  sapply(US_docs[i], ngram, n = 2) )
# }
# get.ngrams(Test2[[1]])
# 
# Test3 <- list()
# for(i in 1:1000){
#     Test3 <- cbind(Test3,  sapply(US_docs[i], ngram, n = 3) )
# }
# get.ngrams(Test3[[1]])

US_short <- list()
for(i in 1:length(US_short_docs)){
    US_short <- cbind(US_short,  sapply(US_short_docs[i], ngram, n = 1) )
}