library(koRpus)
library(tm)
library(tau)
library(stringi)

S <-c(
    c("The guy in front of me just bought a pound of bacon, a bouquet, and a case of"),
    c("You're the reason why I smile everyday. Can you follow me please? It would mean the"),
    c("Hey sunshine, can you follow me and make me the"),
    c("Very early observations on the Bills game: Offense still struggling but the"),
    c("Go on a romantic date at the"),
    c("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"),
    c("Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some"),
    c("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"),
    c("Be grateful for the good times and keep the faith during the"),
    c("If this isn't the cutest thing you've ever seen, then you must be")
)

A <- rbind(
    c("cheese", "soda", "pretzels", "beer"),
    c("most","world","best","universe"),
    c("smelliest","saddest","happiest","bluest"),
    c("players","referees","defense","crowd"),
    c("mall","movies","grocery","beach"),
    c("motorcycle","way","horse","phone"),
    c("years","thing","weeks","time"),
    c("fingers","eyes","toes","ears"),
    c("bad","hard","worse","sad"),
    c("insensitive","asleep","insane","callous")
)


# Set up the function to solve the quiz

# Estimate the Probabilities by Unimodel
UNI.P <- function(x, y){
    x.P <- Training.unimodel.tai[ removePunctuation( unlist( strsplit( stripWhitespace( iconv(x, "UTF-8", "ASCII", "")  ), "[ |']" ) ) ) ]
    y.P <- Training.unimodel.tai[y]
    x.P[is.na(x.P)] = (1 - LAMBA)
    y.P[is.na(y.P)] = (1 - LAMBA)
    result <- sum( log(x.P) ) + log(y.P) 
    return(result)
}

for (i in 1:10){
    print( UNI.P(S[i],A[i,]) )
}

print( UNI.P(S[3],A[3,]) )

# Estimate the Probabilities by Bimodel
BI.P <- function(x, y){
    tmp <- rep(0,4)
    x.raw <- paste0(paste("si", removePunctuation( stripWhitespace( iconv(x, "UTF-8", "ASCII", "")  ) ) , " "), paste(y,"se"))
    for(i in 1:length(x.raw)){
        y.bi <- names( textcnt(x.raw[i], tolower = FALSE, method = "string", n = 2) ) 
        y.P <- Training.bi.df[y.bi,"p_smooth_bigrams"]
        y.P[is.na(y.P)] = (1 - max(LAMBA.bi))
        tmp[i] = sum(log(y.P))
    }
    return(cbind(y, round(tmp, digits = 2) ))
}

for (i in 1:10){
    print( BI.P(S[i],A[i,]) )
}

BI.P(S[1],A[1,])
