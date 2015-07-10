file.temp <- "./data/Swiftkey.zip"
    
unzip(file.temp)

# Quiz 1: Getting Started
# Question 1
file.info(paste0("./final/",dir("./final/", recursive = TRUE)) )[1]/(1024^2)
## Git Bash: Do `ls -alh` in the `Coursera-Swiftkey/final/en_US directory`.

# Quiz 1: Getting Started
# Question 2
#con1 <- file( paste0("./final/",dir("./final/", recursive = TRUE))[4], 'r' ) # blogs
con1 <- unz("./data/Swiftkey.zip","final/en_US/en_US.blogs.txt")
length(readLines(con1))
#con2 <- file( paste0("./final/",dir("./final/", recursive = TRUE))[5], 'r' ) # news
con2 <- unz("./data/Swiftkey.zip","final/en_US/en_US.news.txt")
length(readLines(con2))
#con3 <- file( paste0("./final/",dir("./final/", recursive = TRUE))[6], 'r' ) # twitter
con3 <- unz("./data/Swiftkey.zip","final/en_US/en_US.twitter.txt")
length(readLines(con3))
## Do wc -l en_US.twitter.txt at the prompt (or git bash on windows) or length(readLines("en_US.twitter.txt")) in R

# Quiz 1: Getting Started
# Question 3
#max( nchar(line1) )
#max( nchar(line2) )
#max( nchar(line3) )
max( nchar(readLines(con1) ) )
max( nchar(readLines(con2) ) )
max( nchar(readLines(con3) ) )
## Again a simple wc command suffices wc -L *.txt inthe directory with the three files. Note, we had a small discrepancy between doing thin in R versus WC.

# Quiz 1: Getting Started
# Question 4
length(grep("love", readLines(con3)))/length(grep("hate", readLines(con3)))
## grep "love" en_US.twitter | wc -l and grep "hate" en_US.twitter | wc -l gives you the counts. Then you could divide in whatever. If you never want to leave the console, you can use bc (not present on gitbash in windows). You could also read into R (readLines) and use character search. This worked on gitbash love=$(grep "love" en_US.twitter.txt | wc -l) then hate=$(grep "hate" en_US.twitter.txt | wc -l) then let m=love/hate then echo $m

# Quiz 1: Getting Started
# Question 5
line3[grep("biostats", readLines(con3))]
## grep -i "biostat" en_US.twitter.txt (note the -i doesn't matter since there's only one line ignoring case).

# Quiz 1: Getting Started
# Question 6
grep( "A computer once beat me at chess, but it was no match for me at kickboxing", readLines(con3))
## grep -x "A computer once beat me at chess, but it was no match for me at kickboxing" en_US.twitter.txt | wc -l

close(con1,con2,con3)
rm(list = ls())
# file.remove( paste0("./final/",dir("./final/", recursive = TRUE)) )
