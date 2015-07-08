file.temp <- "./data/Swiftkey.zip"
    
unzip(file.temp)

# Quiz 1: Getting Started
# Question 1
file.info(paste0("./final/",dir("./final/", recursive = TRUE)) )[1]/(1024^2)


# Quiz 1: Getting Started
# Question 2
con1 <- file( paste0("./final/",dir("./final/", recursive = TRUE))[4], 'r' ) # blogs
line1 = readLines(con1)
con2 <- file( paste0("./final/",dir("./final/", recursive = TRUE))[5], 'r' ) # news
line2 = readLines(con2)
con3 <- file( paste0("./final/",dir("./final/", recursive = TRUE))[6], 'r' ) # twitter
line3 = readLines(con3)
length(line3)
close(con1,con2,con3)

# Quiz 1: Getting Started
# Question 3
max( nchar(line1) )
max( nchar(line2) )
max( nchar(line3) )

# Quiz 1: Getting Started
# Question 4
length(grep("love", line3))/length(grep("hate", line3))

# Quiz 1: Getting Started
# Question 5
line3[grep("biostats", line3)]


# Quiz 1: Getting Started
# Question 5
grep( "A computer once beat me at chess, but it was no match for me at kickboxing", line3)

rm(list = ls())
file.remove( paste0("./final/",dir("./final/", recursive = TRUE)) )
