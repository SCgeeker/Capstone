con1 <- unz("../capstone_data/Swiftkey.zip","final/en_US/en_US.blogs.txt")
con2 <- unz("../capstone_data/Swiftkey.zip","final/en_US/en_US.news.txt")
con3 <- unz("../capstone_data/Swiftkey.zip","final/en_US/en_US.twitter.txt")

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

blogs.test.pool <-  1:length(blogs.docs)[-blogs.seq]
news.test.pool <-  1:length(news.docs)[-news.seq]
twitter.test.pool <-  1:length(twitter.docs)[-twitter.seq]


Training.text <- c(blogs.docs[blogs.seq], news.docs[news.seq], twitter.docs[twitter.seq])

{
set.seed(12345)
blogs.test.seq <- sample(blogs.test.pool, 100)
news.test.seq <- sample(news.test.pool, 100)
twitter.test.seq <- sample(twitter.test.pool, 100)
}
