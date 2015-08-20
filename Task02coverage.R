if(!file.exists("data")) {
    dir.create("data")
}

## SEMiSUSANNE Corpus
## The SEMiSUSANNE Corpus was developed in 2006 by Christopher Powell of the Ashmolean Museum, Oxford University, who has kindly permitted me to distribute it from my site. SEMiSUSANNE supplements the grammatical annotations of SUSANNE with semantic annotations identifying the WordNet senses in which vocabulary items are used. It covers 33 of the 64 SUSANNE texts.

fileUrl1 <- "http://www.grsampson.net/SEMiSUSANNE.zip"
download.file(fileUrl1, destfile = "./data/SEMiSUSANNE.zip")

## CHRISTINE Corpus, Release 2
## The second release of CHRISTINE, which became available in August 2000, incorporates a minor change in the distribution of analytic information between the fields, to make it more compatible with SUSANNE and easier to read.

fileUrl2 <- "http://www.grsampson.net/CHRISTINE1.tgz"
download.file(fileUrl2, destfile = "./data/CHRISTINE1.tgz")

## LUCY Corpus, Release 2
## Release 2 of the LUCY Corpus, circulated in December 2005, corrects a number of errors in the initial release of 2003.

fileUrl3 <- "http://www.grsampson.net/LUCYrf.tgz"
download.file(fileUrl3, destfile = "./data/LUCYrf.tgz")

unzip("./data/SEMiSUSANNE.zip", list = TRUE)$Name
untar("./data/CHRISTINE1.tgz",list=TRUE) 
untar("./data/LUCYrf.tgz",list=TRUE)

con1 <- unz("./data/SEMiSUSANNE.zip")