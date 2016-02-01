####Recitation 1: Introduction to the course

## Examples taken wholesale from Ken Benoit's NYU Dept. of Politics short course Fall 2014
## Avaliable on his website: www.kenbenoit.net









###Make sure you have devtools installed
install.packages("devtools")
library(devtools)
## be sure to install the latest version from GitHub, using dev branch:
devtools::install_github("quanteda", username="kbenoit", dependencies=FALSE, ref="dev")
install.packages("quanteda")
## and quantedaData
devtools::install_github("quantedaData", username="kbenoit")

library(quanteda)

## stemming examples
sampletxt <- "The police with their policing strategy instituted a policy of general 
iterations at the Data Science Institute."
wordstem(tokenize(sampletxt, simplify=TRUE))

# stopwords examples
library(quantedaData)
data(exampleString)
topfeatures(dfm(exampleString, verbose=FALSE))
topfeatures(dfm(exampleString, ignoredFeatures=stopwords("english"), verbose=FALSE))
topfeatures(dfm(exampleString, ignoredFeatures=stopwords("english"), stem=TRUE, verbose=FALSE))



## constructing bigrams
collocations(sampletxt, size = 2 )
collocations(sampletxt, size = 3 )

## detect collocations in UK party manifestos speeches
library(quantedaData)
data(ukManifestos)
colloc <- collocations(subset(ukManifestos, Year==2001))
stopwords <- sort(stopwords("english"))

## remove any collocations containing a word in the stoplist
stopwordindex <- which(colloc$word1 %in% stopwords | colloc$word2 %in% stopwords)
colloc[stopwordindex, ]   # collocations containing stopwords
colloc[-stopwordindex,]  # collocations not containing stopwords

