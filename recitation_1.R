####Recitation 1: Introduction to the course

## Examples taken wholesale from Ken Benoit's NYU Dept. of Politics short course Fall 2014
## Avaliable on his website: www.kenbenoit.net


## be sure to install the latest version from GitHub, using dev branch:
devtools::install_github("kbenoit/quanteda")
## and quantedaData
devtools::install_github("kbenoit/quantedaData")

library(quanteda)

##start with a short document
sampletxt <- "The police with their policing strategy instituted a policy of general 
iterations at the Data Science Institute."

#Let's tokenize
tokens <- tokenize(sampletxt)
?tokenize

tokens <- tokenize(sampletxt, removePunct=TRUE)


## stemming examples

stems <- wordstem(tokens)
?wordstem

# vector representations

data("SOTUCorpus", package = "quantedaData")


last_speech_text <- SOTUCorpus[ndoc(SOTUCorpus)]
# same as 
last_speech_text <- texts(SOTUCorpus)[ndoc(SOTUCorpus)]

obama_dfm <- dfm(last_speech_text)
?dfm

##stopwords example

stopwords("english")

obama_dfm <- dfm(last_speech_text, ignoredFeatures = stopwords("english"))


###full matrix of all speeches

full_dfm <- dfm(SOTUCorpus, ignoredFeatures = stopwords("english"))

topfeatures(full_dfm)

plot(full_dfm, min.freq = 30)

##weighting--tfidf


weighted_dfm <- tfidf(full_dfm)


topfeatures(weighted_dfm)


##Think about what this means!



normalized <- tfidf(full_dfm, normalize=TRUE)

topfeatures(normalized)

## constructing bigrams
collocations(last_speech_text)



collocations(last_speech_text, size=3)

## detect collocations in overall

colloc <- collocations(last_speech_text)


## remove any collocations containing a word in the stoplist
# isStopwordList <- lapply(colloc$word1, `%in%`, stopwords("english"))
# stopwordindex <- which(colloc$word1 %in% stopwords("english")| colloc$word2 %in% stopwords("english"))
# colloc[-stopwordindex]  # collocations not containing stopwords
removeFeatures(colloc, stopwords("english"))


##now let's take a little look at regular expressions

# regular expressions are a very powerful tool in wrangling text
# not a focus of this class, but something to be aware of

?regex

s_index <- grep(" s ", texts(SOTUCorpus))

?grep

# this returns every speech that contains " s "
grep(" s ", texts(SOTUCorpus), value=TRUE)


s_index <- grep(" s ", texts(SOTUCorpus))



no_s <- gsub(" s ", "",  SOTUCorpus[s_index])




