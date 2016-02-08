####Recitation 1: Introduction to the course

## Examples taken wholesale from Ken Benoit's NYU Dept. of Politics short course Fall 2014
## Avaliable on his website: www.kenbenoit.net


## be sure to install the latest version from GitHub, using dev branch:
devtools::install_github("quanteda", username="kbenoit", dependencies=TRUE, ref="dev")
## and quantedaData
devtools::install_github("quantedaData", username="kbenoit")

library(quanteda)
library(quantedaData)
##start with a short document
sampletxt <- "The police with their policing strategy instituted a policy of general 
iterations at the Data Science Institute."

#Let's tokenize
tokens<-tokenize(sampletxt)
?tokenize

tokens<-tokenize(sampletxt, removePunct=TRUE)


## stemming examples

stems<-wordstem(tokens)
?wordstem

# vector representations

data("SOTUCorpus")

speeches<-data.frame(SOTUCorpus$documents)

last_speech_text<-speeches$texts[230]

obama_dfm<-dfm(last_speech_text)
?dfm

##stopwords example

stopwords("english")

obama_dfm<-dfm(last_speech_text, ignoredFeatures = stopwords("english"))


###full matrix of all speeches

full_dfm<-dfm(speeches$texts, ignoredFeatures = stopwords("english"))

topfeatures(full_dfm)

plot(full_dfm)

##weighting--tfidf


weighted<-tfidf(full_dfm)


topfeatures(weighted)


##Think about what this means!



normalized<-tfidf(full_dfm, normalize=TRUE)

topfeatures(normalized)

## constructing bigrams
collocations(last_speech_text)



collocations(last_speech_text, size=3)

## detect collocations in overall

colloc <- collocations(last_speech_text)


## remove any collocations containing a word in the stoplist
isStopwordList <- lapply(colloc$word1, `%in%`, stopwords("english"))
stopwordindex <- which(colloc$word1 %in% stopwords("english")| colloc$word2 %in% stopwords("english"))
colloc[-stopwordindex]  # collocations not containing stopwords


##now let's take a little look at regular expressions

# regular expressions are a very powerful tool in wrangling text
# not a focus of this class, but something to be aware of

?regex

s_index<-grep(" s ", speeches$texts )

?grep

grep(" s ", speeches$texts, value=TRUE )


s_index<-grep(" s ", speeches$texts )



no_s<-gsub(" s ", "",  speeches$texts[s_index] )




