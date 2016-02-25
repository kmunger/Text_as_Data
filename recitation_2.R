####Recitation 1: Introduction to the course

## Examples taken wholesale from Ken Benoit's NYU Dept. of Politics short course Fall 2014
## Avaliable on his website: www.kenbenoit.net


## be sure to install the latest version from GitHub, using dev branch:
devtools::install_github("kbenoit/quanteda")
## and quantedaData
devtools::install_github("kbenoit/quantedaData")

library(quanteda)


## demonstrate Heap's law

##      M = kT^b

# M = vocab size
# T = number of tokens
# k, b are constants

tokens <- tokenize(inaugCorpus, removePunct=TRUE) 
Tee <- lengths(tokens)
Tee <- sum(Tee)

mydfm <- dfm(inaugCorpus)
### nooooo
M <- length(mydfm@Dimnames$features)
## yes
M <- nfeature(mydfm)
# Let's check using parameter values from MRS for a corpus with more than 100,000 tokens

k <- 44
b <- .49

k * (Tee)^b


##Let's think about why










inaugCorpus[1]

inaugCorpus[57]

## demonstrate Zipf's law

mydfm <- dfm(inaugCorpus)
plot(log10(1:100), log10(topfeatures(mydfm, 100)),
     xlab="log10(rank)", ylab="log10(frequency)", main="Top 100 Words")
# regression to check if slope is approx -1.0
regression <- lm(log10(topfeatures(mydfm, 100)) ~ log10(1:100))
abline(regression, col="red")
confint(regression)

##stopwords--do they matter?

mydfm <- dfm(inaugCorpus, ignoredFeatures=stopwords("english"))
plot(log10(1:100), log10(topfeatures(mydfm, 100)),
     xlab="log10(rank)", ylab="log10(frequency)", main="Top 100 Words")
# regression to check if slope is approx -1.0
regression <- lm(log10(topfeatures(mydfm, 100)) ~ log10(1:100))
abline(regression, col="red")
confint(regression)

## let's look at co-locations 

last_speech_text <- inaugCorpus[57]

collocations(last_speech_text)

collocations(last_speech_text, size=3)


## remove any collocations containing a word in the stoplist
colloc <- collocations(last_speech_text)

# isStopwordList <- lapply(colloc$word1, `%in%`, stopwords("english"))
# stopwordindex <- which(colloc$word1 %in% stopwords("english")| colloc$word2 %in% stopwords("english"))
# colloc[-stopwordindex]  # collocations not containing stopwords
removeFeatures(colloc, stopwords("english"))


##that's not all we can do with collocations

##Key Words In Context (KWIC) is a good way to summarize info about a topic

kwic(inaugCorpus, "terror", 3)


##any other terms y'all think are interesting?

kwic(inaugCorpus, "angels", 3)

kwic(inaugCorpus, "slavery", 3)

##Comparing two texts
#   this helps illustrate the value of the vector representation

# cosine similarity--take the dot product of two vectors

# x * y = |x||y|cos
# cos = x*y/|x||y|

x <- c(1,2,3)
y <- c(1,2,3)

##define the norm
norm_vec <- function(x) sqrt(sum(x^2))


x %*% y / (norm_vec(x)*norm_vec(y))


#what if they're different
a <- c(1,2,3)
b <- c(1,2,4000)

a %*% b / (norm_vec(a)*norm_vec(b))


##let's do it with texts
last_speech_text <- inaugCorpus[ndoc(inaugCorpus)]
first_speech_text <- inaugCorpus[1]

##make a dfm of these two
inaug_dfm <- dfm(c(last_speech_text, first_speech_text), ignoredFeatures = stopwords("english"), stem = TRUE)

#calculate similarity
tmp <- similarity(inaug_dfm, margin = "documents")

as.matrix(tmp)

##Let's see how stopwords/stemming affect this
inaug_dfm <- dfm(c(last_speech_text, first_speech_text))

#calculate similarity
tmp <- similarity(inaug_dfm, margin = "documents")

as.matrix(tmp)




##make a dfm of a bunch
inaug_dfm <- dfm(subset(inaugCorpus , Year > 1980),ignoredFeatures = stopwords("english"), stem = TRUE)

#calculate similarity
tmp <- similarity(inaug_dfm, margin = "documents")

as.matrix(tmp)


##specific comparisons
similarity(inaug_dfm, "2009-Obama", n = 5, margin = "documents")



