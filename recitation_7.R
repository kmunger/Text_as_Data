####Recitation 7 (the 8th one, sorry)


##let's look at a few more unsupervised approaches

##Latent Semantic Analysis
install.packages("lsa")


library(quanteda)
library(quantedaData)
library(lsa)
data("SOTUCorpus")

##use the real example from class

SOTU_dfm<-dfm(SOTUCorpus[145:223,], stem = T, removePunct=T, ignoredFeatures = stopwords("english"))

SOTU_dfm@Dimnames$docs


##check to see what a good number of dimensions is


SOTU_dfm_lsa_svd<-svd(SOTU_dfm_lsa)$d


dimcalc_share()(SOTU_dfm_lsa_svd)

plot(SOTU_dfm_lsa_svd)


##default is .5 ; let's try .9

dimcalc_share(share=0.9)(SOTU_dfm_lsa_svd)

##seems kinda silly




##class example uses 5

lsa_fit<-lsa(t(SOTU_dfm_lsa), 5 )

myNewMatrix = t(as.textmatrix(lsa_fit) )


##compare features
SOTU_dfm@Dimnames$docs[9]

topfeatures(SOTU_dfm[9,])

sort(myNewMatrix[9,], decreasing=T)[1:10]



SOTU_dfm@Dimnames$docs[55]

topfeatures(SOTU_dfm[55,])

sort(myNewMatrix[55,], decreasing=T)[1:10]


SOTU_dfm@Dimnames$docs[72]

topfeatures(SOTU_dfm[72,])

sort(myNewMatrix[55,], decreasing=T)[1:10]

##see how it does at finding similar terms


lsa_fit<-lsa(t(SOTU_dfm), 3 )


myNewMatrix = as.textmatrix(lsa_fit) 


china<-associate(myNewMatrix, "china", "cosine", threshold = .7)

china[1:10]


oil<-associate(myNewMatrix, "oil", "cosine", threshold = .7)

oil[1:10]

america<-associate(myNewMatrix, "america", "cosine", threshold = .7)

america[1:10]

health<-associate(myNewMatrix, "health", "cosine", threshold = .7)

health[1:10]


##sort of similar to word2vec, but without taking account of word order




##let's talk about: WORDFISH

## how is it different from other approaches we've used for scaling?


##read in conservative and labour manifestos

setwd("C:/Users/kevin/Documents/GitHub/Text_as_Data/cons_lab/")

##read in the files
files <- list.files( full.names=TRUE)
text <- lapply(files, readLines)
text<-unlist(lapply(text, function(x) paste(x, collapse = " ")))


#name data
files<-unlist(files)
files<-gsub("./", "", files )
files<-gsub(".txt", "", files )

##create metadata

year<-unlist(strsplit(files, "[^0-9]+"))

year<-year[year!=""]

party<-unlist(strsplit(files, "[^A-z]+"))

party<-party[party!="a" & party!="b"]

#create data frame
man_df<-data.frame(year = as.numeric(year),
                   party = party,
                   stringsAsFactors = TRUE)
man_df$text<-text


lab_con_dfm<-dfm(man_df$text, stem=T, ignoredFeatures=stopwords("english"))



##fit wordfish

##setting the anchor on parties

df_fit<-textmodel_wordfish(lab_con_dfm, c(1,24))


?textmodel

plot(year[1:23], df_fit@theta[1:23])

points(year[24:46], df_fit@theta[24:46], pch=8)
?plot

plot(as.factor(party), df_fit@theta)


##most important features--word fixed effects


words<-df_fit@psi
names(words) <- df_fit@features

sort(words)[1:50]

sort(words, decreasing=T)[1:50]

##guitar plot


weights<-df_fit@beta

plot(weights, words)



