####Recitation 4

## Some Examples taken wholesale from Ken Benoit's NYU Dept. of Politics short course Fall 2014
## Avaliable on his website: www.kenbenoit.net

##load data

library(quanteda)
library(quantedaData)
##read in conservative manifestos

setwd("C:/Users/kevin/Documents/GitHub/Text_as_Data/cons")

##read in the files
files <- list.files( full.names=TRUE)
text <- lapply(files, readLines)
text<-unlist(lapply(text, function(x) paste(x, collapse = " ")))


#name data
files<-unlist(files)
files<-gsub("./Con", "", files )
files<-gsub(".txt", "", files )

#don't use this exact line--think about how it's different
man_df<-data.frame(year = as.numeric(files), text = text , stringsAsFactors = FALSE)




## basic regular expression function demos
grep("^vot", c("voting", "votes", "devoted", "vote"))
grepl("^vot", c("voting", "votes", "devoted", "vote"))
## replace patterns
gsub("(\\w)-(\\d{2,2})", "\\1-19\\2", c("Roosevelt-33", "Roosevelt-37", "Obama-2003"))
# observe the difference the $ makes
gsub("(\\w)-(\\d{2,2})$", "\\1-19\\2", c("Roosevelt-33", "Roosevelt-37", "Obama-2003"))





##Ã using dfm to keep only certain terms
## keep only certain words
testText <- "The quick brown fox named Seamus jumps over the lazy dog also named Seamus, with
the newspaper from a a boy named Seamus, in his mouth."
print(dfm(testCorpus, keptFeatures="s$"), TRUE)  # keep only words ending in "s"
testTweets <- c("My homie @justinbieber #justinbieber getting his shopping on in #LA yesterday #beliebers",
                "To all the haters including my brother #justinbieber #justinbiebermeetcrystaltalley #emabiggestfansjustinbieber",
                "Justin Bieber #justinbieber #belieber #kidrauhl #fetusjustin #EMABiggestFansJustinBieber")
print(dfm(testTweets, keptFeatures="^#"), TRUE)  # keep only hashtags


## similar keep for dfms
library(quantedaData)
data(ie2010Corpus)
ieDfm <- dfm(ie2010Corpus, keptFeatures=c("tax|budg|^auster"))

View(ieDfm)

## compound words
mytexts <- c("The new law included a capital gains tax, and an inheritance tax.",
             "New York City has raised a taxes: an income tax and a sales tax.")
mydict <- list(tax=c("tax", "income tax", "capital gains tax", "inheritance tax"))
print(dfm(mytexts, dictionary=mydict), show.values=TRUE)



## Laver Garry dictionary

setwd("C:/Users/kevin/Documents/GitHub/Text_as_Data")

lgdict <- dictionary(file = "LaverGarry.cat", format = "wordstat")

head(dfm(man_df$text, dictionary=lgdict))

dic_dfm<-dfm(df$text, dictionary=lgdict)

features(dic_dfm)

##plot it

plot(man_df$year, 
     dic_dfm[,"CULTURE.SPORT"],
     xlab="Year", ylab="SPORTS", type="b", pch=19)

plot(man_df$year, 
     dic_dfm[,"VALUES.CONSERVATIVE"],
     xlab="Year", ylab="Conservative values", type="b", pch=19)


plot(man_df$year, 
     dic_dfm[,"INSTITUTIONS.CONSERVATIVE"] - dic_dfm[,"INSTITUTIONS.RADICAL"],
     xlab="Year", ylab="Net Conservative Institutions", type="b", pch=19)





##RID Dictionary--Regressive Imagery Dictionary

setwd("C:/Users/kevin/Documents/GitHub/Text_as_Data")

rid_dict <- dictionary(file = "RID.cat", format = "wordstat")

data("SOTUCorpus")

sotus <- texts(SOTUCorpus)

year<-(SOTUCorpus$documents$Date)

pres<-(SOTUCorpus$documents$President)


head(dfm(sotus, dictionary=rid_dict))

dic_dfm<-dfm(sotus, dictionary=rid_dict)

features(dic_dfm)

plot(year, 
     dic_dfm[,"PRIMARY.REGR_KNOL.NARCISSISM"],
     xlab="Year", ylab="Narcissism", type="b", pch=19)

plot(year, 
     dic_dfm[,"EMOTIONS.ANXIETY._"],
     xlab="Year", ylab="Anxiety", type="b", pch=19)

plot(year, 
     dic_dfm[,"EMOTIONS.AGGRESSION._"],
     xlab="Year", ylab="Aggression", type="b", pch=19)

plot(year, 
     dic_dfm[,"PRIMARY.ICARIAN_IM.FIRE"] + dic_dfm[,"PRIMARY.ICARIAN_IM.ASCEND"] +dic_dfm[,"PRIMARY.ICARIAN_IM.DESCENT"] +
       dic_dfm[,"PRIMARY.ICARIAN_IM.DEPTH"] + dic_dfm[,"PRIMARY.ICARIAN_IM.HEIGHT"] + dic_dfm[,"PRIMARY.ICARIAN_IM.WATER"],
     xlab="Year", ylab="Icarian-ness", type="b", pch=19)

