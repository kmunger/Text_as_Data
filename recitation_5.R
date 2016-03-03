####Recitation 5

## Some Examples taken wholesale from Ken Benoit's NYU Dept. of Politics short course Fall 2014
## Avaliable on his website: www.kenbenoit.net

##load data

library(quanteda)
library(quantedaData)



##Naie Bayes example

##Replication of 13.1 from IIR textbook

trainingset <- matrix(0,ncol=6,nrow=5)
trainingset[1,] <- c(1, 2, 0, 0, 0, 0)
trainingset[2,] <- c(0, 2, 0, 0, 1, 0)
trainingset[3,] <- c(0, 1, 0, 1, 0, 0)
trainingset[4,] <- c(0, 1, 1, 0, 0, 1)
trainingset[5,] <- c(0, 3, 1, 0, 0, 1)
colnames(trainingset) <- c("Beijing", "Chinese",  "Japan", "Macao", "Shanghai", "Tokyo")
rownames(trainingset) <- paste("d", 1:5, sep="")
trainingset <- as.dfm(trainingset)
trainingclass <- factor(c("Y", "Y", "Y", "N", NA), ordered=TRUE)


### replicate IIR p261 prediction for test set (document 5)
nb.p261 <- textmodel_NB(x=trainingset, y=trainingclass, data=NULL,
                      smooth=1, prior="docfreq")


pr.p261 <- predict(nb.p261)
pr.p261





##read in conservative and labour manifestos

setwd("C:/Users/k/Documents/GitHub/Text_as_Data/cons_lab/")

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


test_speech<-man_df[46,]

training<-man_df[1:45,]

##do data frame

lab_con_dfm<-dfm(training$text)

test_dfm<-dfm(test_speech$text)

ws_base<-textmodel(lab_con_dfm, y=2*(as.numeric(training$party)-1)-1,
                   model="wordscores")
##look at strongest features
lab_features<-sort(ws_base@Sw, decreasing=TRUE)

lab_features[1:10]

con_features<-sort(ws_base@Sw, decreasing=FALSE)

con_features[1:10]

ws_base@Sw[c("drugs", "minorities", "unemployment")]

##try it with smoothing
ws_smooth<-textmodel(lab_con_dfm, y=2*(as.numeric(training$party)-1)-1,
                   model="wordscores", smooth=1)

ws_smooth@Sw[c("drugs", "minorities", "unemployment")]

plot(ws_base@Sw, ws_smooth@Sw, xlim=c(-1, 1),
     xlab="No Smooth", ylab="Smooth")



## predict that last speech
predict(ws_base, newdata = test_dfm,
        rescaling = "none", level = 0.95, verbose = TRUE)


predict(ws_base, newdata = test_dfm,
        rescaling = "lbg", level = 0.95, verbose = TRUE)

### Amicus texts from Evans et al
###
require(quantedaData)
data(amicusCorpus)
summary(amicusCorpus)
amDfm <- dfm(amicusCorpus)


amNBmodel <- textmodel(amDfm, docvars(amicusCorpus, "trainclass"), model="NB", smooth=1) 
print(amNBmodel, 10)
(amNBpredict <- predict(amNBmodel))
# "confusion matrix"
table(amNBpredict$docs$predicted, docvars(amicusCorpus, "testclass"))

reference <- c(1, 1, -1, -1, rep(NA, 98))
amWSmodel <- textmodel(amDfm, reference, model="wordscores", smooth=1)
plot(amWSmodel@Sw, c(1, -1) %*% amNBmodel$PcGw, xlab="Wordscore", ylab="Linear Posterior Class Pr. Diff")
(amWSpredict <- predict(amWSmodel))
# "confusion matrix"
table(amNBpredict$docs$ws.predicted, docvars(amicusCorpus, "testclass"))



## plot differences between wordscore and NB class prediction

plot(jitter(amNBpredict$word$wordscore.word,20), 
     jitter(amNBpredict$word$bayesscore.word,20),
     pch=19, cex=.6, main="(a) Word level", col="grey70",
     xlab="Wordscores", ylab="NB")
plot(jitter(amNBpredict$docs$wordscore.doc[-c(1,2)],20), 
     jitter(amNBpredict$docs$bayesscore.doc[-c(1,2)],20),
     pch=19, cex=.6, main="(b) Document level",
     col=ifelse(docvars(amicusCorpus, "testclass")=="AP", "blue", "red"),
     xlab="Wordscores", ylab="NB")
abline(v=0, lty="dashed", col="grey80")
abline(h=0, lty="dashed", col="grey80")

