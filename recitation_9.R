##Recitation 9

#     Kevin Munger




###Running STM


###Make sure you have the appropriate packages installed

install.packages("quanteda")
install.packages("stm")
install.packages("ggplot2")

library(quanteda)
library(stm)
library(ggplot2)

###First, you need to go to my github and download the data

###Save the two folders to your desktop


###read in the tweets
setwd("C:/Users/kevin/Desktop")


###Get the list of files
g1 <- list.files("MA paper/govdates/", full.names=TRUE)
g2 <- list.files("MA paper/oppdates/",  full.names=TRUE)
files<-c(g1, g2)
tweets <- lapply(files, readLines)


##Combine all the tweets per day to form the documents
tweets<-lapply(tweets, function(x) paste(x, collapse=" "))
txt <- unlist(tweets)





###create covariates
team<-rep("gov", 162)
team[163:324]<-rep("opp", 162)
dates<-seq(as.Date("2013/12/18"), by="days", length=162)

days<-dates
days[163:324]<-dates

data<-data.frame(team, txt, days)

##use STM's cleaning functions


processed <- textProcessor(data$txt, metadata=data, language="spanish", stem=TRUE)


##remove some words for speed purposes
out_20 <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh=20)


##Search K; we're not going to run this now

model<-searchK(out_20$documents, out_20$vocab, K=c(25, 50, 75))
plot(model)

###Run the model with the same number of topics as before

fitSpec50 <- stm(out_20$documents,out_20$vocab,K=50, init.type="LDA", 
                    content=~team, prevalence = ~team + as.numeric(days), max.em.its=30, data=out_20$meta, seed=5926696)

fitSpec25 <- stm(out_20$documents,out_20$vocab,K=25, init.type="LDA", 
                 content=~team, prevalence = ~team + as.numeric(days), max.em.its=30, data=out_20$meta, seed=5926696)




##find biggest topics

plot.STM(fitSpec25, type="summary")

plot.STM(fitSpec50, type="summary")



big_25<-c(15)

big_50<-c(15)


###Words in those topics of interest

labelTopics(fitSpec25, big_25)

labelTopics(fitSpec50, big_50)

###Look at how content varies in these topics

##change data types
out_20$meta$team<-as.factor(out_20$meta$team)
out_20$meta$days<-as.numeric(out_20$meta$days)

##pick specifcation
prep<-estimateEffect(big_25 ~ team , fitSpec25, meta=out_20$meta)


##plot effects
plot.estimateEffect(prep, covariate="team", topics=big_25, model=out_20, method="difference",  cov.value1 = "gov", cov.value2 = "opp",
                    xlab = "More Opp......More Gov", xlim=c(-.1, .1))



##pick specifcation----50 topics
prep<-estimateEffect(big_50 ~ team , fitSpec50, meta=out_20$meta)


##plot effects
plot.estimateEffect(prep, covariate="team", topics=big_50, model=out_20, method="difference",  cov.value1 = "gov", cov.value2 = "opp",
                    xlab = "More Opp......More Gov", xlim=c(-.1, .1))





##pick specifcation--over time
prep<-estimateEffect(big_25 ~ s(days) , fitSpec25, meta=out_20$meta)


##plot effects
plot.estimateEffect(prep, covariate="days", topics=big_25, model=out_20, method="continuous")


##pick specifcation--over time--50 
prep<-estimateEffect(big_50 ~ s(days) , fitSpec50, meta=out_20$meta)


##plot effects
plot.estimateEffect(prep, covariate="days", topics=big_50, model=out_20, method="continuous")






###Let's see how the terms used vary within a topic




plot.STM(fitSpec25, type="perspectives", topics = 12)

plot.STM(fitSpec25, type="perspectives", topics = xx)

plot.STM(fitSpec25, type="perspectives", topics = xx)




plot.STM(fitSpec50, type="perspectives", topics = xx)

plot.STM(fitSpec50, type="perspectives", topics = xx)

plot.STM(fitSpec50, type="perspectives", topics = xx)
