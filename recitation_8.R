####Recitation 8 (the 9th one, sorry)




###Running LDA


###Make sure you have the appropriate packages installed

install.packages("quanteda")
install.packages("topicmodels")
install.packages("ggplot2")

library(quanteda)
library(topicmodels)
library(ggplot2)

###First, you need to go to my github and download the data

###Save the two folders to your desktop


setwd("C:/Users/kevin/Desktop")


###Get the list of files
g1 <- list.files("MA paper/govdates/", full.names=TRUE)
g2 <- list.files("MA paper/oppdates/",  full.names=TRUE)
files<-c(g1, g2)



###read in the tweets
tweets <- lapply(files, readLines)


##Combine all the tweets per day to form the documents
tweets<-lapply(tweets, function(x) paste(x, collapse=" "))
txt <- unlist(tweets)



##Tokenize and clean the text

txt<-tokenize(txt, removePunct = TRUE, removeTwitter = FALSE )



##Convert to Document Feature Matrix (AKA Document Term Matrix)

mat <-dfm(txt, stem=TRUE, language = "spanish", ignoredFeatures = stopwords(kind="spanish"), toLower=T)



##Run LDA 
#
best.model <- lapply(seq(20,100, by=5), function(k){LDA(mat, k)})
best.model.logLik <- as.data.frame(as.matrix(lapply(best.model, logLik)))

best.model.logLik.df <- data.frame(topics=c(seq(20,100, by=5)), LL=as.numeric(as.matrix(best.model.logLik)))




##Set number of topics
k <-50


SEED<-2010

##Run the topic model
TM<-list(Gibbs = LDA(mat, k = k, method = "Gibbs",  control = list(seed = SEED, burnin = 3,thin = 30, iter = 30)))

##Store the results of the distribution of topics over documents
doc_topics<-TM[["Gibbs"]]@gamma


##Store the results of words over topics

words_topics<-TM[["Gibbs"]]@beta

###Look at a visualization of the topics

###transpose the data so that the days are columns


doc_topics<-t(doc_topics)

#arrange topics
max<-apply(doc_topics, 1, which.max)

##write a function that finds the second max
which.max2<-function(x){
  which(x == sort(x,partial=(k-1))[k-1])
  
}

max222<- apply(doc_topics, 1, which.max2)
max222<-sapply(max222, max)


##combine data
index<-seq(1:162)
top2<-data.frame(max, max222, index)
dates<-seq(as.Date("2013/12/18"), by="days", length=162)

gov2<-data.frame(dates, max[1:162], max222[1:162])
opp2<-data.frame(dates, max[163:324], max222[163:324])

####plot
z<-ggplot(gov2, aes(x=index, y=max.1.162., pch="First")) 

z + geom_point(aes(x=index, y=max222.1.162., pch="Second") ) +theme_bw() + ylab("Topic Number")  + ggtitle("Government")  + 
  xlab(NULL) + theme(axis.ticks = element_blank(), axis.text.x = element_blank()) + geom_point() + 
  geom_vline(xintercept=57) +
  geom_vline(xintercept=143)  +
  geom_vline(xintercept=114, linetype=2) +
  scale_shape_manual(values=c(18, 1), name = "Topic Rank") 



z<-ggplot(opp2, aes(x=index, y=max.163.324., pch="First")) 

z + geom_point(aes(x=index, y=max222.163.324., pch="Second") ) + ylab("Topic Number")+theme_bw()   + ggtitle("Opposition")  +
  xlab(NULL) + theme(axis.ticks = element_blank(), axis.text.x = element_blank()) + geom_point() + 
  geom_vline(xintercept=57) +
  geom_vline(xintercept=143)  +
  geom_vline(xintercept=114, linetype=2) +  scale_shape_manual(values=c(18, 1), name = "Topic Rank") 





###Now let's look at the words in each of these topics



num.words <- 10

normalized.topics <- exp(TM[["Gibbs"]]@beta) / rowSums(exp(TM[["Gibbs"]]@beta))
calculate.specificity <- function(mod) {
  if(!inherits(mod,"LDA") & !inherits(mod,"CTM") ) stop("mod object must inherit from LDA or CTM")
  terms <- posterior(mod)$terms
  topics <- posterior(mod)$topics
  Nwords<-ncol(terms)
  Ntopics<-ncol(topics)
  Ndocs<-nrow(topics)
  ptopic <- apply(topics,2,sum)/Ndocs
  pwords <- apply(terms,2,function(x) sum(x*ptopic))
  numer <- terms*ptopic
  denom  <- matrix(pwords,nrow=Ntopics,ncol=Nwords,byrow=TRUE)
  return(numer/denom)
}
K<-k
normalized.words <- calculate.specificity(TM[["Gibbs"]])
normalized.words <- apply(exp(TM[["Gibbs"]]@beta), 2, function(x) x/sum(x))

scores <- apply(normalized.topics, 2, function(x) 
  x * ( log(x + 1e-05) - sum(log(x + 1e-05))/length(x)) )
colnames(scores) <- TM[["Gibbs"]]@terms
words <- apply(scores, 1, function(x) 
  colnames(scores)[order(x, decreasing = TRUE)[1:num.words]])
f.scores <- apply(scores, 1, function(x) 
  x[order(x, decreasing = TRUE)[1:num.words]])
n.topics <- rep(seq(1, K, 1), each=num.words)
order.topics <- rep(seq(1, num.words, 1), times=K)
info.df <- data.frame(
  topic = n.topics,
  word = c(words),
  order = as.character(order.topics),
  score = c(f.scores),
  stringsAsFactors=F)
info.df$order <- factor(info.df$order, levels=as.character(10:1))

info.df$specificity <- NA
for (i in 1:length(info.df$topic)){
  info.df$specificity[i] <- normalized.words[info.df$topic[i], which(colnames(scores) %in% info.df$word[i])]
}
info.df$topic <- paste0("Topic ", info.df$topic)
info.df$topic <- factor(info.df$topic, levels=paste0("Topic ", 1:K))

topten<-vector("list", K)

for (i in 1:K){
  j<-10*(i-1)+1
  m<-10*i
  topten[[i]]<-cbind(info.df$word[j:m])
}

####Now let's look at a few of the topics of interest

gov1

gov2

topten[[?]]

##shannon entropy

shanDiv <- function( pVec, zeroPad = 1e-100 ) {  
  psum <- sum( pVec )
  zeros <- which(pVec == 0)
  if (length(zeros) > 0) {
    ##print( "you can't take log of 0\n")
    pVec[zeros] <- zeroPad
  }
  if (psum != 1) {
    print( "your probabilities don't sum to 1 \n")
  }
  hvec <- pVec * log( pVec, base = 2)
  h <- -1.0 * sum( hvec )
  return( h )
}



##look at diversity scores

##apply diversity scores by day
doc_topics<-t(doc_topics)
ndx<-apply(doc_topics, 2, sort, decreasing=TRUE)

nd<-apply(ndx, 2, shanDiv)
###test<-apply(ndx, 2, sum) ##YES IT SUMS TO 1


#divide into coalitions and by time
govgammas<-nd[1:162]
oppgammas<-nd[163:324]
index<-seq(1:162)

index1<-index

Period<-ordered(index1)
dates<-seq(as.Date("2013/12/18"), by="days", length=162)


#
install.packages("reshape2")
library("reshape2")


dat<-data.frame(dates,oppgammas, govgammas)
alldens.mm = melt(dat, id.vars ="dates", measure.vars = c("oppgammas","govgammas"))
alldens.mm$Faction<-alldens.mm$variable
levels(alldens.mm$Faction)[levels(alldens.mm$Faction) =="oppgammas"]<-"Opposition"
levels(alldens.mm$Faction)[levels(alldens.mm$Faction) =="govgammas"]<-"Regime"




###Plot results


{ggplot(alldens.mm, aes(y=value, x=dates, pch = Faction, linetype=Faction)) + geom_point() + 
  
  xlab(NULL) + ylab("Shannon Entropy") + scale_shape_manual(values=c(18, 1))+
  stat_smooth(span=.5, se=TRUE)+
  
  # geom_vline(xintercept=as.numeric(date[61])) +
  geom_vline(xintercept=as.numeric(dates[57])) +
  geom_vline(xintercept=as.numeric(dates[143]))  +
  geom_vline(xintercept=as.numeric(dates[114]), linetype=2)  

}



