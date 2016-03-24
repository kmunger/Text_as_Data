####Recitation 6 (the 7th one, sorry)




##Go over problem set



##tricky bits



##MLTD by hand

tokens2009<-tokenize(o2009,removePunct=TRUE )


MLTD2009<-vector()
for(j in 1:25){
  i<-0
  TTR2009<-1
  while(TTR2009>.72){
    i<-i + 1;
    MLTD2009[j]<-i;
    tok<-tokens2009[[1]][(j-1)*200 +1: i];
    
    tokenz<-length(tok);
    
    typez<-length(unique(tok));
    
    TTR2009<-typez/tokenz;
    
  }
}
mean(MLTD2009)


#dividing up texts

##decide on snippet length
snip_length<-150

## divide up austen texts
for(i in 1:ceiling(length(austen)/snip_length)){
  index<-(i-1)*snip_length + 1
  # create appropriate length snippet
  austen_snippets[i]<-paste(austen[index:(index+snip_length)],sep="", collapse=" ")
}


###FRE bootstrap: Question 6

#setwd("C:/Users/kevin/Dropbox/Text_As_Data_Spring_2016_SpirlingMunger/homeworks/hw1/text_data/cons")
setwd("C:/Users/kevin/Documents/GitHub/Text_as_Data/cons")


##read in the files
files <- list.files( full.names=TRUE)
text <- lapply(files, readLines)
##load data

#name data
files<-unlist(files)
files<-gsub("./", "", files )
files<-gsub(".txt", "", files )

df<-data.frame(year = rep((files), sapply(text, length)), text = (unlist(text)), stringsAsFactors = FALSE)

library(dplyr)
##code to clean up text

df<-filter(df, grepl("^\\ï", df$text)==FALSE & grepl("^\\d", df$text) ==FALSE)
df<-filter(df, ntoken(df$text)>3)


##divide up by sentence, assign labels

tokens<-vector()
labels<-vector()
for(i in 1:length(df$text)){
  tokens<-c(tokens, unlist(tokenize(df$text[i], what = "sentence")))
  labels<-c(labels, rep(df$year[i], length(unlist(tokenize(df$text[i], what = "sentence")))))
  
}


df_tokens<-data.frame(tokens, stringsAsFactors = FALSE)
df_tokens$year<-as.factor(labels)


##restrict to real sentences
df_tokens<-filter(df_tokens, ntoken(df_tokens$tokens)>3)

#find the correlation

df_tokens$read_FRE<-readability(df_tokens$tokens, "Flesch")
df_tokens$read_DC<-readability(df_tokens$tokens, "Dale.Chall")

cor(df_tokens$read_FRE, df_tokens$read_DC)

##



##initialize data frames
year_FRE<-data.frame(matrix(ncol = 23, nrow = 100))


#run the bootstraps

for(i in 1:100){
  
  #sample 2000 
  
  bootstrapped_year<-sample_n(df_tokens, 3000, replace=TRUE)
  
  bootstrapped_year$read_FRE<-readability(bootstrapped_year$tokens, "Flesch")
  
  #store results
  
  year_FRE[i,]<-aggregate(bootstrapped_year$read_FRE, by=list(bootstrapped_year$year), FUN=mean)[,2]
  
  length(aggregate(bootstrapped_year$read_FRE, by=list(bootstrapped_year$year), FUN=mean)[,2])
  
}

#name the data frames

colnames(year_FRE)<-names(table(df_tokens$year))

#define the standard error function
std <- function(x) sd(x)/sqrt(length(x))

##calculate standard errors and point estimates

year_ses<-apply(year_FRE, 2, std)

year_means<-apply(year_FRE, 2, mean)

#graphs --- year

coefs<-year_means
ses<-year_ses

y.axis <- c(1:23)
min <- min(coefs - 2*ses)
max <- max(coefs + 2*ses)
var.names <- colnames(year_FRE)
adjust <- 0
par(mar=c(2,8,2,2))
#setwd("C:/Users/kevin/Dropbox/tokens_As_Data_Spring_2016_SpirlingMunger/homeworks")
#pdf_tokens("Q6.pdf_tokens", 7, 5)

plot(coefs, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, cex = .8, 
     xlim=c(min,max),ylim = c(.5,23.5), main = "")
#rect(min,.5,max,1.5, col = c("grey97"), border="grey90", lty = 2)
#rect(min,1.5,max,2.5, col = c("grey95"), border="grey90", lty = 2)
#rect(min,2.5,max,3.5, col = c("grey97"), border="grey90", lty = 2)
#rect(min,3.5,max,4.5, col = c("grey95"), border="grey90", lty = 2)
#rect(min,4.5,max,5.5, col = c("grey97"), border="grey90", lty = 2)
#rect(min,5.5,max,6.5, col = c("grey97"), border="grey90", lty = 2)

axis(1, at = seq(min,max,(max-min)/10), 
     labels = c(round(min+0*((max-min)/10),3),
                round(min+1*((max-min)/10),3),
                round(min+2*((max-min)/10),3),
                round(min+3*((max-min)/10),3),
                round(min+4*((max-min)/10),3),
                round(min+5*((max-min)/10),3),
                round(min+6*((max-min)/10),3),
                round(min+7*((max-min)/10),3),
                round(min+8*((max-min)/10),3),
                round(min+9*((max-min)/10),3),
                round(max,3)),tick = T,cex.axis = .75, mgp = c(2,.7,0))
axis(2, at = y.axis, label = var.names, las = 1, tick = FALSE, cex.axis =.8)
abline(h = y.axis, lty = 2, lwd = .5, col = "white")
segments(coefs-qnorm(.975)*ses, y.axis+2*adjust, coefs+qnorm(.975)*ses, y.axis+2*adjust, lwd =  1)

segments(coefs-qnorm(.95)*ses, y.axis+2*adjust-.035, coefs-qnorm(.95)*ses, y.axis+2*adjust+.035, lwd = .9)
segments(coefs+qnorm(.95)*ses, y.axis+2*adjust-.035, coefs+qnorm(.95)*ses, y.axis+2*adjust+.035, lwd = .9)
points(coefs, y.axis+2*adjust,pch=21,cex=.8, bg="white")
observed<-aggregate(df_tokens$read_FRE, by=list(df_tokens$year), FUN=mean)[,2]
points(observed, y.axis+2*adjust,pch=13,cex=.8, bg="white")
#dev.off()












##code for Question 3/4

##read in the files




setwd("C:/Users/kevin/Documents/GitHub/Text_as_Data/dickens_austen")

##read in the files
files <- list.files( full.names=TRUE)
texts <- lapply(files, readLines)


##create labels
files<-unlist(files)
files<-gsub("./", "", files )
files<-gsub(".txt", "", files )


##clean up files

head(texts[[1]], n=100)
tail(texts[[1]], n=500)

texts[[1]]<-head(texts[[1]], -366)
texts[[1]]<-tail(texts[[1]], -29)


texts[[2]]<-head(texts[[2]], -366)
texts[[2]]<-tail(texts[[2]], -29)

texts[[3]]<-head(texts[[3]], -366)
texts[[3]]<-tail(texts[[3]], -29)

texts[[4]]<-head(texts[[4]], -366)
texts[[4]]<-tail(texts[[4]], -29)

texts[[5]]<-head(texts[[5]], -366)
texts[[5]]<-tail(texts[[5]], -29)


texts[[6]]<-head(texts[[6]], -360)
texts[[6]]<-tail(texts[[6]], -33)


texts[[7]]<-head(texts[[7]], -363)
texts[[7]]<-tail(texts[[7]], -29)

texts[[8]]<-head(texts[[8]], -363)
texts[[8]]<-tail(texts[[8]], -29)


texts[[9]]<-head(texts[[9]], -363)
texts[[9]]<-tail(texts[[9]], -29)


texts[[10]]<-head(texts[[10]], -363)
texts[[10]]<-tail(texts[[10]], -29)

texts[[11]]<-head(texts[[11]], -363)

##remove blank characters--note that this isn't strictly necessary
texts<-lapply(1:11, function(i) subset(texts[[i]], texts[[i]]!=""))


##chopping up the texts into the right size chunks, according to P&H

# combine all books
austen<-unlist(texts[1:5])


#initialize
austen_snippets<-vector()


##decide on snippet length
snip_length<-150

## divide up austen texts
for(i in 1:ceiling(length(austen)/snip_length)){
  index<-(i-1)*snip_length + 1
  # create appropriate length snippet
  austen_snippets[i]<-paste(austen[index:(index+snip_length)],sep="", collapse=" ")
}


##now for dickens

dickens<-unlist(texts[6:10])

#initialize
dickens_snippets<-vector()

## divide up dickens texts
for(i in 1:ceiling(length(dickens)/snip_length)){
  index<-(i-1)*snip_length + 1
  # create appropriate length snippet
  dickens_snippets[i]<-paste(dickens[index:(index+snip_length)],sep="", collapse=" ")
}

## clean up mystery document

mystery<-unlist(texts[11])


#initialize
mystery_snippets<-vector()


## divide up mystery text
for(i in 1:ceiling(length(mystery)/snip_length)){
  index<-(i-1)*snip_length + 1
  # create appropriate length snippet
  mystery_snippets[i]<-paste(mystery[index:(index+snip_length)],sep="", collapse=" ")
}


##combine the snippets
snippets<-c(austen_snippets,dickens_snippets)


#create a labeled vector
authors<-c(rep("austen", length(austen_snippets)), rep("dickens", length(dickens_snippets)))


###list function words--taken from P & H 
function_words<-c("a", "been", "had", "its", "one", "the", "were", "all", "but", "has", "may", "only", "their", "what",
                  "also", "by", "have", "more", "or", "then", "when", "an", "can", "her", "must", "our", "there", "which",
                  "and", "do", "his", "my", "should", "things", "who",  "any", "down", "if", "no", "so", "this", "will",  
                  "are", "even", "in", "not", "some", "to", "with",  "as", "every", "into", "now", "such", "up", "would",
                  "at", "for", "is", "of", "than", "upon", "your","be", "from", "it", "on", "that", "was")


##Create DFMs of just the function words

snippets_dfm<-dfm(snippets, keptFeatures=function_words)


mystery_dfm<-dfm(mystery_snippets, keptFeatures=function_words)



###run PCA


snippets_pca<-prcomp(snippets_dfm, center=TRUE, scale.=TRUE)
?prcomp

##examine number of components

plot(snippets_pca, type = "l")

##packages for visualization--code taken from http://www.r-bloggers.com/computing-and-visualizing-pca-in-r/


install_github("ggbiplot", "vqv")


library(ggbiplot)


g <- ggbiplot(snippets_pca, obs.scale = 1, var.scale = 1, 
              groups = authors)
g<- g + theme(legend.direction = 'horizontal', 
              legend.position = 'top')
g




##Predict 
predicted<-predict(snippets_pca, newdata=mystery_dfm)


##Fisher's linear discrimination rule: choose the group that has a closer group mean; just 2 dimensions

#find the mean of the first two PCs 
austen_pc1_mean<-mean(snippets_pca$x[1:326,1])
austen_pc2_mean<-mean(snippets_pca$x[1:326,2])
austen_mean<-c(austen_pc1_mean, austen_pc2_mean)


dickens_pc1_mean<-mean(snippets_pca$x[327:1033,1])
dickens_pc2_mean<-mean(snippets_pca$x[327:1033,2])
dickens_mean<-c(dickens_pc1_mean, dickens_pc2_mean)


mystery_pc1_mean<-mean(predicted[,1])
mystery_pc2_mean<-mean(predicted[,2])
mystery_mean<-c(mystery_pc1_mean, mystery_pc2_mean)




#calculate the distances
austen_distance<-dist(rbind(austen_mean, mystery_mean))
dickens_distance<-dist(rbind(dickens_mean, mystery_mean))

###it's austen!

###how else might we do this? 












### k nearest neighbors! 

#install.packages("class")
library(class)

?knn




knn_1<-knn(train=snippets_dfm, test = mystery_dfm, cl = authors, k=1 )


knn_3<-knn(train=snippets_dfm, test = mystery_dfm, cl = authors, k=3 )

knn_10<-knn(train=snippets_dfm, test = mystery_dfm, cl = authors, k=10 )

## well then....




## Some Examples taken wholesale from Ken Benoit's NYU Dept. of Politics short course Fall 2014
## Avaliable on his website: www.kenbenoit.net

##load data

library(quanteda)
library(quantedaData)
