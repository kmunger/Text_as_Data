##Code for homework 1


##Question 3 : PCA 

###run PCA: input the snippets of text, counting the appropriate features

snippets_pca<-prcomp(snippets_dfm, center=TRUE, scale.=TRUE)

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



##Predict : input the dfm (with appropriate features) of the mystery text

predicted<-predict(snippets_pca, newdata=mystery_dfm)

##Fisher's linear discrimination rule: choose the group that has a closer group mean; just 2 dimensions


d<-length(dickens_snippets)
a<-length(austen_snippets)


#find the mean of the first two PCs 
austen_pc1_mean<-mean(snippets_pca$x[1:a,1])
austen_pc2_mean<-mean(snippets_pca$x[1:a,2])



dickens_pc1_mean<-mean(snippets_pca$x[327:1033,1])
dickens_pc2_mean<-mean(snippets_pca$x[327:1033,2])
dickens_mean<-c(dickens_pc1_mean, dickens_pc2_mean)


mystery_pc1_mean<-mean(predicted[,1])
mystery_pc2_mean<-mean(predicted[,2])
mystery_mean<-c(mystery_pc1_mean, mystery_pc2_mean)

##now you need to find which is closer to the mystery mean



#Question 6: filtering text

library(dplyr)

##apply to your data frame of the texts of the manifestos 

df<-filter(df, grepl("^\\ï", df$text)==FALSE &grepl("^\\d", df$text) ==FALSE)
df<-filter(df, ntoken(df$text)>3)