####Recitation 5



###Bullying Data---Example from Pablo Barbera's Short Course on R, NYU 2016   
##   https://github.com/pablobarbera/data-science-workshop
install.packages("tm")
library(tm)

setwd("C:/Users/kevin/Documents/GitHub/Text_as_Data/")




df.tweets <- read.csv("bullying.csv", stringsAsFactors = FALSE)

# Identify posts with and without bullying traces and create large documents
no_bullying <- paste(df.tweets$text[df.tweets$bullying_traces=="n"], collapse=" ")
yes_bullying <- paste(df.tweets$text[df.tweets$bullying_traces=="y"], collapse=" ")
# Create DTM and preprocess
groups <- VCorpus(VectorSource(c("No bullying" = no_bullying, "Yes bullying" = yes_bullying)))
groups <- tm_map(groups, content_transformer(tolower))
groups <- tm_map(groups, removePunctuation)
groups <- tm_map(groups, stripWhitespace)
dtm <- DocumentTermMatrix(groups)
## Label the two groups
dtm$dimnames$Docs = c("No bullying", "Yes bullying")
## Transpose matrix so that we can use it with comparison.cloud
tdm <- t(dtm)
## Compute TF-IDF transformation
tdm <- as.matrix(weightTfIdf(tdm))

## Display the two word clouds
library(wordcloud)
comparison.cloud(tdm, max.words=100, colors=c("red", "blue"))


###Let's train an SVM


df.tweets$type <- as.numeric(factor(df.tweets$bullying_traces))
training_break <- as.integer(0.9*nrow(df.tweets))

##new package, better for SVM
install.packages("RTextTools")
library(RTextTools)


?create_matrix


dtm       <- create_matrix(df.tweets$text, language="english", stemWords = FALSE,
                           weighting = weightTfIdf, removePunctuation = FALSE)





#####Make it all in-sample
container      <- create_container(dtm, t(df.tweets$type), trainSize=1:length(df.tweets$type),
                                    virgin=FALSE)

##train the model
cv.svm <- cross_validate(container, nfold=2, algorithm = 'SVM', kernel = 'linear')

?cross_validate



########



container      <- create_container(dtm, t(df.tweets$type), trainSize=1:training_break,
                                   testSize=training_break:nrow(df), virgin=FALSE)

###Let's train the model 


cv.svm <- cross_validate(container, nfold=2, algorithm = 'SVM', kernel = 'linear')



##validate
cv.svm$meanAccuracy


prop.table(table(df.tweets$type)) # baseline



####################Let's try again with a different kernel

cv.svm <- cross_validate(container, nfold=10, algorithm = 'SVM', kernel = 'radial')




#################What if we try with different % test/train


training_break <- as.integer(0.5*nrow(df.tweets))




dtm       <- create_matrix(df.tweets$text, language="english", stemWords = FALSE,
                           weighting = weightTfIdf, removePunctuation = FALSE)
?create_matrix

###
container      <- create_container(dtm, t(df.tweets$type), trainSize=1:training_break,
                                   testSize=training_break:nrow(df), virgin=FALSE)


##validate

cv.svm <- cross_validate(container, nfold=2, algorithm = 'SVM', kernel = 'linear')

cv.svm$meanAccuracy


prop.table(table(df.tweets$type)) # baseline





#############Let's do this multinomially
df.tweets <- read.csv("bullying.csv", stringsAsFactors = FALSE)


table(df.tweets$type)

##filter to only those that are bullying

df <- df.tweets[df.tweets$type!="",]

df$type <- as.numeric(factor(df$type))
training_break <- as.integer(0.9*nrow(df))


dtm       <- create_matrix(df$text, language="english", stemWords = FALSE,
                           weighting = weightTfIdf, removePunctuation = FALSE)


container      <- create_container(dtm, t(df$type), trainSize=1:training_break,
                                   testSize=training_break:nrow(df), virgin=FALSE)

?create_container
cv.svm <- cross_validate(container, nfold=5, algorithm = 'SVM', kernel = 'linear')

cv.svm$meanAccuracy
prop.table(table(df$type)) # baseline





################################Virality of stories from NYT




nyt.fb <- read.csv("./nyt-fb.csv", stringsAsFactors=FALSE)


str(nyt.fb)


##create variables for month and hour

head(nyt.fb$created_time)

month <- substr(nyt.fb$created_time, 6, 7)


hour <- substr(nyt.fb$created_time, 12, 13)


nyt.fb <- data.frame(nyt.fb, month, hour)



##########decide what constitutes ''viral"

total.resp <- nyt.fb$likes_count + nyt.fb$shares_count + nyt.fb$comments_count

##look at the extreme of the distribution
quantile(total.resp, .95) 



viral <- total.resp > 10000

nyt.fb$viral <- viral



########### For the purposes of not destroying my laptop, let's choose a set of features

nyt.fb$viral <- as.numeric(factor(nyt.fb$viral))


training_break <- as.integer(0.9*nrow(nyt.fb))


dtm       <- create_matrix(nyt.fb$message, language="english", stemWords = FALSE,
                           weighting = weightTfIdf, removePunctuation = FALSE)


container      <- create_container(dtm, t(nyt.fb$type), trainSize=1:training_break,
                                   testSize=training_break:nrow(nyt.fb), virgin=FALSE)



cv.svm <- cross_validate(container, nfold=2, algorithm = 'SVM', kernel = 'linear')


cv.svm$meanAccuracy

prop.table(table(nyt.fb$viral))



############relative to logistic regression

message <- removePunctuation(tolower(nyt.fb$message))
nyt.fb$israel <- grepl("israel", message)
nyt.fb$trump <- grepl("trump", message)
nyt.fb$hillary <- grepl("hillary", message)
nyt.fb$obama <- grepl("barack|obama", message)
nyt.fb$terror <- grepl("terror|isis|isil|qaeda", message)
nyt.fb$kill <- grepl("kill|murder|shot", message)
nyt.fb$debate <- grepl("debat", message)


# regression example
glm.viral <- glm(as.factor(viral) ~ month + hour  + 
                   israel + trump + hillary + obama + terror + kill + 
                   debate , data=nyt.fb, family=binomial(logit))





table(round(glm.viral$fitted.values))
