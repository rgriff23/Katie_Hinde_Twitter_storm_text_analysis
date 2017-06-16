################
# PREPARATIONS #
################

# load packages
library("twitteR")
library("plyr")

# import data
tweet_data <- read.csv("https://raw.githubusercontent.com/rgriff23/Katie_Hinde_Twitter_storm_text_analysis/master/data/tweet_data.csv", row.names=1)

# format data
tweet_data$sentimentA <- factor(tweet_data$sentimentA, levels=c("Very Negative", "Negative", "Neutral", "Positive","Very Positive"))
tweet_data$sentimentB <- factor(tweet_data$sentimentB, levels=c("joy","anger","sadness","surprise","fear","disgust"))
tweet_data$time <- as.POSIXct(tweet_data$time)

#########################
# WORD FREQUENCY CLOUDS #
#########################

# first check frequency of different sentiments
layout(matrix(1:2, 1, 2))
tableA <- table(tweet_data$sentimentA)
tableB <- table(tweet_data$sentimentB)
barplot(tableA/sum(tableA), xlab="Emotional valence", ylab="Proportion of tweets")
barplot(tableB/sum(tableB), xlab="Emotional tone")

# make word cloud for entire corpus
corpus <- Corpus(VectorSource(tweet_data$text))
corpus <- tm_map(tm_map(corpus, removeWords, stopwords('english')), stemDocument)
wordcloud(corpus$content, max.words = 100, colors=heat.colors(6), random.order = FALSE)

# make comparison word cloud for sentimentA 
tweet_data2 <- tweet_data[tweet_data$sentimentA != "Neutral",]
tweet_data2$sentimentA <- ifelse(tweet_data2$sentimentA %in% c("Very Positive", "Positive"), "Positive", "Negative")
cloudA <- ddply(tweet_data2, .(sentimentA), function (x) {paste(x$text, collapse=" ")})
cloudA <- Corpus(VectorSource(cloudA$V1))
cloudA <- tm_map(tm_map(cloudA, removeWords, stopwords('english')), stemDocument)
cloudA <- as.matrix(TermDocumentMatrix(cloudA))
colnames(cloudA) <- c("Negative","Positive")
comparison.cloud(cloudA, max.words=60, scale=c(4,.5), title.size=2, colors=rev(brewer.pal(3, "YlOrRd")))

# make comparison word cloud for sentimentB 
cloudB <- ddply(tweet_data[!is.na(tweet_data$sentimentB),], .(sentimentB), function (x) {paste(x$text, collapse=" ")})
cloudB <- Corpus(VectorSource(cloudB$V1))
cloudB <- tm_map(tm_map(cloudB, removeWords, stopwords('english')), stemDocument)
cloudB <- as.matrix(TermDocumentMatrix(cloudB))
colnames(cloudB) <- levels(tweet_data$sentimentB)
cloudB <- cloudB[,c("sadness", "joy","surprise","fear","anger","disgust")]
comparison.cloud(cloudB, max.words=60, scale=c(4,.5), title.size=2, colors=rev(brewer.pal(5, "Reds")))

#######
# END #
#######