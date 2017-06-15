################
# PREPARATIONS #
################

# load packages
library("twitteR")
library("plyr")

# import data
clean_tweet_data <- read.csv("https://raw.githubusercontent.com/rgriff23/Katie_Hinde_Twitter_storm_text_analysis/master/data/clean_tweet_data.csv", row.names=1)

# format data
clean_tweet_data$sentimentA <- factor(clean_tweet_data$sentimentA, levels=c("Very Negative", "Negative", "Neutral", "Positive","Very Positive"))
clean_tweet_data$sentimentB <- factor(clean_tweet_data$sentimentB, levels=c("joy","sadness","anger","surprise","fear","disgust"))

#######################
# BASIC DATA OVERVIEW #
#######################

# Frequency of different sentiments
matrix(1:2, 1, 2)
barplot(table(clean_tweet_data$sentimentA), xlab="Emotional valence", ylab="Proportion of tweets")
barplot(table(clean_tweet_data$sentimentB), xlab="Emotional tone")

# Make word cloud for all tweets 
corpus <- Corpus(VectorSource(clean_tweet_data$text))
corpus <- tm_map(tm_map(corpus, removeWords, stopwords('english')), stemDocument)
wordcloud(corpus$content, max.words = 100, colors=rainbow(10), random.order = FALSE)

# word clouds for sentimentA 
clean_tweet_data2 <- clean_tweet_data[clean_tweet_data$sentimentA != "Neutral",]
clean_tweet_data2$sentimentA <- ifelse(clean_tweet_data2$sentimentA %in% c("Very Positive", "Positive"), "Positive", "Negative")
cloudA <- ddply(clean_tweet_data2, .(sentimentA), function (x) {paste(x$text, collapse=" ")})
cloudA <- Corpus(VectorSource(cloudA$V1))
cloudA <- tm_map(tm_map(cloudA, removeWords, stopwords('english')), stemDocument)
cloudA <- as.matrix(TermDocumentMatrix(cloudA))
colnames(cloudA) <- c("Negative","Positive")
comparison.cloud(cloudA, max.words=40, scale=c(4,.5), title.size=2, colors=rev(brewer.pal(3, "YlOrRd")))

# word clouds for sentimentB 
cloudB <- ddply(clean_tweet_data[!is.na(clean_tweet_data$sentimentB),], .(sentimentB), function (x) {paste(x$text, collapse=" ")})
cloudB <- Corpus(VectorSource(cloudB$V1))
cloudB <- tm_map(tm_map(cloudB, removeWords, stopwords('english')), stemDocument)
cloudB <- as.matrix(TermDocumentMatrix(cloudB))
colnames(cloudB) <- levels(clean_tweet_data$sentimentB)
cloudB <- cloudB[,c("sadness", "joy","surprise","fear","anger","disgust")]
comparison.cloud(cloudB, max.words=60, scale=c(4,.5), title.size=2, colors=rev(brewer.pal(5, "Reds")))

wordcloud(corpus$content, max.words = 100, colors=rev(heat.colors(10)), random.order = FALSE)
comparison.cloud(cloudA, max.words=60, scale=c(4,.5), title.size=2, colors=rev(brewer.pal(3, "YlOrRd")))
comparison.cloud(cloudB, max.words=60, scale=c(4,.5), title.size=2, colors=rev(brewer.pal(6, "Reds")))


#######
# END #
#######