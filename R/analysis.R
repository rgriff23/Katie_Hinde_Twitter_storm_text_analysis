################
# PREPARATIONS #
################

# load packages
library("plyr")
library("ggplot2")
library("tm")
library("topicmodels")
library("wordcloud")
library("igraph")
library("animation")

# import and format tweet data
tweet_data <- read.csv("https://raw.githubusercontent.com/rgriff23/Katie_Hinde_Twitter_storm_text_analysis/master/data/tweet_data.csv")
tweet_data$sentimentA <- factor(tweet_data$sentimentA, levels=c("Very Negative", "Negative", "Neutral", "Positive","Very Positive"))
tweet_data$sentimentB <- factor(tweet_data$sentimentB, levels=c("joy","sadness","anger","surprise","fear","disgust"))
tweet_data$time <- as.POSIXct(tweet_data$time, format="%Y-%m-%d %H:%M:%S")

# import social network, layout for plotting, and popular friends from each cluster
temp <- tempfile()
download.file("https://github.com/rgriff23/Katie_Hinde_Twitter_storm_text_analysis/raw/master/data/graph_fancy.rds", temp)
g <- readRDS(temp)
download.file("https://github.com/rgriff23/Katie_Hinde_Twitter_storm_text_analysis/raw/master/data/graph_layout.rds", temp)
g_layout <- readRDS(temp)
download.file("https://github.com/rgriff23/Katie_Hinde_Twitter_storm_text_analysis/raw/master/data/topfriends.rds", temp)
topfriends <- readRDS(temp)
unlink(temp)

#########################################
# TWEETS OVER TIME + INFLUENTIAL TWEETS #
#########################################

# histogram of tweet volume over time, with popular tweets overlaid
popular_tweets <- tweet_data[tweet_data$retweets>9,]
popular_tweets$height <- popular_tweets$retweets/10
popular_tweets$user <- as.character(popular_tweets$user) 
super_tweets <- as.character(head(tweet_data[order(tweet_data$retweets, decreasing = TRUE),1:5],6)$user)
popular_tweets$user <- ifelse(popular_tweets$user%in%super_tweets, popular_tweets$user, NA)
ggplot(tweet_data, aes(time, label=user)) +
  geom_histogram(binwidth=1800,fill=I('lightsteelblue')) +
  geom_segment(aes(x=time, y=0, xend=time, yend=height), data=popular_tweets, color="red", size=0.4) + 
  geom_text(aes(y=height, label=user), data=popular_tweets, size=3) +
  ylab("Count") +
  xlab("Time") +
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=20))

####################################
# SENTIMENT ANALYSIS & WORD CLOUDS #
####################################

# barplot for emotional valence
ggplot(tweet_data, aes(sentimentA)) + 
  geom_bar() +
  ylab("Count") +
  xlab("") +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20))

# barplot for emotional tone
ggplot(tweet_data[!is.na(tweet_data$sentimentB),], aes(sentimentB)) + 
  geom_bar() +
  ylab("Count") +
  xlab("") +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20))

# create corpus from tweets
corpus <- Corpus(VectorSource(tweet_data$text))
corpus <- tm_map(tm_map(corpus, removeWords, stopwords('english')), stemDocument)

# make word cloud for entire corpus
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

##################
# TOPIC ANALYSIS #
##################

# This isn't very interesting because there is really only one topic, but here's the code
mat <- create_matrix(tweet_data$text[1:10], language='english', removeNumbers=TRUE, stemWords=TRUE)
dtm <- DocumentTermMatrix(corpus)
dtm   <- dtm[apply(dtm , 1, sum)> 0, ] #remove docs without words
test <- LDA(dtm, 5)
term <- terms(test, 7)
term <- apply(term, MARGIN = 2, paste, collapse = ", ")
term

###########################
# SOCIAL NETWORK ANALYSIS #
###########################

# remove edges for plotting
g2 <- delete_edges(g, E(g))

# static plot
plot(g2, ylim=c(-1,0.5), xlim=c(-0.5,1), vertex.label=V(g2)$label_special, vertex.label.cex=1, vertex.label.color="black", vertex.label.font=2, layout=g_layout)

# animated plot
saveGIF({
  cols <- rep(NA, vcount(g2))
  labs <- rep(NA, vcount(g2))
  for (i in 2:13) {
    cols[V(g2)$time_bin<i] <- V(g2)$color[V(g2)$time_bin<i]
    labs[V(g2)$time_bin<i] <- V(g2)$label_special[V(g2)$time_bin<i]
    plot(g2, vertex.color=cols, ylim=c(-0.75,0.3), xlim=c(-0.3,0.6), vertex.label=labs, vertex.label.cex=1, vertex.label.color="black", vertex.label.font=2, layout=g_layout)
    }
},
interval=1, movie.name="~/Desktop/GitHub/Katie_Hinde_Twitter_storm_text_analysis/network_animation.gif")

########################################
# MOST POPULAR FRIENDS IN EACH CLUSTER #
########################################

# combine top friends into table
topfriends <- cbind(topfriends[[1]], topfriends[[2]], topfriends[[3]])
colnames(topfriends) <- c("Cluster 1", "Cluster 2", "Cluster 3")
rownames(topfriends) <- 1:25
topfriends

######################################
# SENTIMENTS & WORDS IN EACH CLUSTER #
######################################

# stacked barplot of volume of tweets over time
tweet_data$time_bin <- factor(tweet_data$time_bin, levels=1:12)
tweet_data$cluster <- mapvalues(tweet_data$cluster, 1:12, c("Apolitical", "Rightwing","Leftwing",NA,NA,NA,NA,NA,NA,NA,NA,NA))
tweet_data2 <- tweet_data[!is.na(tweet_data$cluster),]
ggplot(tweet_data2, aes(x=time_bin, fill=cluster)) +
  geom_bar() +
  scale_fill_manual(values=c("darkgray","blue","red")) +
  xlab("Time slice (3 hour intervals)") +
  ylab("Count") +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20),
        legend.title=element_text(size=20),
        legend.text=element_text(size=12))

# side-by-side barplot for emotional valence
ggplot(tweet_data2, aes(x=cluster, fill=sentimentA)) +
  geom_bar(position=position_dodge()) +
  scale_fill_brewer(palette="RdBu", name="Emotional valence") +
  ylab("Count") +
  xlab("") +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20),
        legend.title=element_text(size=20),
        legend.text=element_text(size=12))

# side-by-side barplot for emotional tone
ggplot(tweet_data2[!is.na(tweet_data2$sentimentB),], aes(x=cluster, fill=sentimentB)) +
  geom_bar(position=position_dodge()) +
  scale_fill_brewer(palette="Accent", name="Emotion") +
  ylab("Count") +
  xlab("") +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20),
        legend.title=element_text(size=20),
        legend.text=element_text(size=12))

# create corpuses (corpi?)
corpus1 <- Corpus(VectorSource(tweet_data$text[tweet_data$cluster==1]))
corpus1 <- tm_map(tm_map(corpus1, removeWords, stopwords('english')), stemDocument)
corpus2 <- Corpus(VectorSource(tweet_data$text[tweet_data$cluster==2]))
corpus2 <- tm_map(tm_map(corpus2, removeWords, stopwords('english')), stemDocument)
corpus3 <- Corpus(VectorSource(tweet_data$text[tweet_data$cluster==3]))
corpus3 <- tm_map(tm_map(corpus3, removeWords, stopwords('english')), stemDocument)

# wordclouds for each cluster
wordcloud(corpus1$content, max.words = 100, colors=heat.colors(6), random.order = FALSE)
wordcloud(corpus2$content, max.words = 100, colors=heat.colors(6), random.order = FALSE)
wordcloud(corpus3$content, max.words = 100, colors=heat.colors(6), random.order = FALSE)

# comparison cloud
cloudC <- ddply(tweet_data[tweet_data$cluster%in%c(1:3),], .(cluster), function (x) {paste(x$text, collapse=" ")})
cloudC <- Corpus(VectorSource(cloudC$V1))
cloudC <- tm_map(tm_map(cloudC, removeWords, stopwords('english')), stemDocument)
cloudC <- as.matrix(TermDocumentMatrix(cloudC))
colnames(cloudC) <- c("Apolitical", "Rightwing","Leftwing")
comparison.cloud(cloudC, max.words=60, scale=c(4,.5), title.size=2, colors=c("gray","red","blue"))

#######
# END #
#######
