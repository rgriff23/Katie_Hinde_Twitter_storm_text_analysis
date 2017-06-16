################
# PREPARATIONS #
################

# load packages
library("twitteR")
library("ggplot2")

# import data
clean_tweet_data <- read.csv("https://raw.githubusercontent.com/rgriff23/Katie_Hinde_Twitter_storm_text_analysis/master/data/clean_tweet_data.csv", row.names=1)

# format data
clean_tweet_data$sentimentA <- factor(clean_tweet_data$sentimentA, levels=c("Very Negative", "Negative", "Neutral", "Positive","Very Positive"))
clean_tweet_data$sentimentB <- factor(clean_tweet_data$sentimentB, levels=c("joy","anger","sadness","surprise","fear","disgust"))
clean_tweet_data$time <- as.POSIXct(clean_tweet_data$time)

###################################
# VIZUALIZE TWEET STORM OVER TIME #
###################################

# Volume of tweets over time
timebins <- cut.POSIXt(clean_tweet_data$time, breaks="hour")
barplot(table(timebins))

# SentimentA
tableA <- table(clean_tweet_data$sentimentA,timebins)
tableA2 <- tableA
tableA2[2,] <- tableA2[1,]+tableA2[2,]
tableA2[4,] <- tableA2[4,]+tableA2[5,]
tableA2 <- tableA2[-c(1,5),]
barplot(tableA2, beside = TRUE, col=c("red","gray","white"))

# SentimentB
tableB <- table(clean_tweet_data$sentimentB, timebins)
barplot(tableB[1:3,], beside=TRUE, col=c("white","lightblue", "red"))


#######
# END #
#######

