################
# PREPARATIONS #
################

# load packages
library("twitteR")
library("ggplot2")

# import data
replies_data <- read.csv("https://raw.githubusercontent.com/rgriff23/Katie_Hinde_Twitter_storm_text_analysis/master/data/replies_data.csv", row.names=1)
quotes_data <- read.csv("https://raw.githubusercontent.com/rgriff23/Katie_Hinde_Twitter_storm_text_analysis/master/data/quotes_data.csv", row.names=1)

# format data
replies_data$sentimentA <- factor(replies_data$sentimentA, levels=c("Very Negative", "Negative", "Neutral", "Positive","Very Positive"))
replies_data$sentimentB <- factor(replies_data$sentimentB, levels=c("joy","anger","sadness","surprise","fear","disgust"))
replies_data$time <- as.POSIXct(replies_data$time)
quotes_data$sentimentA <- factor(quotes_data$sentimentA, levels=c("Very Negative", "Negative", "Neutral", "Positive","Very Positive"))
quotes_data$sentimentB <- factor(quotes_data$sentimentB, levels=c("joy","anger","sadness","surprise","fear","disgust"))
quotes_data$time <- as.POSIXct(quotes_data$time)

#############
# BEAUTIFUL #
#############

# shaded curve showing the volume of tweets over time
# vertical lines representing the number of retweets for individual tweets
    # divide by 10 and display only those with at least 10 retweets (62 total)
    # label lines with >300 retweets (6 total)

###################################
# VIZUALIZE TWEET STORM OVER TIME #
###################################

# Volume of tweets over time
reply_timebins <- cut.POSIXt(replies_data$time, breaks="hour")
quote_timebins <- cut.POSIXt(quotes_data$time, breaks="hour")
layout(matrix(1:2,2,1))
barplot(table(reply_timebins), main="Replies")
barplot(table(quote_timebins), main="Quotes")


# SentimentA
reply_tableA <- table(replies_data$sentimentA,reply_timebins)
reply_tableA2 <- reply_tableA
reply_tableA2[2,] <- reply_tableA2[1,]+reply_tableA2[2,]
reply_tableA2[4,] <- reply_tableA2[4,]+reply_tableA2[5,]
reply_tableA2 <- reply_tableA2[-c(1,5),]
quote_tableA <- table(quotes_data$sentimentA,quote_timebins)
quote_tableA2 <- quote_tableA
quote_tableA2[2,] <- quote_tableA2[1,]+quote_tableA2[2,]
quote_tableA2[4,] <- quote_tableA2[4,]+quote_tableA2[5,]
quote_tableA2 <- quote_tableA2[-c(1,5),]
layout(matrix(1:2,2,1))
barplot(reply_tableA2, beside = TRUE, col=c("red","gray","white"), main="Replies")
barplot(quote_tableA2, beside = TRUE, col=c("red","gray","white"), main="Quotes")

# SentimentB
reply_tableB <- table(replies_data$sentimentB, reply_timebins)
quote_tableB <- table(quotes_data$sentimentB, quote_timebins)
layout(matrix(1:2,2,1))
barplot(reply_tableB[1:3,], beside=TRUE, col=c("white","lightblue", "red"), main="Replies")
barplot(quote_tableB[1:3,], beside=TRUE, col=c("white","lightblue", "red"), main="Quotes")


#######
# END #
#######

