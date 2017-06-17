################
# PREPARATIONS #
################

# load packages
library("twitteR")
library("ggplot2")

# import data
tweet_data <- read.csv("https://raw.githubusercontent.com/rgriff23/Katie_Hinde_Twitter_storm_text_analysis/master/data/tweet_data.csv", row.names=1)

# format data
tweet_data$sentimentA <- factor(tweet_data$sentimentA, levels=c("Very Negative", "Negative", "Neutral", "Positive","Very Positive"))
tweet_data$sentimentB <- factor(tweet_data$sentimentB, levels=c("joy","anger","sadness","surprise","fear","disgust"))
tweet_data$time <- as.POSIXct(tweet_data$time)

###################
# QUICK AND DIRTY #
###################

# Volume of tweets over time
timebins <- cut.POSIXt(tweet_data$time, breaks="hour")
barplot(table(timebins))


# SentimentA
tableA <- table(tweet_data$sentimentA,timebins)
tableA2 <- tableA
tableA2[2,] <- tableA2[1,]+tableA2[2,]
tableA2[4,] <- tableA2[4,]+tableA2[5,]
tableA2 <- tableA2[-c(1,5),]
# need to fix dates on x axis
barplot(tableA2, beside = TRUE, col=c("red","orange","yellow"), xlab="time", ylab="number of tweets")
legend("topright", legend=rownames(tableA2), fill=c("red","orange","yellow"))

# SentimentB
tableB <- table(tweet_data$sentimentB, timebins)
barplot(tableB, col=c("yellow","lightblue","red","orange","green","chocolate4"), xlab="time", ylab="number of tweets", main="stacked bars")
legend("topright", legend=rev(rownames(tableB)), fill=rev(c("yellow","lightblue","red","orange","green","chocolate4")))

#############
# BEAUTIFUL #
#############

# shaded curve showing the volume of tweets over time
# vertical lines representing the number of retweets for individual tweets
# divide by 10 and display only those with at least 10 retweets (62 total)
# label lines with >300 retweets (6 total)

#######
# END #
#######

