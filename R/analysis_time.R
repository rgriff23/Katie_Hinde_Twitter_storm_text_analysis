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

###################################
# VIZUALIZE TWEET STORM OVER TIME #
###################################

# Volume of tweets
rev(table(cut.POSIXt(clean_tweet_data$time, breaks=12)))

# SentimentA


# SentimentB

#######
# END #
#######

