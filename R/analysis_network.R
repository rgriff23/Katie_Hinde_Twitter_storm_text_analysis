################
# PREPARATIONS #
################

# load packages
library("twitteR")
library("plyr")
library("igraph")

# import data
clean_tweet_data <- read.csv("https://raw.githubusercontent.com/rgriff23/Katie_Hinde_Twitter_storm_text_analysis/master/data/clean_tweet_data.csv", row.names=1)
social_network_user_data <- read.csv("https://raw.githubusercontent.com/rgriff23/Katie_Hinde_Twitter_storm_text_analysis/master/data/social_network_user_data.csv", row.names=1)

# format data
clean_tweet_data$sentimentA <- factor(clean_tweet_data$sentimentA, levels=c("Very Negative", "Negative", "Neutral", "Positive","Very Positive"))
clean_tweet_data$sentimentB <- factor(clean_tweet_data$sentimentB, levels=c("joy","sadness","anger","surprise","fear","disgust"))
clean_tweet_data$time <- as.POSIXct(clean_tweet_data$time)

# Twitter authentication (customize this for yourself to reproduce analysis)
source('~/Dropbox/Code/R/twitter_setup.R', chdir = TRUE)

########################
# GET CO-FOLLOWER DATA #
########################

# function for getting friends (use as a model)
#https://github.com/pablobarbera/twitter_ideology/blob/master/pkg/tweetscores/R/get-friends.R

# Empty lists to store users and friends
users <- friends <- list()

# vector of usernames
usernames <- clean_tweet_data$user[clean_tweet_data$friendCount<1000]
test <- getUser(usernames[1])

# Loop to get users (have to keep restarting it after hitting the rate/retry limit)
for (i in 1:length(usernames)) {
  temp <- try(getUser(usernames[i]))
  if (class(temp) == "try-error") {
    users[[i]] <- NA
  } else { 
    users[[i]] <- temp
  }
  print(i)
}

# Loop to get friends (THIS ISN'T WORKING YET)
for (i in 1:length(users)) {
  friends[[i]] <- users[i]$getFriendIDs()
  print(i)
}
names(list) <- usernames

##############################
# CREATE CO-FOLLOWER NETWORK #
##############################

# loop through each dyad, count matches
# assign count to appropriate cell of N x N matrix
# convert matrix to igraph network

#####################
# VIZUALIZE NETWORK #
#####################

# color nodes by emotional valence
# identify modules
# overlay text or images indicating popular cofollows

#######
# END #
#######

