################
# PREPARATIONS #
################

# load packages
library("twitteR")
library("plyr")
library("igraph")

# import data
tweet_data <- read.csv("https://raw.githubusercontent.com/rgriff23/Katie_Hinde_Twitter_storm_text_analysis/master/data/tweet_data.csv", row.names=1)
#social_network_user_data <- read.csv("https://raw.githubusercontent.com/rgriff23/Katie_Hinde_Twitter_storm_text_analysis/master/data/social_network_user_data.csv", row.names=1)

# format data
tweet_data$sentimentA <- factor(tweet_data$sentimentA, levels=c("Very Negative", "Negative", "Neutral", "Positive","Very Positive"))
tweet_data$sentimentB <- factor(tweet_data$sentimentB, levels=c("joy","sadness","anger","surprise","fear","disgust"))
tweet_data$time <- as.POSIXct(tweet_data$time)

# Twitter authentication (customize this for yourself to reproduce analysis)
source('~/Dropbox/Code/R/twitter_setup.R', chdir = TRUE)

########################
# GET CO-FOLLOWER DATA #
########################

# get users (CHANGE TO FRIENDS)
users <- list()
lookup <- unique(tweet_data$user[tweet_data$friendCount < 2000]) # keeps 92.4% of users (4307)
for (i in 1:length(lookup)) {
  temp <- try(getUser(lookup[i])$getFollowerIDs(retryOnRateLimit=100))
  if (class(temp)=="try-error") {
    if(grep("HTTP 404", test[1])==1) {users[[i]] <- NA} else {stop("Rate limit reached")}
  } else {users[[i]] <- temp}
  print(i)
}
#sum(sapply(users, function(x) is.na(x)))
#users <- users[sapply(users, function(x) is.na(x))]

# function for getting friends (use as a model)
#https://github.com/pablobarbera/twitter_ideology/blob/master/pkg/tweetscores/R/get-friends.R

##############################
# CREATE CO-FOLLOWER NETWORK #
##############################

# create empty graph

# loop through each dyad, count matches, and add edges to graph
for (i in 1:length(friends)) {
  for (j in (1+i):length(friends)) {
    w <- length(intersect(friends[[i]], friends[[j]]))
    if (w > 0) {
      # add edge to the graph
    }
  }
}

#######################
# MODULARITY ANALYSIS #
#######################



#####################
# VIZUALIZE NETWORK #
#####################

# color nodes by emotional valence
# show modules
# overlay text or images indicating popular cofollows

#######
# END #
#######

