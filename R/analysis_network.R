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

########################
# GET CO-FOLLOWER DATA #
########################

# Identify unique users and their sentiment 'score'
# Very Negative = -2, Negative = -1, Neutral = 0, Positive = 1, Very Positive = 2
sentiments <- c("Very Negative", "Negative", "Neutral", "Positive", "Very Positive")
score <- c(-2, -1, 0, 1, 2)
users <- ddply(clean_tweet_data, .(user), function (x) {
  sentimentA <- sum(as.numeric(as.character(mapvalues(x$sentimentA, sentiments, score))))
  data.frame(user=x$user[1], sentimentA=sentimentA)
})

# Remove users that follow too many people
numFriends <- c()
for (i in 2440:nrow(users)) {
  temp <- try(getUser(users$user[i])$friendsCount)
  if (class(temp) != "try-error") {
    numFriends[i] <- temp
  } else {numFriends[i] <- NA}
  print(i)
}
hist(numFriends)
nrow(users[numFriends<2000,])/nrow(users) # keeps over 90% of users
users_trimmed <- users[numFriends<2000,]
# Export data for users to be included in the social network
write.csv(users_trimmed, file="~/Desktop/GitHub/Katie_Hinde_Twitter_storm_text_analysis/data/social_network_user_data.csv")

# Empty list to store friends
friends <- list()

# Get 'friends' (THIS ISN'T WORKING YET)
for (i in 1:nrow(users_trimmed)) {
  temp <- try(getUser(users_trimmed$user[i]))
  if (class(temp) != "try-error") {
    temp2 <- try(temp$getFriendIDs())
    if (class(temp2) != "try-error") {
      friends[[i]] <- temp2
    } else  {friends[[i]] <- NA}
  } else {friends[[i]] <- NA}
  print(i)
}
names(list) <- users$user

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

