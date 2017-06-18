################
# PREPARATIONS #
################

# load packages
library("twitteR")
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

# get users 
friends <- list()
lookup <- unique(tweet_data$user[tweet_data$friendCount < 2000]) # keeps 92.4% of users (4307)
for (i in 1:length(lookup)) {
  temp <- try(getUser(lookup[i])$getFollowerIDs(retryOnRateLimit=100))
  if (class(temp)=="try-error") {friends[[i]] <- NA} else {friends[[i]] <- temp}
  print(i)
}
#sum(sapply(users, function(x) is.na(x[[1]])))
#users <- users[!sapply(users, function(x) is.na(x[[1]]))]

# function for getting friends (use as a model)
#https://github.com/pablobarbera/twitter_ideology/blob/master/pkg/tweetscores/R/get-friends.R

##############################
# CREATE CO-FOLLOWER NETWORK #
##############################

# create empty matrix
mat <- c()

# loop through each dyad, count matches, and add edges to graph
friends=toy
for (i in 1:(length(friends)-1)) {
  for (j in (1+i):length(friends)) {
    w <- length(intersect(friends[[i]], friends[[j]]))
    if (w > 0) {mat <- rbind(mat, c(i,j,w))}
  }
}

# create graph
g <- graph_from_edgelist(mat[,1:2], directed=FALSE)
E(g)$weight <- mat[,3]

# plot
plot(g, edge.width=edge_attr(g)$weight, edge.color="black")

#######################
# MODULARITY ANALYSIS #
#######################

# identify modules
modules <- cluster_spinglass(g)

# find the most popular friends within each module

#####################
# VIZUALIZE NETWORK #
#####################

# plot with modules, no vertex labels or edge widths
plot(g, vertex.label=c(NA,NA,"3",NA), mark.groups=communities(modules))

# color nodes by emotional valence
# overlay text or images indicating popular mutual friends
# identify @Mammals_Suck (and other nodes of interest) by creating labels only for them (other nodes are NA)

#######
# END #
#######

