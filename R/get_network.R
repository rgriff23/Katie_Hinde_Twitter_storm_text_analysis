# NOTE: the network animation requires that the Image Magick software is installed.
# See here for instructions on installing Image Magick: https://www.imagemagick.org/script/download.php 
# Since I'm working on a Mac, I did this using the Macports method.

################
# PREPARATIONS #
################

# load packages
library("twitteR")
library("igraph")
library("plyr")

# My Twitter authentication (customize for yourself)
# source('~/Dropbox/Code/R/twitter_setup.R', chdir = TRUE)
# Tutorial on authenticating: https://www.r-bloggers.com/getting-started-with-twitter-in-r/

# import and format tweet data
tweet_data <- read.csv("https://raw.githubusercontent.com/rgriff23/Katie_Hinde_Twitter_storm_text_analysis/master/data/tweet_data.csv")
tweet_data$sentimentA <- factor(tweet_data$sentimentA, levels=c("Very Negative", "Negative", "Neutral", "Positive","Very Positive"))
tweet_data$sentimentB <- factor(tweet_data$sentimentB, levels=c("joy","sadness","anger","surprise","fear","disgust"))
tweet_data$time <- as.POSIXct(tweet_data$time, format="%Y-%m-%d %H:%M:%S")

###################
# USER-LEVEL DATA #
###################

# create user level data
user_data <- ddply(tweet_data, .(user), function(x) {
  time_join <- min(x$time)
  time_bin <- x[which.min(x$time),"time_bin"]
  followers <- x$followerCount[1]
  data.frame(time_join, time_bin, followers)
})

######################################
# GET FRIEND DATA FOR SOCIAL NETWORK #
######################################

# get users (run in parallel with multiple OAuth to speed up)
friends <- list()
lookup <- unique(tweet_data$user[tweet_data$friendCount < 2000]) # keeps 92.4% of users (4307)
for (i in 1:length(lookup)) {
  temp <- try(getUser(lookup[i])$getFriendIDs(retryOnRateLimit=100))
  if (class(temp)=="try-error") {friends[[i]] <- NA} else {friends[[i]] <- temp}
  print(i)
}

# drop NULL/NA users
sum(sapply(friends, function(x) length(x)==1)) # drop 316
friends <- friends[!sapply(friends, function(x) length(x)==1)] # 3992 remaining

# export friend list
# saveRDS(friends, file="~/Desktop/GitHub/Katie_Hinde_Twitter_storm_text_analysis/data/friend_list.rds")

##############################
# CREATE CO-FOLLOWER NETWORK #
##############################

# create edge list matrix
l <- length(friends)
mat <- c()
for (i in 1:(l-1)) {
  temp <- sapply(friends[(i+1):l], function (x) {sum(friends[[i]] %in% x)})
  sink <- ((i+1):l)[temp>0]
  source <- rep(i, length(sink))
  weight <- temp[temp>0]
  temp <- cbind(source, sink, weight)
  dimnames(temp) <- NULL
  mat <- rbind(mat, cbind(source, sink, weight))
  print(i)
}
nrow(mat) # number of edges = 4,542,165
nrow(mat)/((l^2 - l)/2) # percent edges = 57%

# create graph
g <- graph_from_edgelist(mat[,1:2], directed=FALSE)
length(E(g)) # 4,542,165
length(V(g)) # 3992
E(g)$weight <- mat[,3] # add edge weights
V(g)$label <- names(friends) # add vertex labels

# export full edge list matrix and graph
#saveRDS(g, "~/Desktop/GitHub/Katie_Hinde_Twitter_storm_text_analysis/data/graph_basic.rds")

####################
# CLUSTER ANALYSIS #
####################

# extract greedy cluster membership information
greedy <- cluster_fast_greedy(g)
greedy_comm <- communities(greedy)
greedy_mem <- membership(greedy)
table(greedy_mem) # size of the clusters

##################################
# ADD VERTEX AND EDGE ATTRIBUTES #
##################################

### VERTEX ATTRIBUTES

# add cluster membership to vertex attributes
V(g)$membership <- greedy_mem

# color nodes based on cluster membership
V(g)$color <- c("gray","red","blue", rep("white",6))[greedy_mem]
V(g)$frame.color <- NA

# label users that made the most influential tweets
# @Mammals_Suck, @upthetwerx, @PrisonPlanet, @surfbordt, @AskTarget, @lawilson009, @LiteralSalt
head(tweet_data[order(tweet_data$retweets, decreasing = TRUE),1:5],6)
special <- c("Mammals_Suck","upthetwerx","surfbordt", "PrisonPlanet","AskTarget","lawilson009","LiteralSalt")
special %in% V(g)$label
V(g)$label_special <- ifelse(V(g)$label %in% special, V(g)$label, NA)

# add time_bin in which the user joined the social network
V(g)$time_bin <- user_data[match(V(g)$label, user_data$user),"time_bin"]

# size nodes based on popularity (but no 0's allowed)
pop <- user_data[match(V(g)$label, user_data$user),"followers"]
pop[is.na(pop)] <- 0
V(g)$size <- round(seq(1,10,length.out=(max(V(g)$size,na.rm=T)+1))[V(g)$size],2)

### EDGE ATTRIBUTES

# color edges based on edge weight
gry_blk <- colorRampPalette(c("lightgray","black"))
gry_blk <- gry_blk(659)
E(g)$color <- gry_blk[E(g)$weight]

# export fancy graph
#saveRDS(g, "~/Desktop/GitHub/Katie_Hinde_Twitter_storm_text_analysis/data/graph_fancy.rds")

###############################
# TOP FRIENDS IN EACH CLUSTER #
###############################

# function to find the N most popular friends within each major cluster (1-3)
popular.friends <- function (graph, membershipVector, friendsList, clusterID, N) {
  clust <- V(g)[membershipVector==clusterID]$label
  friend_tab <- sort(table(unlist(friendsList[clust])), decreasing=TRUE)
  i <- 1
  topfriends <- c()
  while (length(topfriends) < N) {
    temp <- try(getUser(names(friend_tab)[i]))
    if (class(temp)!="try-error") {topfriends <- append(topfriends, temp$screenName)} 
    i <- i + 1
  }
  return(topfriends)
}
topfriends1 <- popular.friends(g, greedy_mem, friends, 1, 25)
topfriends2 <- popular.friends(g, greedy_mem, friends, 2, 25)
topfriends3 <- popular.friends(g, greedy_mem, friends, 3, 25)

#########################################
# ADD CLUSTER INFORMATION TO TWEET DATA #
#########################################

# export updated tweet_data
tweet_data$cluster <- 0
for (i in 1:nrow(tweet_data)) {
  if (tweet_data$user[i] %in% V(g)$label) {
    tweet_data$cluster[i] <- V(g)$membership[which(V(g)$label==tweet_data$user[i])]
  } else (tweet_data$cluster[i] <- NA)
}

# export updated tweet_data
#write.csv(tweet_data, "~/Desktop/GitHub/Katie_Hinde_Twitter_storm_text_analysis/data/tweet_data.csv", row.names = FALSE)

############################################
# FIND GOOD LAYOUT FOR VIZUALIZING NETWORK #
############################################

# helpful tutorial: http://kateto.net/network-visualization

# drop small clusters (only keep clusters 1-3)
g <- delete_vertices(g, V(g)$membership>3)

# set graph layout (log transform edge weights first)
E(g)$weight <- log1p(E(g)$weight)
layout_drl <- layout_with_drl(g) # about 25-30 mins

# export graph layout
#saveRDS(layout_drl, "~/Desktop/GitHub/Katie_Hinde_Twitter_storm_text_analysis/data/graph_layout.rds")

#######
# END #
#######



