################
# PREPARATIONS #
################

# load packages
library("twitteR")
library("igraph")
library("plyr")

# Twitter authentication (customize this for yourself to reproduce analysis)
source('~/Dropbox/Code/R/twitter_setup.R', chdir = TRUE)

# import and format tweet data
#tweet_data <- read.csv("https://raw.githubusercontent.com/rgriff23/Katie_Hinde_Twitter_storm_text_analysis/master/data/tweet_data.csv", row.names=1)
tweet_data <- read.csv("~/Desktop/GitHub/Katie_Hinde_Twitter_storm_text_analysis/data/tweet_data.csv")
tweet_data$sentimentA <- factor(tweet_data$sentimentA, levels=c("Very Negative", "Negative", "Neutral", "Positive","Very Positive"))
tweet_data$sentimentB <- factor(tweet_data$sentimentB, levels=c("joy","sadness","anger","surprise","fear","disgust"))
tweet_data$time <- as.POSIXct(tweet_data$time, format="%Y-%m-%d %H:%M:%S")

# import and format user-level data
user_data <- read.csv("~/Desktop/GitHub/Katie_Hinde_Twitter_storm_text_analysis/data/user_data.csv")
user_data$time_join <- as.POSIXct(user_data$time_join, format="%Y-%m-%d %H:%M:%S")

######################################
# GET FRIEND DATA FOR SOCIAL NETWORK #
######################################

# import social network friend list
friends <- readRDS("~/Desktop/GitHub/Katie_Hinde_Twitter_storm_text_analysis/data/friend_list.rds")

# get users (run in parallel with multiple OAuth to speed up)
#friends <- list()
#lookup <- unique(tweet_data$user[tweet_data$friendCount < 2000]) # keeps 92.4% of users (4307)
#for (i in 1:length(lookup)) {
#  temp <- try(getUser(lookup[i])$getFriendIDs(retryOnRateLimit=100))
#  if (class(temp)=="try-error") {friends[[i]] <- NA} else {friends[[i]] <- temp}
#  print(i)
#}

# get rid of NULL/NA users
#sum(sapply(friends, function(x) length(x)==1)) # drop 316
#friends <- friends[!sapply(friends, function(x) length(x)==1)] # 3992 remaining

##############################
# CREATE CO-FOLLOWER NETWORK #
##############################

# import graph
g <- readRDS("~/Desktop/GitHub/Katie_Hinde_Twitter_storm_text_analysis/data/graph_full.rds")

# create edge list matrix
#l <- length(friends)
#mat <- c()
#for (i in 1:(l-1)) {
#  temp <- sapply(friends[(i+1):l], function (x) {sum(friends[[i]] %in% x)})
#  sink <- ((i+1):l)[temp>0]
#  source <- rep(i, length(sink))
#  weight <- temp[temp>0]
#  temp <- cbind(source, sink, weight)
#  dimnames(temp) <- NULL
#  mat <- rbind(mat, cbind(source, sink, weight))
#  print(i)
#}
#nrow(mat) # number of edges = 4,542,165
#nrow(mat)/((l^2 - l)/2) # percent edges = 57%

# create graph
#g <- graph_from_edgelist(mat[,1:2], directed=FALSE)
#length(E(g)) # 4,542,165
#length(V(g)) # 3992
#E(g)$weight <- mat[,3] # add edge weights
#V(g)$label <- names(friends) # add vertex labels

# export full edge list matrix and graph
#saveRDS(mat, "~/Desktop/GitHub/Katie_Hinde_Twitter_storm_text_analysis/data/edge_matrix_full.rds")
#saveRDS(g, "~/Desktop/GitHub/Katie_Hinde_Twitter_storm_text_analysis/data/graph_full.rds")

#############################################
# MODULARITY ANALYSIS AND ADDING ATTRIBUTES #
#############################################

# function to test different clustering algorithms and edge-weight cutoffs
# algorithm options: cluster_label_prop, cluster_walktrap, cluster_fast_greedy
test.structure <- function (graph, cutoff, algorithm="cluster_label_prop") {
  graph <- delete_edges(graph, E(graph)[weight<cutoff])
  if (algorithm == "cluster_label_prop") {comm <- cluster_label_prop(graph)}
  if (algorithm == "cluster_walktrap") {comm <- cluster_walktrap(graph)}
  if (algorithm == "cluster_fast_greedy") {comm <- cluster_fast_greedy(graph)}
  return(sapply(communities(comm), length))
}
test.structure(g, 50, "cluster_label_prop")
# cluster_label_prop with cutoff=10 yields 2 major communities
# cluster_walktrap with cutoff=0+ yields 3 major communities (1000, 1250, 1700)
# cluster_fast_greedy with cutoff=0+ yields 3 major communities (350, 1500, 2000)

# extract greedy cluster membership information
greedy <- cluster_fast_greedy(g)
greedy_comm <- communities(greedy)
greedy_mem <- membership(greedy)
table(greedy_mem) # size of the clusters

# label users that made the most influential tweets
# @Mammals_Suck, @upthetwerx, @PrisonPlanet, @surfbordt, @AskTarget, @lawilson009, @LiteralSalt
head(tweet_data[order(tweet_data$retweets, decreasing = TRUE),1:5],6)
special <- c("Mammals_Suck","upthetwerx","PrisonPlanet","surfbordt","AskTarget","lawilson009","LiteralSalt")
special %in% V(g)$label
V(g)$label_special <- ifelse(V(g)$label %in% special, V(g)$label, NA)

# add time_bin in which the user joined the social network
V(g)$time_bin <- user_data[match(V(g)$label, user_data$user),"time_bin"]

# size nodes based on popularity (but no 0's allowed)
pop <- user_data[match(V(g)$label, user_data$user),"followers"]
pop[is.na(pop)] <- 0
V(g)$size <- round(seq(1,10,length.out=(max(V(g)$size,na.rm=T)+1))[V(g)$size],2)

# add cluster membership to vertex attributes
V(g)$membership <- greedy_mem

# color nodes based on module membership
V(g)$color <- c("gray","red","blue", rep("white",6))[greedy_mem]
V(g)$frame.color <- NA

# color edges based on edge weight
gry_blk <- colorRampPalette(c("lightgray","black"))
gry_blk <- gry_blk(659)
E(g)$color <- gry_blk[E(g)$weight]

# keep major connected component
#conn <- components(g)$membership
#g <- delete_vertices(g, conn>1)

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
topfriendsSN1 # idk?
topfriendsSN2 # the right
topfriendsSN3 # the left

#########################################
# ADD CLUSTER INFORMATION TO TWEET DATA #
#########################################

# export updated tweet_data
tweet_data$cluster <- 0
for (i in 1:nrow(tweet_data)) {
  if (tweet_data$user[i] %in% V(gf)$label) {
    tweet_data$cluster[i] <- V(gf)$membership[which(V(gf)$label==tweet_data$user[i])]
  } else (tweet_data$cluster[i] <- NA)
}


# export updated tweet_data
#write.csv(tweet_data, "~/Desktop/GitHub/Katie_Hinde_Twitter_storm_text_analysis/data/tweet_data.csv")

############################################
# FIND GOOD LAYOUT FOR VIZUALIZING NETWORK #
############################################

# read fancy graph
g <- readRDS("~/Desktop/GitHub/Katie_Hinde_Twitter_storm_text_analysis/data/graph_fancy.rds")

# awesome tutorial
#http://kateto.net/network-visualization

# drop small clusters (only keep clusters 1-3)
g <- delete_vertices(g, V(g)$membership>3)

# set graph layout
E(g)$weight <- log1p(E(g)$weight)
layout_drl <- layout_with_drl(g) # about 25-30 mins
fivenum(layout_drl[,1])
fivenum(layout_drl[,2])
saveRDS(layout_drl, "~/Desktop/GitHub/Katie_Hinde_Twitter_storm_text_analysis/data/graph_layout.rds")

# remove edges for plotting
g2 <- delete_edges(g, E(g))

# plot
plot(g2, ylim=c(-1,0.5), xlim=c(-0.5,1), vertex.label=V(g2)$label_special, vertex.label.cex=1, vertex.label.color="black", vertex.label.font=2, layout=layout_drl)

# time plot
gry_blk <- colorRampPalette(c("white","black"))
gry_blk <- gry_blk(12)
V(g)$color2 <- gry_blk[V(g)$time_bin]
plot(g2, ylim=c(-1,0.5), xlim=c(-0.5,1), vertex.color=V(g)$color2, vertex.label=V(g2)$label_special, vertex.label.cex=1, vertex.label.color="red", vertex.label.font=2, layout=layout_drl)


# FOR MOVIE
# no edges (edge.color=NA)
# nodes appear in 3 hour intervals


#######
# END #
#######



