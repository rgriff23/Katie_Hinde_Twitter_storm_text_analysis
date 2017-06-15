################
# PREPARATIONS #
################

# install Rstem and sentiment package (not available on CRAN)
#library("devtools")
#install.packages("Rstem", repos = "http://www.omegahat.net/R")
#install_url("https://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")

# Load packages
library("twitteR")
library("tm")
library("wordcloud")
library("sentiment")
library("Rstem")
library("plyr")

# Twitter authentication (customize this for yourself to reproduce analysis)
source('~/Dropbox/Code/R/twitter_setup.R', chdir = TRUE)

############
# GET DATA #
############

# ID of The Tweet
id <- "874116254767865856"

# Get replies to The Tweet prior to the blog post ~36 hours later 
#tweets <- searchTwitter("@Mammals_Suck", sinceID=id, maxID="874656504204279808", n=11000, retryOnRateLimit=100)
reply <- lapply(tweets, function (x) {ifelse(x$replyToSID == id, TRUE, FALSE)})
reply <- tweets[unlist(reply)] # 3821

# Get retweets- not working- 'Forbidden (HTTP 403)'???
#retweet <- retweets(showStatus(id)$getId(), n=30)

# Clean text
tweet_text <- reply
clean_tweets <- lapply(tweet_text, function (tweet_text) {
  tweet_text <- tweet_text$getText() # get text alone
  tweet_text <- gsub("&amp", "", tweet_text) # rm ampersands
  tweet_text <- gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*) ?", "", tweet_text) # rm links
  tweet_text <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweet_text) # rm RT info
  tweet_text <- gsub("#\\w+", "", tweet_text) # rm hashtags
  tweet_text <- gsub("@\\w+", "", tweet_text) # rm usernames
  tweet_text <- iconv(tweet_text, "latin1", "ASCII", sub="") # rm emojis
  tweet_text <- gsub("[[:punct:]]", "", tweet_text) # rm punctuation
  tweet_text <- gsub("[[:digit:]]", "", tweet_text) # rm numbers
  tweet_text <- gsub("[ \t]{2}", " ", tweet_text) # rm tabs
  tweet_text <- gsub("\\s+", " ", tweet_text) # rm extra spaces
  tweet_text <- trimws(tweet_text) # rm leading and trailing white space
  tweet_text <- tolower(tweet_text) # convert to lower case
})

# Combine text with metadata (user, time, favorites, retweets)
clean_tweet_data <- data.frame(text=unlist(clean_tweets))
clean_tweet_data$user <- unlist(lapply(reply, function(x) x$screenName))
clean_tweet_data$time <- do.call("c", lapply(reply, function(x) x$created))
clean_tweet_data$favorites <- unlist(lapply(reply, function(x) x$favoriteCount))
clean_tweet_data$retweets <- unlist(lapply(reply, function(x) x$retweetCount))
clean_tweet_data <- clean_tweet_data[clean_tweet_data$text != "",] # 3625

# Make corpus and quick word cloud
# one issue is that certain words should be grouped together, e.g., minimum-wage
# another issue is that certain words should be removed, e.g., also, way, sure
corpus <- Corpus(VectorSource(clean_tweet_data$text))
corpus <- tm_map(tm_map(corpus, removeWords, stopwords('english')), stemDocument)
wordcloud(corpus$content, max.words = 100, colors=rainbow(10), random.order = FALSE)

######################
# SENTIMENT ANALYSIS #
######################

# Empty columns to store sentiments of each tweet
clean_tweet_data$sentimentA <- clean_tweet_data$sentimentB <- rep("0", nrow(clean_tweet_data))

# Very negative/negative/neutral/positive/very positive 
for (i in 1:nrow(clean_tweet_data)) {
  clean_tweet_data$sentimentA[i] <- as.character(calculate_sentiment(clean_tweet_data$text[i])$sentiment)
  print(i)
}
clean_tweet_data$sentimentA <- factor(clean_tweet_data$sentimentA, levels=c("Very Negative", "Negative", "Neutral", "Positive","Very Positive"))
barplot(table(clean_tweet_data$sentimentA))

# Sad/disgusted/angry/fearful/surprised/joyful 
for (i in 1:nrow(clean_tweet_data)) {
  clean_tweet_data$sentimentB[i] <- classify_emotion(clean_tweet_data$text[i], algorithm="bayes", prior=1)[,"BEST_FIT"]
  print(i)
}
sum(is.na(clean_tweet_data$sentimentB)) # 64% left unclassified
clean_tweet_data$sentimentB <- factor(clean_tweet_data$sentimentB, levels=c("joy","anger","sadness","surprise","fear","disgust"))
barplot(table(clean_tweet_data$sentimentB))

######################
# GET MORE USER DATA #
######################

# Empty columns to store GIS coordinates, follower counts, and descriptions
clean_tweet_data$followerCount <- rep(0, nrow(clean_tweet_data))
clean_tweet_data$location <- clean_tweet_data$description <- rep("0", nrow(clean_tweet_data))

# Loop to get data (have to keep restarting it after hitting the rate/retry limit)
for (i in 1:nrow(clean_tweet_data)) {
  temp <- try(getUser(clean_tweet_data$user[i]))
  if (class(temp) == "try-error") {
    clean_tweet_data$location[i] <- NA
    clean_tweet_data$followerCount[i] <- NA
    clean_tweet_data$description[i] <- NA
  } else {
    clean_tweet_data$location[i] <- temp$location
    clean_tweet_data$followerCount[i] <- temp$followersCount
    clean_tweet_data$description[i] <- temp$description
  }
  print(i)
}

# Clean location data (https://www.r-bloggers.com/mapping-twitter-followers-in-r/)
# Do the geocoding in another R script (it's a lot)
clean_tweet_data$location[clean_tweet_data$location ==""] <- NA

# Clean description data (similar to tweets)
clean_tweet_data$description <- lapply(desc_text, function (text) {
  text <- gsub("&amp", "", text) # rm ampersands
  text <- gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*) ?", "", text) # rm links
  text <- gsub("@\\w+", "", text) # rm usernames
  text <- iconv(text, "latin1", "ASCII", sub="") # rm emojis
  text <- gsub("[[:punct:]]", "", text) # rm punctuation
  text <- gsub("[[:digit:]]", "", text) # rm numbers
  text <- gsub("[ \t]{2}", " ", text) # rm tabs
  text <- gsub("\\s+", " ", text) # rm extra spaces
  text <- trimws(text) # rm leading and trailing white space
  text <- tolower(text) # convert to lower case
})
clean_tweet_data$description <- unlist(clean_tweet_data$description)

# Export data frame
write.csv(clean_tweet_data, file="~/Desktop/GitHub/Katie_Hinde_Twitter_storm_text_analysis/data/clean_tweet_data.csv")

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

# Create co-follower matrix

# Export co-follower matrix


#######
# END #
#######