# NOTE: tweets get cut off at 140 characters, which is problematic since some tweets are actually longer
# given Twitter's new features that don't count certain characters in the character count.
# I don't know how to adjust this with the 'searchTwitter' function, so for now this is just an unfortunate
# shortcoming of the analysis. Fortunately, much of the time it is only a hyperlink that gets cut off, which
# is eliminated during data cleaning anyway.

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
library("Rstem") # dependency for sentiment
library("sentiment") # naive bayes
library("RSentiment") # calculate_sentiment
library("plyr") 

# Twitter authentication (customize this for yourself to reproduce analysis)
source('~/Dropbox/Code/R/twitter_setup.R', chdir = TRUE)

##########################
# GET REPLIES AND QUOTES #
##########################

# ID of The Tweet
id <- "874116254767865856"

# Get mentions of @Mammals_Suck prior to the blog post ~36 hours later (rate limited 14 times, 33261 returned)
#tweets <- searchTwitter("@Mammals_Suck", sinceID=id, maxID="874656504204279808", n=50000, retryOnRateLimit=20)
#load('/Users/nunnlab/Desktop/GitHub/Katie_Hinde_Twitter_storm_text_analysis/Rdata/all_tweets_33261.RData') # 33261 tweets

# Extract direct replies (must begin with "@Mammals_Suck" and reply to the original tweet)
replies <- lapply(tweets, function (x) {
  if (length(x$replyToSID) == 1) {
    if (x$replyToSID == id) {
      if (substr(x$text, 1, 13)=="@Mammals_Suck") {
        return (TRUE)
      } else return (FALSE)
    } else return(FALSE)
  } else return(FALSE)
})
replies <- tweets[unlist(replies)] # 2936

# Get quote tweets (these don't mention @Mammals_Suck)
quotes <- searchTwitter("https://twitter.com/Mammals_Suck/status/874116254767865856", sinceID=id, maxID="874656504204279808", n=50000, retryOnRateLimit=5) #8270
quotes <- strip_retweets(quotes2) #2281

# Function to clean tweets
clean_tweets <- function (tweet_list) {
  lapply(tweet_list, function (x) {
    x <- x$getText() # get text alone
    x <- gsub("&amp", "", x) # rm ampersands
    x <- gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*) ?", "", x) # rm links
    x <- gsub("#\\w+", "", x) # rm hashtags
    x <- gsub("@\\w+", "", x) # rm usernames
    x <- iconv(x, "latin1", "ASCII", sub="") # rm emojis
    x <- gsub("[[:punct:]]", "", x) # rm punctuation
    x <- gsub("[[:digit:]]", "", x) # rm numbers
    x <- gsub("[ \t]{2}", " ", x) # rm tabs
    x <- gsub("\\s+", " ", x) # rm extra spaces
    x <- trimws(x) # rm leading and trailing white space
    x <- tolower(x) # convert to lower case
  })
}

# Clean tweet text 
replies_clean <- unlist(clean_tweets(replies))
quotes_clean <- unlist(clean_tweets(quotes))

# Combine text with metadata (user, time, favorites, retweets) and drop empty text
replies_data <- data.frame(text=replies_clean)
replies_data$user <- unlist(lapply(replies, function(x) x$screenName))
replies_data$time <- do.call("c", lapply(replies, function(x) x$created))
replies_data$favorites <- unlist(lapply(replies, function(x) x$favoriteCount))
replies_data$retweets <- unlist(lapply(replies, function(x) x$retweetCount))
replies_data <- replies_data[replies_data$text != "",] # 2726
quotes_data <- data.frame(text=quotes_clean)
quotes_data$user <- unlist(lapply(quotes, function(x) x$screenName))
quotes_data$time <- do.call("c", lapply(quotes, function(x) x$created))
quotes_data$favorites <- unlist(lapply(quotes, function(x) x$favoriteCount))
quotes_data$retweets <- unlist(lapply(quotes, function(x) x$retweetCount))
quotes_data <- quotes_data[quotes_data$text != "",] # 2117

#write.csv(replies_data, file="~/Desktop/GitHub/Katie_Hinde_Twitter_storm_text_analysis/data/replies_data.csv")
#write.csv(quotes_data, file="~/Desktop/GitHub/Katie_Hinde_Twitter_storm_text_analysis/data/quotes_data.csv")

######################
# SENTIMENT ANALYSIS #
######################

# combine data
tweet_data <- rbind(replies_data, quotes_data)
tweet_data$type <- c(rep("reply", nrow(replies_data)), rep("quote", nrow(quotes_data)))

# Empty columns to store sentiments of each tweet
tweet_data$sentimentA <- tweet_data$sentimentB <- rep("0", nrow(tweet_data))

# Very negative/negative/neutral/positive/very positive 
for (i in 1:nrow(tweet_data)) {
  tweet_data$sentimentA[i] <- as.character(calculate_sentiment(tweet_data$text[i])$sentiment)
  print(i)
}

# Sad/disgusted/angry/fearful/surprised/joyful 
for (i in 1:nrow(tweet_data)) {
  tweet_data$sentimentB[i] <- classify_emotion(tweet_data$text[i], algorithm="bayes", prior=1)[,"BEST_FIT"]
  print(i)
}
sum(is.na(tweet_data$sentimentB))/nrow(tweet_data) # 65.3% unclassified

#write.csv(tweet_data, file="~/Desktop/GitHub/Katie_Hinde_Twitter_storm_text_analysis/data/tweet_data.csv")

########################################
# ADD TIME AND USER DATA TO TWEET DATA #
########################################

# Empty columns to store location, follower counts, and descriptions
tweet_data$followerCount <- tweet_data$friendCount <- rep(0, nrow(tweet_data))
tweet_data$location <- tweet_data$description <- rep("0", nrow(tweet_data))

# Loop to get data (have to keep restarting it after hitting the rate/retry limit of 900/15 mins)
for (i in 4501:nrow(tweet_data)) {
  temp <- try(getUser(tweet_data$user[i]))
  if (class(temp) == "try-error") {
    tweet_data$followerCount[i] <- NA
    tweet_data$friendCount[i] <- NA
    tweet_data$description[i] <- NA
    tweet_data$location[i] <- NA
  } else {
    tweet_data$followerCount[i] <- temp$followersCount
    tweet_data$friendCount[i] <- temp$friendsCount
    tweet_data$description[i] <- temp$description
    tweet_data$location[i] <- temp$location
  }
  print(i)
}

# Clean description data (similar to tweets)
backup <- tweet_data$description
tweet_data$description <- lapply(tweet_data$description, function (text) {
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
tweet_data$description <- unlist(tweet_data$description)

# add time bins (3 hour intervals, for 12 total intervals)
tweet_data$time_bin <- cut.POSIXt(tweet_data$time, breaks="3 hours", labels = FALSE)

#write.csv(tweet_data, file="~/Desktop/GitHub/Katie_Hinde_Twitter_storm_text_analysis/data/tweet_data.csv", row.names = FALSE)

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

# export user data
#write.csv(user_data, file="~/Desktop/GitHub/Katie_Hinde_Twitter_storm_text_analysis/data/user_data.csv", row.names = FALSE)

#######
# END #
#######