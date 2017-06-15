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

# Get direct replies to The Tweet prior to the blog post ~36 hours later 
tweets <- searchTwitter("@Mammals_Suck", sinceID=id, maxID="874656504204279808", n=11000, retryOnRateLimit=100)
reply <- lapply(tweets, function (x) {
  if (length(x$replyToSID) == 1) {
    if (x$replyToSID == id) {
      if (substr(x$text, 1, 13)=="@Mammals_Suck") {
        return (TRUE)
      } else return (FALSE)
    } else return(FALSE)
  } else return(FALSE)
  })
reply <- tweets[unlist(reply)] # 2950

# Clean tweet text
clean_tweets <- lapply(reply, function (x) {
  x <- x$getText() # get text alone
  x <- gsub("&amp", "", x) # rm ampersands
  x <- gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*) ?", "", x) # rm links
  x <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", x) # rm RT info
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

# Combine text with metadata (user, time, favorites, retweets)
clean_tweet_data <- data.frame(text=unlist(clean_tweets))
clean_tweet_data$user <- unlist(lapply(reply, function(x) x$screenName))
clean_tweet_data$time <- do.call("c", lapply(reply, function(x) x$created))
clean_tweet_data$favorites <- unlist(lapply(reply, function(x) x$favoriteCount))
clean_tweet_data$retweets <- unlist(lapply(reply, function(x) x$retweetCount))
clean_tweet_data <- clean_tweet_data[clean_tweet_data$text != "",] # 2737

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

# Sad/disgusted/angry/fearful/surprised/joyful 
for (i in 1:nrow(clean_tweet_data)) {
  clean_tweet_data$sentimentB[i] <- classify_emotion(clean_tweet_data$text[i], algorithm="bayes", prior=1)[,"BEST_FIT"]
  print(i)
}
sum(is.na(clean_tweet_data$sentimentB))/nrow(clean_tweet_data) # 64% unclassified

######################
# GET MORE USER DATA #
######################

# Empty columns to store GIS coordinates, follower counts, and descriptions
clean_tweet_data$followerCount <- rep(0, nrow(clean_tweet_data))
clean_tweet_data$location <- clean_tweet_data$description <- rep("0", nrow(clean_tweet_data))

# Loop to get data (have to keep restarting it after hitting the rate/retry limit)
for (i in 2701:nrow(clean_tweet_data)) {
  temp <- try(getUser(clean_tweet_data$user[i]))
  if (class(temp) == "try-error") {
    clean_tweet_data$followerCount[i] <- NA
    clean_tweet_data$friendCount[i] <- NA
    clean_tweet_data$description[i] <- NA
    clean_tweet_data$location[i] <- NA
  } else {
    clean_tweet_data$followerCount[i] <- temp$followersCount
    clean_tweet_data$friendCount[i] <- temp$friendsCount
    clean_tweet_data$description[i] <- temp$description
    clean_tweet_data$location[i] <- temp$location
  }
  print(i)
}

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

#######
# END #
#######