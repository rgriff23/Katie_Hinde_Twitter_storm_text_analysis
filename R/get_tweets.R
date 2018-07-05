# NOTE: tweets get cut off at 140 characters, which is problematic since some tweets are actually longer
# given Twitter's new features that don't count certain characters in the character count.
# I don't know how to adjust this with the 'searchTwitter' function, so for now this is just an unfortunate
# shortcoming of the analysis. Fortunately, much of the time it is only a hyperlink that gets cut off, which
# is eliminated during data cleaning anyway.

################
# PREPARATIONS #
################

# installing Rstem and sentiment package (not available on CRAN)
#library("devtools")
#install.packages("Rstem", repos = "http://www.omegahat.net/R")
#install_url("https://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")

# Load packages
library("twitteR")
library("tm")
library("wordcloud")
library("Rstem") 
library("sentiment") 
library("RSentiment") 
library("plyr") 

# My Twitter authentication (customize for yourself)
# source('~/Dropbox/Code/R/twitter_setup.R', chdir = TRUE)
# Tutorial on authenticating: https://www.r-bloggers.com/getting-started-with-twitter-in-r/

################################
# GET REPLIES AND QUOTE TWEETS #
################################

# ID of The Tweet
id <- "874116254767865856"

# Get mentions of @Mammals_Suck prior to the blog post ~36 hours later 
tweets <- searchTwitter("@Mammals_Suck", sinceID=id, maxID="874656504204279808", n=50000, retryOnRateLimit=20)
#saveRDS(tweets, '~/Desktop/GitHub/Katie_Hinde_Twitter_storm_text_analysis/data/all_raw_tweets_33261.rds') # 33261 tweets

# Get direct replies (must begin with "@Mammals_Suck" and reply to the original tweet)
replies <- lapply(tweets, function (x) {
  if (length(x$replyToSID) == 1) {
    if (x$replyToSID == id) {
      if (substr(x$text, 1, 13)=="@Mammals_Suck") {
        return (TRUE)
      } else return (FALSE)
    } else return(FALSE)
  } else return(FALSE)
})
replies <- tweets[unlist(replies)] # 2936 replies

# Get quote tweets (must quote the original tweet)
quotes <- searchTwitter("https://twitter.com/Mammals_Suck/status/874116254767865856", sinceID=id, maxID="874656504204279808", n=50000, retryOnRateLimit=5) #8270
quotes <- strip_retweets(quotes) #2281 quote tweets

##################################
# CLEAN REPLIES AND QUOTE TWEETS #
##################################

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

# Combine and clean tweet text 
tweets_combined <- append(replies, quotes)
tweets_clean <- unlist(clean_tweets(tweets_combined))

# Combine text with metadata (user, time, favorites, retweets) and drop empty text
tweet_data <- data.frame(text=tweets_clean)
tweet_data <- tweet_data[tweet_data$text != "",]
tweet_data$user <- unlist(lapply(tweets_combined, function(x) x$screenName))
tweet_data$time <- do.call("c", lapply(tweets_combined, function(x) x$created))
tweet_data$favorites <- unlist(lapply(tweets_combined, function(x) x$favoriteCount))
tweet_data$retweets <- unlist(lapply(tweets_combined, function(x) x$retweetCount))
tweet_data$type <- c(rep("reply", nrow(replies_data)), rep("quote", nrow(quotes_data)))
tweet_data$time_bin <- cut.POSIXt(tweet_data$time, breaks="3 hours", labels = FALSE)

########################################
# ADD SENTIMENT ANALYSIS TO TWEET DATA #
########################################

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

###############################
# ADD USER DATA TO TWEET DATA #
###############################

# Empty columns to store user follower count, friend count, location, and description
tweet_data$followerCount <- tweet_data$friendCount <- rep(0, nrow(tweet_data))
tweet_data$location <- tweet_data$description <- rep("0", nrow(tweet_data))

# Loop to get data (note: rate/retry limit of 900/15 mins)
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

# Clean description data (similar to cleaning tweets)
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

# export tweet data
#write.csv(tweet_data, file="~/Desktop/GitHub/Katie_Hinde_Twitter_storm_text_analysis/data/tweet_data.csv", row.names = FALSE)

#######
# END #
#######