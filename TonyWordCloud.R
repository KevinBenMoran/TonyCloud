setwd("~/Projects/TonyAwardViz")

library(twitteR)
library(ROAuth)
library(httr)
library(wordcloud2)

# Set API Keys
api_key <- "W5vrgfgFaEWonAMvtThwJKomf"
api_secret <- "cZpBPK3zfuluOBueloIesngxrUbAVRLnSHblYXdjUzL7GfmDym"
access_token <- "3224154249-6DZpglzrCX4leYoNd7YxAN820qwTWzRTChqfbTR"
access_token_secret <- "NXguMCSBxnHMdag0yYAIHlNTtD5BFa5GSfESX85sUqbnh"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

tonyTweets <- searchTwitteR('#tonyawards since:2015-07-01 until:2016-06-20', n = 5000, retryOnRateLimit = 100)

library(plyr)
feed_tony = laply(tonyTweets, function(t) t$getText())
head(feed_tony)
typeof(feed_tony)
feed_tony

write(feed_tony, file = "2016tonytweets.txt")

# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

text <- readLines(file.choose())
tweets <- Corpus(VectorSource(text))
inspect(tweets)


toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
tweets <- tm_map(tweets, toSpace, "/")
tweets <- tm_map(tweets, toSpace, "@")
tweets <- tm_map(tweets, toSpace, "\\|")
tweets <- tm_map(tweets, toSpace, " ?(f|ht)tp(s?)://(.*)[.][a-z]+")

# Convert the text to lower case
tweets <- tm_map(tweets, content_transformer(tolower))
# Remove numbers
tweets <- tm_map(tweets, removeNumbers)
# Remove english common stopwords
tweets <- tm_map(tweets, removeWords, stopwords(kind = "en"))
# Remove your own stop word
# specify your stopwords as a character vector
tweets <- tm_map(tweets, removeWords, c("https", "tco","tonyawards","tony","awards")) 
# Remove punctuations
tweets <- tm_map(tweets, removePunctuation)
# Eliminate extra white spaces
tweets <- tm_map(tweets, stripWhitespace)
# Text stemming
# tweets <- tm_map(tweets, stemDocument)

dtm <- TermDocumentMatrix(tweets)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 30)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

set.seed(123)
wordcloud2(d, figPath = 'C:/Users/Kevin/Documents/Projects/TonyAwardViz/star.png', size=0.5, color = 'black', backgroundColor = '#ba8d05')

             
             