#install.packages('memoise')
#install.packages('rtweet')
#install.packages('syuzhet')
#install.packages('SnowballC')
#install.packages('tm')
#install.packages('wordcloud')
library(SnowballC)
library(tm)
library(wordcloud)
library(memoise)
library(syuzhet)
library(rtweet)
library(stringr)
library(plotly)


### TWITTER
# Twitter Tokens and access
token = "176595081-xCQcCKtvzf67pObQutqjEY215fWrmmJr9PIL0KDS"
token_secret = "2iYbUOC1d7K3EVFM1KJF9nffcK6kNz9tWF8ZSFMzFA5HU"

# Twitter API Keys
appname <-"MIS510-Umbrella"
key = "vccjkMmxk3hcSAnRg3oleRBVK"
secret = "mDO140G2qoPR1wI9b2PN75nfULzh78GP9HWEAfIaAtGh9JjYqf"

twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = token,
  access_secret = token_secret
)


#books <<- list("Tucson" = "tucson",
#               "Denver" = "denver",
#               "Phoenix" = "phoenix")


#city = 'phoenix'
#######################################


getTermMatrix <- memoise(function(city) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
#  if (!(book %in% books))
#    stop("Unknown book")

rt <- search_tweets(q =  "rain OR weather OR snow OR sun OR storm",
                      geocode = lookup_coords(city, apikey = 'AIzaSyAKH5Rb4GiK7KNnLDD5MwRIdPZte08Y9l0' ),
                      include_rts = FALSE,
                      n = 500 #, retryonratelimit = TRUE
  )    

##  Remove Links from text
rt$stripped_text <- gsub("http.*","",  rt$text)
rt$stripped_text <- iconv(rt$text, to = 'ASCII', from = 'UTF-8', sub="byte")
rt$stripped_text=str_replace_all(rt$stripped_text,"[^[:graph:]]", " ") 
rt$stripped_text <- gsub("https.*","", rt$stripped_text)

# transform text to Lower
rt$stripped_text <- tolower(rt$stripped_text)

# Remove Stop Words from text
#rt$no_stop_text <- rt(rt$stripped_text, stopwords())

# Remove Mentions from text
rt$no_symbol_mentions <- gsub("\\B[@#]\\S+\\b", "", rt$stripped_text )

# Remove Hashtags from text
rt$no_punc <- removePunctuation(rt$no_symbol_mentions)

# Remove all Numbers
rt$no_numbers <- removeNumbers(rt$no_punc)

rt$all_clean <- rt$no_numbers

short_word.df <- as.vector(rt$all_clean)
nrc_emotion.df <- get_nrc_sentiment(short_word.df)
nrc_emotion <- cbind(rt, nrc_emotion.df)

wordcloud_tweet = c(
  paste(rt$all_clean[nrc_emotion.df$anger > 0], collapse=" "),
  paste(rt$all_clean[nrc_emotion.df$anticipation > 0], collapse=" "),
  paste(rt$all_clean[nrc_emotion.df$disgust > 0], collapse=" "),
  paste(rt$all_clean[nrc_emotion.df$fear > 0], collapse=" "),
  paste(rt$all_clean[nrc_emotion.df$joy > 0], collapse=" "),
  paste(rt$all_clean[nrc_emotion.df$sadness > 0], collapse=" "),
  paste(rt$all_clean[nrc_emotion.df$surprise > 0], collapse=" "),
  paste(rt$all_clean[nrc_emotion.df$trust > 0], collapse=" ")
) 
  
  corpus = Corpus(VectorSource(wordcloud_tweet))
  
  # remove punctuation, convert every word in lower case and remove stop words
  
  corpus = tm_map(corpus, tolower)
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, c(stopwords("english")))
  corpus = tm_map(corpus, removeWords, c(stopwords("SMART")))
#  corpus = tm_map(corpus, stemDocument)
  
  tdm = TermDocumentMatrix(corpus)
  
  # convert as matrix
  tdm = as.matrix(tdm)
  
  tdmnew <- tdm[nchar(rownames(tdm)) < 11,]
  
  colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
  colnames(tdmnew) <- colnames(tdm)
  print("End of Corpus")
  
  sort(rowSums(tdmnew), decreasing = TRUE)
})

# p <- plot_ly(emotion_sum, x=~emotion, y=~count, type="bar", color=~emotion) %>%
#   layout(xaxis=list(title=""), showlegend=FALSE,
#          title="Emotion Type for hashtag: #Storm")



getbanana <- memoise(function(city) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  #  if (!(book %in% books))
  #    stop("Unknown book")
  
  rt <- search_tweets(q =  "rain OR weather OR snow OR sun OR storm OR flood OR thunder",
                      geocode = lookup_coords(city, apikey = 'AIzaSyAKH5Rb4GiK7KNnLDD5MwRIdPZte08Y9l0' ),
                      include_rts = FALSE,
                      n = 500 #, retryonratelimit = TRUE
  )    
  
  ##  Remove Links from text
  rt$stripped_text <- gsub("http.*","",  rt$text)
  rt$stripped_text <- iconv(rt$text, to = 'ASCII', from = 'UTF-8', sub="byte")
  rt$stripped_text=str_replace_all(rt$stripped_text,"[^[:graph:]]", " ") 
  rt$stripped_text <- gsub("https.*","", rt$stripped_text)
  
  # transform text to Lower
  rt$stripped_text <- tolower(rt$stripped_text)
  
  # Remove Stop Words from text
  #rt$no_stop_text <- rt(rt$stripped_text, stopwords())
  
  # Remove Mentions from text
  rt$no_symbol_mentions <- gsub("\\B[@#]\\S+\\b", "", rt$stripped_text )
  
  # Remove Hashtags from text
  rt$no_punc <- removePunctuation(rt$no_symbol_mentions)
  
  # Remove all Numbers
  rt$no_numbers <- removeNumbers(rt$no_punc)
  
  rt$all_clean <- rt$no_numbers
  
  short_word.df <- as.vector(rt$all_clean)
  nrc_emotion.df <- get_nrc_sentiment(short_word.df)
  nrc_emotion <- cbind(rt, nrc_emotion.df)
  
  wordcloud_tweet = c(
    paste(rt$all_clean[nrc_emotion.df$anger > 0], collapse=" "),
    paste(rt$all_clean[nrc_emotion.df$anticipation > 0], collapse=" "),
    paste(rt$all_clean[nrc_emotion.df$disgust > 0], collapse=" "),
    paste(rt$all_clean[nrc_emotion.df$fear > 0], collapse=" "),
    paste(rt$all_clean[nrc_emotion.df$joy > 0], collapse=" "),
    paste(rt$all_clean[nrc_emotion.df$sadness > 0], collapse=" "),
    paste(rt$all_clean[nrc_emotion.df$surprise > 0], collapse=" "),
    paste(rt$all_clean[nrc_emotion.df$trust > 0], collapse=" ")
  ) 
  
  corpus = Corpus(VectorSource(wordcloud_tweet))
  
  # remove punctuation, convert every word in lower case and remove stop words
  
  corpus = tm_map(corpus, tolower)
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, c(stopwords("english")))
  corpus = tm_map(corpus, removeWords, c(stopwords("SMART")))
  #  corpus = tm_map(corpus, stemDocument)
  
  tdm = TermDocumentMatrix(corpus)
  
  # convert as matrix
  tdm = as.matrix(tdm)
  
  tdmnew <- tdm[nchar(rownames(tdm)) < 11,]
  
  colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
  colnames(tdmnew) <- colnames(tdm)
  #sort(rowSums(tdmnew), decreasing = TRUE)
  print("End of Banana")
  return(tdmnew)
  
})

getGrape <- memoise(function(city) {
  print('Got here')
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  #  if (!(book %in% books))
  #    stop("Unknown book")
  
  rt <- search_tweets(q =  "rain OR weather OR snow OR sun OR storm",
                      geocode = lookup_coords(city, apikey = 'AIzaSyAKH5Rb4GiK7KNnLDD5MwRIdPZte08Y9l0' ),
                      include_rts = FALSE,
                      n = 500 #, retryonratelimit = TRUE
  )    
  
  ##  Remove Links from text
  rt$stripped_text <- gsub("http.*","",  rt$text)
  rt$stripped_text <- iconv(rt$text, to = 'ASCII', from = 'UTF-8', sub="byte")
  rt$stripped_text=str_replace_all(rt$stripped_text,"[^[:graph:]]", " ") 
  rt$stripped_text <- gsub("https.*","", rt$stripped_text)
  
  # transform text to Lower
  rt$stripped_text <- tolower(rt$stripped_text)
  
  # Remove Stop Words from text
  #rt$no_stop_text <- rt(rt$stripped_text, stopwords())
  
  # Remove Mentions from text
  rt$no_symbol_mentions <- gsub("\\B[@#]\\S+\\b", "", rt$stripped_text )
  
  # Remove Hashtags from text
  rt$no_punc <- removePunctuation(rt$no_symbol_mentions)
  
  # Remove all Numbers
  rt$no_numbers <- removeNumbers(rt$no_punc)
  
  rt$all_clean <- rt$no_numbers
  
  short_word.df <- as.vector(rt$all_clean)
  
  syuzhet_emotion.df <- get_sentiment(short_word.df, method = 'syuzhet')
  syuzhet_emotion <- cbind(rt, syuzhet_emotion.df)
  
  nrc_emotion.df <- get_nrc_sentiment(short_word.df)
  nrc_emotion <- cbind(rt, nrc_emotion.df)
  #nrc_short_emotion.df <- cbind(rt, nrc_emotion.df)
  
  nrc_short.df <- subset.data.frame(nrc_emotion.df, select = c(anger, anticipation, disgust, fear, joy, sadness, surprise, trust))
  nrc_short_emotion.df <- cbind(rt, nrc_emotion.df)
  emotion_bar = colSums(nrc_short.df)
  emotion_sum = data.frame(count=emotion_bar, emotion=names(emotion_bar))
  emotion_sum$emotion = factor(emotion_sum$emotion, levels=emotion_sum$emotion[order(emotion_sum$count, decreasing = TRUE)])

  print("End of Grape")
 
    return(mean(syuzhet_emotion$syuzhet_emotion.df))
})

