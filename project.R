library('gutenbergr')
library(tidyverse)
library(tidytext)
library(textdata)

#######################
## Setup
#######################

#Create an American and UK list of book ID's
american_id <- scan("american_corpus")
uk_id <- scan("uk_corpus")

#Create our stop words SMART and BING set
data("stop_words")
SMART <- stop_words %>% filter(lexicon == "SMART")
BING <- get_sentiments("bing")

#######################
## Reusable Functions
#######################

#A function that takes a vector of ID's and returns all of gutenberg books of those id's
download_src <- function(corpus_id)
{
  return(gutenberg_download(corpus_id))
}

#A function that unnests tokens and removes stop words.
clean <- function(corpus)
{
  unnested_tokens <- corpus %>% unnest_tokens(word, text)
  return (unnested_tokens %>% anti_join(SMART))
}

#Create a list of words sorted by occurence and view
view_Top_Occurences <- function(corpus)
{
  sorted <- corpus %>% count(word, sort = TRUE)
  return(sorted)
}

view_Bing_Sentiment <- function(corpus)
{
  temp <- corpus%>%
    inner_join(BING)%>%
    count(index = gutenberg_id, sentiment)%>%
    spread(sentiment, n)%>%
    mutate(sentiment = positive - negative)%>%
    arrange(sentiment, .by_group = TRUE)
  return(temp)
}

###########################
## Corpus Work
###########################

#Use the above functions to create our two corpus's
american_corpus <- clean(download_src(american_id))
uk_corpus <- clean(download_src(uk_id))


#View each corpus
american_words <- view_Top_Occurences(american_corpus)
uk_words <- view_Top_Occurences(uk_corpus)
  
american_sentiment <- view_Bing_Sentiment(american_corpus)
uk_sentiment <- view_Bing_Sentiment(uk_corpus)





