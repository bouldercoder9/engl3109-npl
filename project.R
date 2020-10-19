library('gutenbergr')
library(tidyverse)
library(tidytext)
library(textdata)

#######################
## Setup
#######################

#Create an American and UK list of book ID's
us_id <- scan("american_corpus")
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

get_Sentiment <- function(corpus, lexicon)
{
  temp <- corpus%>%
    inner_join(lexicon)%>%
    count(index = gutenberg_id, sentiment)%>%
    spread(sentiment, n)%>%
    mutate(sentiment = positive - negative)%>%
    arrange(sentiment, .by_group = TRUE)
  return(temp)
}

#Return the top 15 negative and positive words in a corpora
get_Contributing_Negative_Words <- function(corpus)
{
  temp <- corpus%>%
    inner_join(BING)%>%
    count(word, sentiment)%>%
    group_by(sentiment)%>%
    top_n(15)%>%
    ungroup()%>%
    mutate(word = reorder(word,n))%>%
    group_by(sentiment)%>%
    arrange(desc(n), .by_group = TRUE)
  return(temp)
}

#Create a list of the top 15 joy words in a corpus
joy_Count <- function(corpus, joy_list)
{
  temp <- corpus%>%
    inner_join(joy_list)%>%
    count(word, sort = TRUE)%>%
    top_n(15)
  return(temp)
}

#Create a list of the top 15 anger words in a corpus
anger_Count <- function(corpus, anger_list)
{
  temp <- corpus%>%
    inner_join(anger_list)%>%
    count(word, sort = TRUE)%>%
    top_n(15)
  return(temp)
}

get_Top_Disgust <- function(corpus, disgust_list)
{
  temp <- corpus%>%
    inner_join(disgust_list)%>%
    count(index = gutenberg_id)%>%
    arrange(desc(n), .by_group = TRUE)%>%
    top_n(1)
  return(temp)
}

#Gets the AFINN sentiment of a corpus
get_AFINN_Sentiment <- function(corpus, lexicon)
{
  temp <- corpus%>%
    inner_join(lexicon)%>%
    group_by(gutenberg_id)%>%
    summarise(sentiment = sum(value))%>%
    arrange(sentiment, .by_group = TRUE)
  return(temp)
}

###########################
## Corpus Work
###########################

#Use the above functions to create our two corpus's
us_corpus <- clean(download_src(us_id))
uk_corpus <- clean(download_src(uk_id))

#View each corpus
us_words <- view_Top_Occurences(us_corpus)
uk_words <- view_Top_Occurences(uk_corpus)
  
#View the bing sentiment
us_BING_sentiment <- get_Sentiment(us_corpus, BING)
uk_BING_sentiment <- get_Sentiment(uk_corpus, BING)

# To find the total sentiment we calculated the sum of the sentiment columns that we generated earlier 
# To achieve this we are using the sum function and then specifying which variable to pull from and using the $ to specify which column to pull from for the sums
us_total_sentiment = sum(us_BING_sentiment$sentiment)
uk_total_sentiment = sum(uk_BING_sentiment$sentiment)

#View top 15 contributing words to BING sentiment
us__word_sentiment <- get_Contributing_Negative_Words(us_corpus)
uk_word_sentiment <- get_Contributing_Negative_Words(uk_corpus)

#Create our joy and anger word list from the NRC lexicon
nrc <- lexicon_nrc()
nrc_joy <- nrc%>%
  filter(sentiment == "joy")
nrc_anger <- nrc%>%
  filter(sentiment == "anger")
nrc_disgust <- nrc%>%
  filter(sentiment == "disgust")

#Create the anger and joy count for our two corpus
us_joy_count <- joy_Count(us_corpus, nrc_joy)
us_anger_count <- anger_Count(us_corpus, nrc_anger)

uk_joy_count <- joy_Count(uk_corpus, nrc_joy)
uk_anger_count <- anger_Count(uk_corpus, nrc_anger)

#Find the top disgust novel in each corpus
top_us_disgust <- get_Top_Disgust(us_corpus, nrc_disgust)
top_uk_disgust <- get_Top_Disgust(uk_corpus, nrc_disgust)

#Find the AFINN sentiment for each corpus
AFINN <- lexicon_afinn()
us_AFINN_sentiment <- get_AFINN_Sentiment(us_corpus, AFINN)
uk_AFINN_sentiment <- get_AFINN_Sentiment(uk_corpus, AFINN)

