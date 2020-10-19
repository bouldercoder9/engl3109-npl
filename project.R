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

american_corpus <- clean(download_src(american_id))
uk_corpus <- clean(download_src(uk_id))

# To find the total sentiment we calculated the sum of the sentiment columns that we generated earlier 
# To achieve this we are using the sum function and then specifying which variable to pull from and using the $ to specify which column to pull from for the sums
american_total_sentiment = sum(american_sentiment$sentiment)
uk_total_sentiment = sum(uk_sentiment$sentiment)

#View top 15 contributing words to BING sentiment
american__word_sentiment <- get_Contributing_Negative_Words(american_corpus)
uk_word_sentiment <- get_Contributing_Negative_Words(uk_corpus)




#Problem E. First we assign the nrc function and run it for the first time to make sure its running smoothly
nrc <- lexicon_nrc()
nrc_joy_us <- nrc%>%
  filter(sentiment == "joy") # we sort all the joy and anger words into their own variables
nrc_anger_us <- nrc%>%
  filter(sentiment == "anger")
nrc_joy_us_count <- american_corpus%>%
  inner_join(nrc_joy_us)%>%
  count(word, sort = TRUE)%>%
  top_n(15) # this is to sort the variables to the top 15 words by the number of times the words occur
nrc_anger_us_count <- american_corpus%>%
  inner_join(nrc_anger_us)%>%
  count(word, sort = TRUE)%>%
  top_n(15)

# Repeat for UK
nrc_joy_uk <- nrc%>% # this just holds the joy words not actually consisting of the UK joy words because we haven't filtered yet through yet
  filter(sentiment == "joy") # we sort all the joy and anger words into their own variables
nrc_anger_uk <- nrc%>%
  filter(sentiment == "anger")
nrc_joy_uk_count <- uk_corpus%>%
  inner_join(nrc_joy_uk)%>%
  count(word, sort = TRUE)%>%
  top_n(15)
nrc_anger_uk_count <- uk_corpus%>%
  inner_join(nrc_anger_uk)%>%
  count(word, sort = TRUE)%>%
  top_n(15)

## This gets the individual occurences of each UK/US Joy/Anger corpus not combing the occurrences together ##

disgustNovel <- nrc%>% #W.I.P
  filter(sentiment == "disgust")

  

