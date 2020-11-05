library('gutenbergr')
library(tidyverse)
library(tidytext)
library(textdata)
library(ggplot2)

#######################
## Setup
#######################

#Create an American and UK list of book ID's
#us_id <- scan("american_corpus")
#uk_id <- scan("uk_corpus")
us_id <- c(203, 14107, 514, 25344, 2701, 624, 76, 74, 205, 8188, 53071, 73, 2870, 46650, 165, 9839, 24, 3285, 41, 21255)
uk_id <- c(42324, 1260, 174, 98, 244, 768, 11, 730, 1400, 9182, 36, 35, 145, 153, 583, 5658, 120, 4276, 3409, 3760)

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
  sorted <- corpus %>% 
    count(word, sort = TRUE)%>%
    top_n(20)
  return(sorted)
}

#Get the sentiment (this ended up being for BING only)
get_Sentiment <- function(corpus, lexicon)
{
  temp <- corpus%>%
    inner_join(lexicon)%>%
    count(index = gutenberg_id, sentiment)%>%
    spread(sentiment, n)%>%
    mutate(sentiment = positive - negative)%>%
    arrange(sentiment, .by_group = TRUE)%>%
    top_n(10)
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
us_word_sentiment <- get_Contributing_Negative_Words(us_corpus)
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


###########################
## ggplot
###########################

#Plot for most common words
ggplot() +
  geom_col(data=us_words, aes(x=word, y=n), color='green', alpha = 0.6) + 
  geom_col(data=uk_words, aes(x=word, y=n), color='red', alpha = 0.6) +
  xlab("Words") +
  ylab("Number of Occurences") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1))

#Plot for BING sentiment
ggplot() +
  geom_line(data=us_BING_sentiment, aes(x=c(1,2,3,4,5,6,7,8,9,10), y=sentiment), color='green', alpha = 0.6) + 
  geom_line(data=uk_BING_sentiment, aes(x=c(1,2,3,4,5,6,7,8,9,10), y=sentiment), color='red', alpha = 0.6) +
  xlab("Rank") +
  ylab("Sentiment") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10))

#Plot for negatively contributing words (BING)
ggplot() +
  geom_col(data=filter(us_word_sentiment, sentiment == "negative"), aes(x=word, y=n), color='green', alpha = 0.6) + 
  geom_col(data=filter(uk_word_sentiment, sentiment == "negative"), aes(x=word, y=n), color='red', alpha = 0.6) +
  xlab("Words") +
  ylab("Number of Occurences") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Plot for positively contributing words (BING)
ggplot() +
  geom_col(data=filter(us_word_sentiment, sentiment == "positive"), aes(x=word, y=n), color='green', alpha = 0.6) + 
  geom_col(data=filter(uk_word_sentiment, sentiment == "positive"), aes(x=word, y=n), color='red', alpha = 0.6) +
  xlab("Words") +
  ylab("Number of Occurences") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Plot for joy words
ggplot() +
  geom_col(data=us_joy_count, aes(x=word, y=n), color='green', alpha = 0.6) + 
  geom_col(data=uk_joy_count, aes(x=word, y=n), color='red', alpha = 0.6) +
  xlab("Words") +
  ylab("Number of Occurences") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Plot for anger words
ggplot() +
  geom_col(data=us_anger_count, aes(x=word, y=n), color='green', alpha = 0.6) + 
  geom_col(data=uk_anger_count, aes(x=word, y=n), color='red', alpha = 0.6) +
  xlab("Words") +
  ylab("Number of Occurences") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Plot for negative AFINN sentiment
ggplot() +
  geom_line(data=slice_max(us_AFINN_sentiment, desc(sentiment), n=5), aes(x=c(1,2,3,4,5), y=sentiment), color='green', alpha = 0.6) + 
  geom_line(data=slice_max(uk_AFINN_sentiment, desc(sentiment), n=5), aes(x=c(1,2,3,4,5), y=sentiment), color='red', alpha = 0.6) +
  xlab("Rank") +
  ylab("Sentiment") +
  scale_x_continuous(breaks=c(1,2,3,4,5))

#Plot for positive AFINN sentiment
ggplot() +
  geom_line(data=slice_min(us_AFINN_sentiment, desc(sentiment), n=5), aes(x=c(1,2,3,4,5), y=sentiment), color='green', alpha = 0.6) + 
  geom_line(data=slice_min(uk_AFINN_sentiment, desc(sentiment), n=5), aes(x=c(1,2,3,4,5), y=sentiment), color='red', alpha = 0.6) +
  xlab("Rank") +
  ylab("Sentiment") +
  scale_x_continuous(breaks=c(1,2,3,4,5))
