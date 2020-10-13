library('gutenbergr')
library(tidyverse)
library(tidytext)

#Create an American and UK list of book ID's
american_id <- scan("american_corpus")
uk_id <- scan("uk_corpus")

#A function that takes a vector of ID's and returns all of gutenberg books of those id's
download_src <- function(corpus_id)
{
  return(gutenberg_download(corpus_id))
}

clean <- function(corpus)
{
  return (corpus %>% unnest_tokens(word, text))
}

american_corpus <- clean(download_src(american_id))
uk_corpus <- clean(download_src(uk_id))
