library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

################################################################################
############### Function that counts of the number of state related topics
################################################################################
get_n_state_topics <- function(result) {
  
  library(stm)
  
  single_word_states <- c('alabama', 'alaska', 'arizona', 'arkansas', 'california', 'colorado', 'connecticut',
                          'delaware', 'florida', 'georgia', 'hawaii', 'idaho', 'illinois', 'indiana', 'iowa', 'kansas',
                          'kentucky', 'louisiana', 'maine', 'maryland', 'massachusetts', 'michigan', 'minnesota', 'mississippi', 'missouri',
                          'montana', 'nebraska', 'nevada', 'ohio', 'oklahoma', 'oregon', 'pennsylvania',  
                          'tennessee', 'texas', 'utah', 'vermont', 'virginia', 'washington', 'wisconsin', 'wyoming')
  
  two_word_states <- c('new hampshire', 'new jersey', 'new mexico', 'new york', 'north carolina',
                       'north dakota', 'rhode island', 'south carolina', 'south dakota', 'west virginia')
  
  two_word_states_list <- strsplit(two_word_states, ' ')
  
  # get the top 10 words for every topic
  mat <- labelTopics(result, n = 10, frexweight = 0.5)$frex
  
  # iterate over every topic
  counter <- 0
  for (i in 1:nrow(mat)) {
    # get the top 10 words for the topic at hand
    words_to_check <- mat[i,]
    
    # if any of the state names or any of the two word states have both of their words present in the top 10 words
    # then increase the counter by 1
    if (length(intersect(words_to_check, c(single_word_states) )) > 0 ) {
      counter <- counter + 1
    } else if ( any(vapply(two_word_states_list, FUN = function(x) all(x %in% words_to_check), FUN.VALUE=T))) {
      counter <- counter + 1
    }
    
    # otherwise counter stays the same
  }
  
  return(counter)
  
}

################################################################################
############### Count of the number of state related topics for each model
################################################################################
twts_cov <- readRDS("../Metadata_STM_Models/Tweets/stm_twts60_statecov.RDS")
legis_cov <- readRDS("../Metadata_STM_Models/Legislator/stm_legis60_statecov.RDS")

get_n_state_topics(twts_cov)  #15
get_n_state_topics(legis_cov) #33

################################################################################
################ Put Into a Dataframe
################################################################################
# Create Vectors
Documents <- c('Tweets','Legislators')
Topics <- c( "15", "33")

# Create DataFrame
df <- data.frame(Documents,Topics)

# save DataFrame
write.csv(df, "Appendix_Table3.csv")
