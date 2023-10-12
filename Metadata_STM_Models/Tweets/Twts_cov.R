# STATE ID METADATA ANALYSIS 
library(stm)
library(readr)
library(SnowballC)
library(dplyr)
library(textutils)
library(quanteda)

####### TWEETS 60 Topic Model
#load objects
dfm_sm_stm60 <- readRDS("../STM_Models/dfm_sm_twts_stm60.rds")
text_twts <- read_csv("../Tweets_Data/milli.twts_sample.rds")

#add state ID covariate to meta data
dfm_sm_stm60$meta$state_id <- text_twts$legislator_state[as.numeric(names(dfm_sm_stm60$documents))]

system.time(              
  stm_sm_twts <- stm(documents = dfm_sm_stm60$documents,
                     vocab = dfm_sm_stm60$vocab,
                     K=60,
                     prevalence =~ state_id,
                     max.em.its = 1000,
                     gamma.prior='L1',
                     data=dfm_sm_stm60$meta,
                     init.type="Spectral",
                     seed = 178)
)

saveRDS(stm_sm_twts,file="stm_twts60_statecov.RDS") #8M

