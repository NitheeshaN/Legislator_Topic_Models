# STATE ID METADATA ANALYSIS 
library(stm)
library(readr)
library(SnowballC)
library(dplyr)
library(textutils)
library(quanteda)

####### LEGIS 60 Topic Model
#load objects
dfm_sm_leg_stm <- readRDS("../STM_Models/dfm_sm_leg_stm60.rds")
text_handle <- read_csv("../Tweets_Data/handle.milli_sample.rds")
text_twts <- read_csv("../Tweets_Data/milli.twts_sample.rds")

#add state_IDS
text_twts2  <- text_twts[, c("user.screen_name", "legislator_state")]
text_twts3 <- unique(text_twts2)

text_handle2 <- text_handle
text_handle2 <- merge(text_handle2, text_twts3, by = "user.screen_name")

#add state ID covariate to meta data
dfm_sm_leg_stm$meta$state_id <- text_handle2$legislator_state[text_handle2$user.screen_name == names(dfm_sm_leg_stm$documents)]

#find legislator_states for each document
doc_names <-names(dfm_sm_leg_stm$documents)
doc_names <- data.frame(doc_names)
doc_names2 <- merge(text_handle2, doc_names, by.x = "user.screen_name", by.y = "doc_names")

dfm_sm_leg_stm$meta$state_id <- doc_names2$legislator_state
saveRDS(dfm_sm_leg_stm, "dfm_sm_leg_stm_cov.rds")

system.time(              
  stm_sm_legis <- stm(documents = dfm_sm_leg_stm$documents,
                      vocab = dfm_sm_leg_stm$vocab,
                      K=60,
                      prevalence =~ state_id,
                      max.em.its = 1000,
                      gamma.prior='L1',
                      data=dfm_sm_leg_stm$meta,
                      init.type="Spectral",
                      seed = 178)
)

saveRDS(stm_sm_legis,file="stm_legis60_statecov.RDS") #8M
