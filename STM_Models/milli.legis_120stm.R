dir.create(Sys.getenv('R_LIBS_USER'), showWarnings = FALSE, recursive = TRUE)

install.packages("tm", Sys.getenv("R_LIBS_USER"), repos = "https://mirror.las.iastate.edu/CRAN/" , INSTALL_opts = '--no-lock')
library(tm,lib.loc=Sys.getenv("R_LIBS_USER"))

library(glmnet)
library(stm)
library(quanteda)
library(textutils)
library(readr)
library(SnowballC)
library (dplyr)

text_leg <- readRDS("../Tweets_Data/handle.milli_sample.rds")

corpus_leg <- corpus(text_leg$full_text_by_handle, docnames=text_leg$user.screen_name, docvars=text_leg[,1:2])

# Next takes a couple minutes
tokens_leg <- tokens(corpus_leg,
                     what="word",
                     remove_punct=FALSE,
                     remove_symbols=FALSE,
                     remove_url= FALSE,
                     remove_separators=TRUE,
                     split_hyphens=FALSE,
                     include_docvars=TRUE,
                     padding=FALSE)

saveRDS(tokens_leg, file="tokens_leg120.rds") #116MB

dfm_leg <- dfm(tokens_leg, tolower=TRUE) #117MB 779288

saveRDS(dfm_leg, file="full_dfm_leg120.rds")

dfm_sm_leg <- dfm_remove(dfm_leg,stopwords("english"))

dfm_sm_leg <- dfm_remove(dfm_sm_leg, c(',','.',':',';','&',"amp","-","/",")","(","'"))

dfm_sm_leg <- dfm_trim(dfm_sm_leg,
                       min_termfreq=100,
                       min_docfreq=3) #4055x7146, 33M

saveRDS(dfm_sm_leg, file="dfm_sm_leg120.rds")

dfm_sm_leg_stm <- convert(dfm_sm_leg,to="stm") #25M

saveRDS(dfm_sm_leg_stm, file="dfm_sm_leg_stm120.rds")

system.time(              
  stm_sm_leg <- stm(documents = dfm_sm_leg_stm$documents,
                    vocab = dfm_sm_leg_stm$vocab,
                    K=120,
                    data=dfm_sm_leg_stm$meta,
                    init.type="Spectral",
                    seed = 178)
)
# Model Converged 
# user   system  elapsed 
# 1279.581  132.569 1411.666  # ~24 minutes

saveRDS(stm_sm_leg,file="stm_sm_leg120.RDS") #8M

# write top 10 keywords out, incl FREX with different weights

stm_sm_leg_kw <- labelTopics(stm_sm_leg, n=10,frexweight=0.5)
stm_sm_leg_kw_01 <- labelTopics(stm_sm_leg, n=10,frexweight=0.01)                                
stm_sm_leg_kw_99 <- labelTopics(stm_sm_leg, n=10,frexweight=0.99) 


cat("Big Model, By Legislator\n", file="stm_sm_leg120_kw.txt", append=FALSE)
for (k in 1:120) {
  cat(paste("Topic ",k,":\n", sep=""), file="stm_sm_leg120_kw.txt", append=TRUE)
  cat(paste("FREX (.50): ",paste(stm_sm_leg_kw$frex[k,], collapse=", "),"\n", sep=""), file="stm_sm_leg120_kw.txt", append=TRUE)
  cat(paste("FREX (.01): ",paste(stm_sm_leg_kw_01$frex[k,], collapse=", "),"\n",  sep=""), file="stm_sm_leg120_kw.txt", append=TRUE)
  cat(paste("FREX (.99): ",paste(stm_sm_leg_kw_99$frex[k,], collapse=", "),"\n",  sep=""), file="stm_sm_leg120_kw.txt", append=TRUE)
  cat(paste("High Prob: ",paste(stm_sm_leg_kw$prob[k,], collapse=", "),"\n",  sep=""), file="stm_sm_leg120_kw.txt", append=TRUE)
  cat(paste("Lift: ",paste(stm_sm_leg_kw$lift[k,], collapse=", "),"\n",  sep=""), file="stm_sm_leg120_kw.txt", append=TRUE)
  cat(paste("Score: ",paste(stm_sm_leg_kw$score[k,], collapse=", "),"\n",  sep=""), file="stm_sm_leg120_kw.txt", append=TRUE)
  cat("\n", file="stm_sm_leg120_kw.txt", append=TRUE)
}

