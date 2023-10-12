dir.create(Sys.getenv('R_LIBS_USER'), showWarnings = FALSE, recursive = TRUE)

install.packages("tm", Sys.getenv("R_LIBS_USER"), repos = "https://mirror.las.iastate.edu/CRAN/" , INSTALL_opts = '--no-lock')
library(tm,lib.loc=Sys.getenv("R_LIBS_USER"))

library(glmnet)
library(stm)
library(quanteda)
library(textutils)
library(readr)
library(SnowballC)
library(dplyr)

text_twts <- readRDS("../Tweets_Data/milli.twts_sample.rds")


corpus_twts <- corpus(text_twts$full_text, docnames=text_twts$row_id, docvars=text_twts[,1:2])

# Next takes a couple minutes
tokens_twts <- tokens(corpus_twts,
                     what="word",
                     remove_punct=FALSE,
                     remove_symbols=FALSE,
                     remove_url= FALSE,
                     remove_separators=TRUE,
                     split_hyphens=FALSE,
                     include_docvars=TRUE,
                     padding=FALSE)

saveRDS(tokens_twts, file="tokens_twts60.rds") #116MB

dfm_twts <- dfm(tokens_twts, tolower=TRUE) #117MB 779288

saveRDS(dfm_twts, file="full_dfm_twts60.rds")

dfm_sm <- dfm_remove(dfm_twts,stopwords("english"))

dfm_sm <- dfm_remove(dfm_sm, c(',','.',':',';','&',"amp","-","/",")","(","'"))

dfm_sm <- dfm_trim(dfm_sm,
                       min_termfreq=100,
                       min_docfreq=3) #4055x7146, 33M

saveRDS(dfm_sm, file="dfm_sm_twts60.rds")

dfm_sm_stm <- convert(dfm_sm,to="stm") #25M

saveRDS(dfm_sm_stm, file="dfm_sm_twts_stm60.rds")

system.time(              
  stm_sm_twts <- stm(documents = dfm_sm_stm$documents,
                    vocab = dfm_sm_stm$vocab,
                    K=60,
                    data=dfm_sm_stm$meta,
                    init.type="Spectral",
                    seed = 178)
)

saveRDS(stm_sm_twts,file="stm_sm_twts60.RDS") #8M

# write top 10 keywords out, incl FREX with different weights

stm_sm_twts_kw <- labelTopics(stm_sm_twts, n=10,frexweight=0.5)
stm_sm_twts_kw_01 <- labelTopics(stm_sm_twts, n=10,frexweight=0.01)                                
stm_sm_twts_kw_99 <- labelTopics(stm_sm_twts, n=10,frexweight=0.99) 

cat("Small Model, By Legislator\n", file="stm_sm_twts60_kw.txt", append=FALSE)
for (k in 1:60) {
  cat(paste("Topic ",k,":\n", sep=""), file="stm_sm_twts60_kw.txt", append=TRUE)
  cat(paste("FREX (.50): ",paste(stm_sm_twts_kw$frex[k,], collapse=", "),"\n", sep=""), file="stm_sm_twts60_kw.txt", append=TRUE)
  cat(paste("FREX (.01): ",paste(stm_sm_twts_kw_01$frex[k,], collapse=", "),"\n",  sep=""), file="stm_sm_twts60_kw.txt", append=TRUE)
  cat(paste("FREX (.99): ",paste(stm_sm_twts_kw_99$frex[k,], collapse=", "),"\n",  sep=""), file="stm_sm_twts60_kw.txt", append=TRUE)
  cat(paste("High Prob: ",paste(stm_sm_twts_kw$prob[k,], collapse=", "),"\n",  sep=""), file="stm_sm_twts60_kw.txt", append=TRUE)
  cat(paste("Lift: ",paste(stm_sm_twts_kw$lift[k,], collapse=", "),"\n",  sep=""), file="stm_sm_twts60_kw.txt", append=TRUE)
  cat(paste("Score: ",paste(stm_sm_twts_kw$score[k,], collapse=", "),"\n",  sep=""), file="stm_sm_twts60_kw.txt", append=TRUE)
  cat("\n", file="stm_sm_twts60_kw.txt", append=TRUE)
}
