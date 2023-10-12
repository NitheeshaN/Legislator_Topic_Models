library(glmnet)
library(stm)
library(quanteda)
library(textutils)
library(readr)
library(SnowballC)
library (dplyr)


dfm_sm_stm <- readRDS("../STM_Models/dfm_sm_twts_stm60.rds")
dfm_sm_leg_stm <- readRDS("../STM_Models/dfm_sm_leg_stm60.rds")

################## TWEETS
#30
system.time(              
  stm_sm_twts30 <- stm(documents = dfm_sm_stm$documents,
                     vocab = dfm_sm_stm$vocab,
                     K=30,
                     data=dfm_sm_stm$meta,
                     init.type="Spectral",
                     seed = 178)
)

saveRDS(stm_sm_twts30,file="stm_sm_twts30.RDS")

#90
system.time(              
  stm_sm_twts90 <- stm(documents = dfm_sm_stm$documents,
                       vocab = dfm_sm_stm$vocab,
                       K=90,
                       data=dfm_sm_stm$meta,
                       init.type="Spectral",
                       seed = 178)
)

saveRDS(stm_sm_twts90,file="stm_sm_twts90.RDS")

#150
system.time(              
  stm_sm_twts150 <- stm(documents = dfm_sm_stm$documents,
                       vocab = dfm_sm_stm$vocab,
                       K=150,
                       data=dfm_sm_stm$meta,
                       init.type="Spectral",
                       seed = 178)
)

saveRDS(stm_sm_twts150,file="stm_sm_twts150.RDS")


################## LEGIS
#30
system.time(              
  stm_sm_leg30 <- stm(documents = dfm_sm_leg_stm$documents,
                    vocab = dfm_sm_leg_stm$vocab,
                    K=30,
                    data=dfm_sm_leg_stm$meta,
                    init.type="Spectral",
                    seed = 178)
)

saveRDS(stm_sm_leg30,file="stm_sm_leg30.RDS") 

#90
system.time(              
  stm_sm_leg90 <- stm(documents = dfm_sm_leg_stm$documents,
                    vocab = dfm_sm_leg_stm$vocab,
                    K=90,
                    data=dfm_sm_leg_stm$meta,
                    init.type="Spectral",
                    seed = 178)
)

saveRDS(stm_sm_leg90,file="stm_sm_leg90.RDS") 

#150
system.time(              
  stm_sm_leg150 <- stm(documents = dfm_sm_leg_stm$documents,
                    vocab = dfm_sm_leg_stm$vocab,
                    K=150,
                    data=dfm_sm_leg_stm$meta,
                    init.type="Spectral",
                    seed = 178)
)

saveRDS(stm_sm_leg150,file="stm_sm_leg150.RDS")
