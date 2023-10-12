
#take the stm model objects and put into a single list
stm_sm_leg60 <- readRDS("../STM_Models/stm_sm_leg60.RDS")
stm_sm_twts60 <- readRDS("../STM_Models/stm_sm_twts60.RDS")
stm_sm_twts120 <- readRDS("../STM_Models/stm_sm_twts120.RDS")
stm_sm_leg120 <- readRDS("../STM_Models/stm_sm_leg120.RDS")

results <- list(stm_sm_leg60, stm_sm_leg120, stm_sm_twts60,stm_sm_twts120)
saveRDS(results, "results.rds")