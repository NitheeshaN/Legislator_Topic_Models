
library(e1071)
library(readr)

################ calculate skewness
dfm_sm_leg <- readRDS("../STM_Models/dfm_sm_leg60.rds")
dfm_sm_twts60 <- readRDS("./STM_Models//dfm_sm_twts60.rds")

rs_twts <- rowSums(dfm_sm_twts60)
rs_leg <- rowSums(dfm_sm_leg)

e1071::skewness(rs_twts) #0.742588
e1071::skewness(rs_leg) #1.656072

################ Put Into a Dataframe
# Create Vectors
document <- c('Legislator 60-topics','Tweets 60-topics')
skewness <- c( "1.66", "0.74")

# Create DataFrame
df <- data.frame(document,skewness)

# save DataFrame
write.csv(df, "skewness.csv") 


