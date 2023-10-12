library(stm)
library(readr)
library(SnowballC)
library(dplyr)
library(textutils)
library(quanteda)
library(ggplot2)
library(ggrepel)

dfm_sm_stm <- readRDS("../STM_Models/dfm_sm_twts_stm60.rds")
dfm_sm_leg_stm <- readRDS("../STM_Models/dfm_sm_leg_stm60.rds")

stm_sm_twts30 <- readRDS("../Topic_Selection/stm_sm_twts30.RDS")
stm_sm_twts60 <- readRDS("../STM_Models/stm_sm_twts60.RDS")
stm_sm_twts90 <- readRDS("../Topic_Selection/stm_sm_twts90.RDS")
stm_sm_twts120 <- readRDS("../STM_Models/stm_sm_twts120.RDS")
stm_sm_twts150 <- readRDS("../Topic_Selection/stm_sm_twts150.RDS")

stm_sm_leg30 <- readRDS("../Topic_Selection/stm_sm_twts30.RDS")
stm_sm_leg60 <- readRDS("../STM_Models/stm_sm_leg60.RDS")
stm_sm_leg90 <- readRDS("../Topic_Selection/stm_sm_twts90.RDS")
stm_sm_leg120 <- readRDS("../STM_Models/stm_sm_leg120.RDS")
stm_sm_leg150 <- readRDS("../Topic_Selection/stm_sm_twts150.RDS")


################## Tweets

## tweets 30 topic
#semantic coherence
mean(semanticCoherence(stm_sm_twts30, documents = dfm_sm_stm60$documents, M = 10))
#exclusivity
mean(exclusivity(stm_sm_twts30, M = 10, frexw = 0.5))

## tweets 60 topic
#semantic coherence
mean(semanticCoherence(stm_sm_twts60, documents = dfm_sm_stm60$documents, M = 10))
#exclusivity
mean(exclusivity(stm_sm_twts60, M = 10, frexw = 0.5))

## tweets 90 topic
#semantic coherence
mean(semanticCoherence(stm_sm_twts90, documents = dfm_sm_stm60$documents, M = 10))
#exclusivity
mean(exclusivity(stm_sm_twts90, M = 10, frexw = 0.5))

## tweets 120 topic
#semantic coherence
mean(semanticCoherence(stm_sm_twts120, documents = dfm_sm_stm120$documents, M = 10))
#exclusivity
mean(exclusivity(stm_sm_twts120, M = 10, frexw = 0.5))

## tweets 150 topic
#semantic coherence
mean(semanticCoherence(stm_sm_twts150, documents = dfm_sm_stm120$documents, M = 10))
#exclusivity
mean(exclusivity(stm_sm_twts150, M = 10, frexw = 0.5))

## create dataframe
# Create Vectors
Topics <- c("30",'60','90','120','150')
Average_Exclusivity <- c("9.905005", "9.957073", "9.974183", "9.979801", "9.924312")
Average_Semantic_Coherence <- c("-191.0752", "-213.7885", "-225.6904", "-241.5354", "-246.709" )

# Create DataFrame
tweets <- data.frame(Topics,Average_Exclusivity,Average_Semantic_Coherence)

# save DataFrame
write.csv(tweets, "tweets.csv") 

################## LEGIS

## legis 30 topic
#semantic coherence
mean(semanticCoherence(stm_sm_leg30, documents = dfm_sm_leg_stm$documents, M = 10))
#exclusivity
mean(exclusivity(stm_sm_leg30, M = 10, frexw = 0.5))

## legis 60 topic
#semantic coherence
mean(semanticCoherence(stm_sm_leg60, documents = dfm_sm_leg_stm$documents, M = 10))
#exclusivity
mean(exclusivity(stm_sm_leg60, M = 10, frexw = 0.5))

## legis 90 topic
#semantic coherence
mean(semanticCoherence(stm_sm_leg90, documents = dfm_sm_leg_stm$documents, M = 10))
#exclusivity
mean(exclusivity(stm_sm_leg90, M = 10, frexw = 0.5))

## legis 120 topic
#semantic coherence
mean(semanticCoherence(stm_sm_leg120, documents = dfm_sm_leg_stm120$documents, M = 10))
#exclusivity
mean(exclusivity(stm_sm_leg120, M = 10, frexw = 0.5))

## legis 150 topic
#semantic coherence
mean(semanticCoherence(stm_sm_leg150, documents = dfm_sm_leg_stm120$documents, M = 10))
#exclusivity
mean(exclusivity(stm_sm_leg150, M = 10, frexw = 0.5))

## create dataframe
# Create Vectors
Topics <- c("30",'60','90','120','150')
Average_Exclusivity <- c("9.122299", "9.314906", "9.432991", "9.427999", "9.445876")
Average_Semantic_Coherence <- c("-57.25997", "-53.55918", "-58.37006", "-55.07435", "-54.55329")

# Create DataFrame
legis <- data.frame(Topics,Average_Exclusivity,Average_Semantic_Coherence)

# save DataFrame
write.csv(legis, "legis.csv") 

################## PLOTS
###tweets
#Plot without points but with actual point values
pdf(file ='tweets_coherence_vs_exclusivity.pdf', width =  8, height = 6)

ggplot(tweets, aes(x = tweets$Average_Exclusivity, y = tweets$Average_Semantic_ACoherence)) +
  geom_text_repel(aes(label = tweets$Topics)) +
  labs(
    x = "Average Exclusivity",
    y = "Average Semantic Coherence",
    title = "Tweets Document Definition Topic Models Fit Metrics")

dev.off()


###legislator
#Plot without points but with actual point values
pdf(file ='legis_coherence_vs_exclusivity.pdf', width =  8, height = 6)

ggplot(legis, aes(x = legis$Average_Exclusivity, y = legis$Average_Semantic_Coherence)) +
  geom_text_repel(aes(label = legis$Topics)) +
  labs(
    x = "Average Exclusivity",
    y = "Average Semantic Coherence",
    title = "Legislator Aggregated Document Definition Topic Models Fit Metrics"
  )

dev.off()
