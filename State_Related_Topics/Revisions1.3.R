library(stm)
library(readr)
library(SnowballC)
library(dplyr)
library(textutils)
library(quanteda)



#load dfm_sm_leg120
#load milli.twts_sample.csv

################### R1.3 ###################

########## Create Data frame of States and tokens
df_leg120 <- convert(dfm_sm_leg120,to="data.frame")

# merge screen names in df_leg120 to states
df_states <- milli_twts_sample[,c("user.screen_name", "legislator_state")]
df_states <- unique(df_states)

df_leg120 <- df_leg120 %>% rename(user.screen_name = doc_id)

state.tokens <- merge(df_states, df_leg120, by = "user.screen_name")

test <- state.tokens
test = subset(state.tokens, select = -c(user.screen_name))
write.csv(test, "test.csv")
#rest was done in python
  #import pandas as pd
  #df = pd.read_csv("/Users/nitheeshanakka/OneDrive - The Pennsylvania State University/STM/mill_stm/Reviewer Revisions/test.csv")
  #df2 = df.groupby('legislator_state').sum()
  #df2.to_csv('state_tokens.csv')

state_tokens = state_tokens[, c(1, 3:ncol(state_tokens))]
#write.csv(state_tokens, "state_tokens.csv")
state_tokens <- state_tokens[,2:ncol(state_tokens)]
state_tokens <- data.frame(t(state_tokens))
saveRDS(state_tokens, "state_tokens.rds")

########## Create Data frame of topics and FREX terms
#read in the stm model as "stm"
stm <- readRDS("~/OneDrive - The Pennsylvania State University/STM/mill_stm/milli.twts.120/stm_sm_twts120.RDS")

#get FREX words
stm_kw <- labelTopics(stm, n=10,frexweight=0.5)

#put FREX words in a dataframe
topics <- data.frame(stm_kw$frex, header= FALSE)
row.names(topics) <- paste0("topic", 1:nrow(topics))
names(topics) <- NULL
topics<- topics[,1:10]

saveRDS(topics, "10legis60_topics.rds") #change name according to model


########## Create Data frame of Counts of State tokens/Topics
state_tokens <- readRDS("state_tokens.rds")
#read in the topics df as "topics"
topics <- readRDS("2legis60_topics.rds")

#create term x state counts df
topic_list <- list()
df <- data.frame()
df$topic
topic_names <- c(row.names(topics))

for (k in 1:length(topic_names)){
  topic <- rep(k, 10)
  terms <- topics[topic_names[[k]],]
  
  for (i in 1:length(terms)) {
    topic_list[[i]] <- state_tokens[terms[[i]],]
  }
  topic.df <- bind_rows(topic_list)
  df <- rbind(df, cbind(topic, topic.df))
}

colnames(df)[2:ncol(df)] <- state_tokens[1,]
saveRDS(df, "10legis60_tokens.rds")
#use split function to look at huge dataframe by topic

########## Term Proportions Aanalysis
sum_char <- function(x){sum(as.numeric(x))}
row_total <- apply(df[,-1], 1, sum_char) #calc row sums but exclude the first column

max_char <- function(x){max(as.numeric(x))}
row_max <- apply(df[,-1], 1, max_char) #calc row max but exclude the first column

proportions <- row_max/row_total
#row.names(df)[order(-proportions)][1:50]
#props <- cbind(row.names(df)[order(-proportions)][1:600], proportions[order(-proportions)][1:600])

#steps for when I read in state_tokens
#rownames(df) <- df$...1
#df = subset(df, select = -c(...1) )
props <- cbind(row.names(df)[order(-proportions)], proportions[order(-proportions)])

saveRDS(props, "10legis60_props.rds")

########## Calculate Number of topics with Terms from keyword list
state_terms <- props
#state_terms <- readRDS("og_twtw120_props.rds")
topics <- readRDS("og_twts120_topics.rds")


st <- c(row.names(state_terms))
#find threshold for highly state related topics for this model
  #eg. for the twts 120 model the cutoff is "springfield" "0.505521472392638"
index <- which(st == "@kcstar")  
#index the "st" list from 1- thershold index
st <- st[1:index]

#put it all in one place
df2 <- list()
for (k in 1:nrow(topics)){
  top <- c(topics[k,]) #take each row of topics and put it into a list
  list <- top %in% st #check if "top" is in "st" and put the resulting T/F statements into a list
  if ("TRUE" %in% list == FALSE ){
    df2[[k]] <- print("NO") #if there are NO "TRUE" in "list" then topic IS NOT state related
  }else {
    if ("TRUE" %in% list == TRUE ){
      df2[[k]] <- print("YES")#if there ARE "TRUE" in "list" then topic IS state related
    }
  }
}

sum(df2 =="YES")
#37 twts120 - cutoff is 0.5
#20 twts60 - cutoff is "0.676424464192368" , also for 0.5
#68 legis120 - cutoff is 0.5
#39 legis60 - cutoff is 0.5
#19 cov_twts60 - cutoff is 0.6 no 0.5
#39 cov_legis60 - cutoff is 0.5


########## List of state related terms from Twitter
state_tokens <- readRDS("state_tokens.rds")
#set column names to first row
colnames(state_tokens) <- as.character(state_tokens[1,])

sum_char <- function(x){sum(as.numeric(x))}
row_total <- apply(state_tokens, 1, sum_char) #calc row sums but exclude the first column

max_char <- function(x){max(as.numeric(x))}
row_max <- apply(state_tokens, 1, max_char) #calc row max but exclude the first column

proportions <- row_max/row_total
props <- cbind(row.names(state_tokens)[order(-proportions)], proportions[order(-proportions)])

#check functions work
#0.059763008758372 - feel
sum(as.numeric(state_tokens["feel",])) #3882
max(as.numeric(state_tokens["feel",])) #232
232/3882 #0.05976301

#subset data to all terms with proportions that are 0.5 or greater
which(props == "@kcstar") #1830
Twitter_terms <- props[1:1830]
saveRDS(Twitter_terms, "Twitter_terms.rds")

########## state related terms using "get_n_state_topics" function
source('get_n_state_topics.R')

get_n_state_topics(stm10)
