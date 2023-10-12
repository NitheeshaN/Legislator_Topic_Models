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
leg60 <- readRDS("../STM_Models/stm_sm_leg60.RDS")
twts60 <- readRDS("../STM_Models/stm_sm_twts60.RDS")
twts120 <- readRDS("../STM_Models/stm_sm_twts120.RDS")
leg120 <- readRDS("../STM_Models/stm_sm_leg120.RDS")
#source('get_n_state_topics.R')

get_n_state_topics(leg60)  #34
get_n_state_topics(leg120) #43
get_n_state_topics(twts60) #17
get_n_state_topics(twts120) #24

################################################################################
################ Proportions of state related topics for each model
################################################################################
results <- readRDS("results.rds")

single_word_states <- c('alabama', 'alaska', 'arizona', 'arkansas', 'california', 'colorado', 'connecticut',
                        'delaware', 'florida', 'georgia', 'hawaii', 'idaho', 'illinois', 'indiana', 'iowa', 'kansas',
                        'kentucky', 'louisiana', 'maine', 'maryland', 'massachusetts', 'michigan', 'minnesota', 'mississippi', 'missouri',
                        'montana', 'nebraska', 'nevada', 'ohio', 'oklahoma', 'oregon', 'pennsylvania',  
                        'tennessee', 'texas', 'utah', 'vermont', 'virginia', 'washington', 'wisconsin', 'wyoming')

two_word_states <- c('new hampshire', 'new jersey', 'new mexico', 'new york', 'north carolina',
                     'north dakota', 'rhode island', 'south carolina', 'south dakota', 'west virginia')

two_word_states_list <- strsplit(two_word_states, ' ')

################ 60 Legislator Document Def
for (result in results[1]) {
  
  K <- result$settings$dim$K
  N <- result$settings$dim$N
  file_name <- paste0(K, '_topics_', N, "_documents_automated.csv")
  
  mat <- labelTopics(result, n = 10, frexweight = 0.5)$frex
  
  # by default, assume it's not a state topic
  STATE <- rep('N', nrow(mat))
  
  for (i in 1:nrow(mat)) {
    # get the top 10 words for the topic at hand
    words_to_check <- mat[i,]
    
    # if any of the top 10 words of the topic are in one of our state words then increase the counter by 1
    if (length(intersect(words_to_check, c(single_word_states) )) > 0 ) {
      STATE[i] <- 'Y'
    } else if ( any(vapply(two_word_states_list, FUN = function(x) all(x %in% words_to_check), FUN.VALUE=T))) {
      STATE[i] <- 'Y'
    }
    # otherwise counter stays the same
  }
  
  mat %>% 
    as.data.frame() %>% 
    unite(sep = ", ", col = words) %>%
    mutate(STATE = STATE) %>%
    write_csv(file = file_name)
  
}

# where plots will be stored
plots <- list()
counter <- 0 
for (obj in results[1]) { 
  
  counter <- counter + 1
  
  n_topics <- obj$settings$dim$K
  n_documents <- obj$settings$dim$N
  
  topics <- paste0('Topic', 1:n_topics)
  proportions <- colSums(obj$theta) / n_documents 
  frexes <- apply(labelTopics(obj, n = 10)$frex, MARGIN = 1, FUN = function(x) paste(x, collapse = ', ' ))
  file <- paste0(n_topics, "_topics_", n_documents, "_documents_automated.csv")
  state <- read_csv(file, show_col_types = F)
  colors1 <- ifelse(state$STATE == "Y", 'red', 'grey35')
  colors2 <- ifelse(state$STATE == "Y", 'red', 'black')
  
  df <- data.frame(topic = topics, proportion = proportions, frex = frexes, col1 = colors1, col2 = colors2)
  
  df <- df[order(df$proportion, decreasing = T), ]
  
  df$topic <- ordered(df$topic, levels = df$topic[n_topics:1])
  
  if (n_topics == 60) ylim <- 0.09 # if any words do not fit in the plot, you might want to play with these
  if (n_topics == 120) ylim <- 0.07
  
  p <- ggplot(df, aes(x = topic, y = proportion)) +
    geom_col(fill = df$col1, width = 0.5) +
    coord_flip() +
    geom_text(aes(label = frex, hjust = -0.1), color = df$col2, size = 2.5) +
    ylim(0, ylim) +
    theme_bw() +
    theme(aspect.ratio = 3/4) +
    theme(axis.text.y= element_blank(), axis.ticks.y = element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank() ) +
    xlab('') +
    ylab('Expected Topic Proportions') +
    labs(title = paste0('Top Topics (', n_topics, " Topics and ", n_documents, " Documents)"))
  
  plot_name <- paste0(n_topics, "_topics_", n_documents, "_documents")
  plots[[counter]] <- list(plot = p, plot_name = plot_name)
  
}

#find proportion of topics that are state related
with(df, sum(proportion[col1 == 'red'])) #0.314956

################ 120 Legislator Document Def
for (result in results[2]) {
  
  K <- result$settings$dim$K
  N <- result$settings$dim$N
  file_name <- paste0(K, '_topics_', N, "_documents_automated.csv")
  
  mat <- labelTopics(result, n = 10, frexweight = 0.5)$frex
  
  # by default, assume it's not a state topic
  STATE <- rep('N', nrow(mat))
  
  for (i in 1:nrow(mat)) {
    # get the top 10 words for the topic at hand
    words_to_check <- mat[i,]
    
    # if any of the top 10 words of the topic are in one of our state words then increase the counter by 1
    if (length(intersect(words_to_check, c(single_word_states) )) > 0 ) {
      STATE[i] <- 'Y'
    } else if ( any(vapply(two_word_states_list, FUN = function(x) all(x %in% words_to_check), FUN.VALUE=T))) {
      STATE[i] <- 'Y'
    }
    # otherwise counter stays the same
  }
  
  mat %>% 
    as.data.frame() %>% 
    unite(sep = ", ", col = words) %>%
    mutate(STATE = STATE) %>%
    write_csv(file = file_name)
  
}

# where plots will be stored
plots <- list()
counter <- 0 
for (obj in results[2]) { 
  
  counter <- counter + 1
  
  n_topics <- obj$settings$dim$K
  n_documents <- obj$settings$dim$N
  
  topics <- paste0('Topic', 1:n_topics)
  proportions <- colSums(obj$theta) / n_documents 
  frexes <- apply(labelTopics(obj, n = 10)$frex, MARGIN = 1, FUN = function(x) paste(x, collapse = ', ' ))
  file <- paste0(n_topics, "_topics_", n_documents, "_documents_automated.csv")
  state <- read_csv(file, show_col_types = F)
  colors1 <- ifelse(state$STATE == "Y", 'red', 'grey35')
  colors2 <- ifelse(state$STATE == "Y", 'red', 'black')
  
  df <- data.frame(topic = topics, proportion = proportions, frex = frexes, col1 = colors1, col2 = colors2)
  
  df <- df[order(df$proportion, decreasing = T), ]
  
  df$topic <- ordered(df$topic, levels = df$topic[n_topics:1])
  
  if (n_topics == 60) ylim <- 0.09 # if any words do not fit in the plot, you might want to play with these
  if (n_topics == 120) ylim <- 0.07
  
  p <- ggplot(df, aes(x = topic, y = proportion)) +
    geom_col(fill = df$col1, width = 0.5) +
    coord_flip() +
    geom_text(aes(label = frex, hjust = -0.1), color = df$col2, size = 2.5) +
    ylim(0, ylim) +
    theme_bw() +
    theme(aspect.ratio = 3/4) +
    theme(axis.text.y= element_blank(), axis.ticks.y = element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank() ) +
    xlab('') +
    ylab('Expected Topic Proportions') +
    labs(title = paste0('Top Topics (', n_topics, " Topics and ", n_documents, " Documents)"))
  
  plot_name <- paste0(n_topics, "_topics_", n_documents, "_documents")
  plots[[counter]] <- list(plot = p, plot_name = plot_name)
  
}

#find proportion of topics that are state related
with(df, sum(proportion[col1 == 'red'])) #0.2056809

################ 60 Tweets Document Def
for (result in results[3]) {
  
  K <- result$settings$dim$K
  N <- result$settings$dim$N
  file_name <- paste0(K, '_topics_', N, "_documents_automated.csv")
  
  mat <- labelTopics(result, n = 10, frexweight = 0.5)$frex
  
  # by default, assume it's not a state topic
  STATE <- rep('N', nrow(mat))
  
  for (i in 1:nrow(mat)) {
    # get the top 10 words for the topic at hand
    words_to_check <- mat[i,]
    
    # if any of the top 10 words of the topic are in one of our state words then increase the counter by 1
    if (length(intersect(words_to_check, c(single_word_states) )) > 0 ) {
      STATE[i] <- 'Y'
    } else if ( any(vapply(two_word_states_list, FUN = function(x) all(x %in% words_to_check), FUN.VALUE=T))) {
      STATE[i] <- 'Y'
    }
    # otherwise counter stays the same
  }
  
  mat %>% 
    as.data.frame() %>% 
    unite(sep = ", ", col = words) %>%
    mutate(STATE = STATE) %>%
    write_csv(file = file_name)
  
}

# where plots will be stored
plots <- list()
counter <- 0 
for (obj in results[3]) { 
  
  counter <- counter + 1
  
  n_topics <- obj$settings$dim$K
  n_documents <- obj$settings$dim$N
  
  topics <- paste0('Topic', 1:n_topics)
  proportions <- colSums(obj$theta) / n_documents 
  frexes <- apply(labelTopics(obj, n = 10)$frex, MARGIN = 1, FUN = function(x) paste(x, collapse = ', ' ))
  file <- paste0(n_topics, "_topics_", n_documents, "_documents_automated.csv")
  state <- read_csv(file, show_col_types = F)
  colors1 <- ifelse(state$STATE == "Y", 'red', 'grey35')
  colors2 <- ifelse(state$STATE == "Y", 'red', 'black')
  
  df <- data.frame(topic = topics, proportion = proportions, frex = frexes, col1 = colors1, col2 = colors2)
  
  df <- df[order(df$proportion, decreasing = T), ]
  
  df$topic <- ordered(df$topic, levels = df$topic[n_topics:1])
  
  if (n_topics == 60) ylim <- 0.09 # if any words do not fit in the plot, you might want to play with these
  if (n_topics == 120) ylim <- 0.07
  
  p <- ggplot(df, aes(x = topic, y = proportion)) +
    geom_col(fill = df$col1, width = 0.5) +
    coord_flip() +
    geom_text(aes(label = frex, hjust = -0.1), color = df$col2, size = 2.5) +
    ylim(0, ylim) +
    theme_bw() +
    theme(aspect.ratio = 3/4) +
    theme(axis.text.y= element_blank(), axis.ticks.y = element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank() ) +
    xlab('') +
    ylab('Expected Topic Proportions') +
    labs(title = paste0('Top Topics (', n_topics, " Topics and ", n_documents, " Documents)"))
  
  plot_name <- paste0(n_topics, "_topics_", n_documents, "_documents")
  plots[[counter]] <- list(plot = p, plot_name = plot_name)
  
}

#find proportion of topics that are state related
with(df, sum(proportion[col1 == 'red'])) #0.1724156


################ 120 Tweets Document Def
for (result in results[4]) {
  
  K <- result$settings$dim$K
  N <- result$settings$dim$N
  file_name <- paste0(K, '_topics_', N, "_documents_automated.csv")
  
  mat <- labelTopics(result, n = 10, frexweight = 0.5)$frex
  
  # by default, assume it's not a state topic
  STATE <- rep('N', nrow(mat))
  
  for (i in 1:nrow(mat)) {
    # get the top 10 words for the topic at hand
    words_to_check <- mat[i,]
    
    # if any of the top 10 words of the topic are in one of our state words then increase the counter by 1
    if (length(intersect(words_to_check, c(single_word_states) )) > 0 ) {
      STATE[i] <- 'Y'
    } else if ( any(vapply(two_word_states_list, FUN = function(x) all(x %in% words_to_check), FUN.VALUE=T))) {
      STATE[i] <- 'Y'
    }
    # otherwise counter stays the same
  }
  
  mat %>% 
    as.data.frame() %>% 
    unite(sep = ", ", col = words) %>%
    mutate(STATE = STATE) %>%
    write_csv(file = file_name)
  
}

# where plots will be stored
plots <- list()
counter <- 0 
for (obj in results[4]) { 
  
  counter <- counter + 1
  
  n_topics <- obj$settings$dim$K
  n_documents <- obj$settings$dim$N
  
  topics <- paste0('Topic', 1:n_topics)
  proportions <- colSums(obj$theta) / n_documents 
  frexes <- apply(labelTopics(obj, n = 10)$frex, MARGIN = 1, FUN = function(x) paste(x, collapse = ', ' ))
  file <- paste0(n_topics, "_topics_", n_documents, "_documents_automated.csv")
  state <- read_csv(file, show_col_types = F)
  colors1 <- ifelse(state$STATE == "Y", 'red', 'grey35')
  colors2 <- ifelse(state$STATE == "Y", 'red', 'black')
  
  df <- data.frame(topic = topics, proportion = proportions, frex = frexes, col1 = colors1, col2 = colors2)
  
  df <- df[order(df$proportion, decreasing = T), ]
  
  df$topic <- ordered(df$topic, levels = df$topic[n_topics:1])
  
  if (n_topics == 60) ylim <- 0.09 # if any words do not fit in the plot, you might want to play with these
  if (n_topics == 120) ylim <- 0.07
  
  p <- ggplot(df, aes(x = topic, y = proportion)) +
    geom_col(fill = df$col1, width = 0.5) +
    coord_flip() +
    geom_text(aes(label = frex, hjust = -0.1), color = df$col2, size = 2.5) +
    ylim(0, ylim) +
    theme_bw() +
    theme(aspect.ratio = 3/4) +
    theme(axis.text.y= element_blank(), axis.ticks.y = element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank() ) +
    xlab('') +
    ylab('Expected Topic Proportions') +
    labs(title = paste0('Top Topics (', n_topics, " Topics and ", n_documents, " Documents)"))
  
  plot_name <- paste0(n_topics, "_topics_", n_documents, "_documents")
  plots[[counter]] <- list(plot = p, plot_name = plot_name)
  
}

#find proportion of topics that are state related
with(df, sum(proportion[col1 == 'red'])) #0.1324555

################################################################################
################ Put Into a Dataframe
################################################################################
# Create Vectors
Topics <- c('60-topics','120-topics')
Tweets <- c( "17 (0.17)", "24 (0.13)")
Legislators <- c( "34 (0.31)", "43 (0.21)")

# Create DataFrame
df <- data.frame(Topics,Tweets,Legislators)

# save DataFrame
write.csv(df, "Main_Table1.csv") 
