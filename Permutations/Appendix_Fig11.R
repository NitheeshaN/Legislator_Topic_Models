
# input     : ../STM_Models/results.rds
# input     : temp/permutation_results.rds
# output    : results/tweets_permutation_results.pdf
# output    : results/permutation_analysis_description.txt

library(stm)
library(quanteda)
library(dplyr)
library(magrittr)
library(ggplot2)

source('../get_n_state_topics.R')
if(!dir.exists('results')) dir.create('results')
  
original_results <- readRDS('../Tweets_data/results.rds')

permutation_results <- readRDS('temp/permutation_results.rds')

get_visualization_info <- function(results){

  visualization_df <- data.frame(K = numeric(), 
                                 N = character(), 
                                 state_count = numeric())
  
  for (result in results) {
  
    K <- result$settings$dim$K
    N <- result$settings$dim$N
    
    state_count <- get_n_state_topics(result)
    
    visualization_df <- rbind(visualization_df, 
          data.frame(K = K, N = N, state_count = state_count))
  
  }
  
  return(visualization_df)

}


vis_orig <- get_visualization_info(original_results)
# we will only compare the aggregated versions
vis_orig <- vis_orig[vis_orig$N == 17953,]

# goes to a table that shows the results from the automated approach
vis_perm <- get_visualization_info(permutation_results)

df <- data.frame(K = numeric(), state_count = numeric(), moe = numeric())

for (K in c(60, 120) ) {
  mean_state_count <- mean(vis_perm$state_count[which(vis_perm$K == K)])
  moe <- t.test(vis_perm$state_count[which(vis_perm$K == K)])$conf.int[2] - mean_state_count
  df <- rbind(df, data.frame(K = K, state_count = mean_state_count, moe = moe))
  
}


pdf('results/wikip_permutation_results.pdf', width = 4, height = 3)
ggplot(vis_orig, aes(x = factor(K), y = state_count)) +
  geom_point(color = 'red') +
  geom_pointrange(aes(ymin = state_count-moe, ymax = state_count+moe), colour = 'black', data = df) +
  labs(x = 'Number of Topics',
       y = 'Number of State Related Topics')

dev.off()


# means and confidence intervals for description/ text purposes

mean60 <- mean(vis_perm$state_count[which(vis_perm$K == 60)])
ci60 <- t.test(vis_perm$state_count[which(vis_perm$K == 60)])$conf.int |> round(2)

mean120 <- mean(vis_perm$state_count[which(vis_perm$K == 120)])
ci120 <- t.test(vis_perm$state_count[which(vis_perm$K == 120)])$conf.int |> round(2)

sink('results/permutation_analysis_description.txt')
cat("Mean number of state related topics in the 60 topic permutations is", mean60, "and the confidence interval is", ci60, '\n\n\n')
cat("Mean number of state related topics in the 120 topic permutations is", mean120, "and the confidence interval is", ci120, '\n\n\n')
sink()
