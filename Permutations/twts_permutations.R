
# input     :       ../_data/dfm_sm_leg60.rds
# output    :       temp/permutation_results.rds

library(stm)
library(quanteda)
library(doParallel)
library(foreach)

sessionInfo()

dfm_sm_leg <- readRDS("../Tweets_Data/dfm_sm_leg60.rds")

len <- length(docvars(dfm_sm_leg)$user.screen_name)
n_permutations <- 10

set.seed(0)

indices <- lapply(1:n_permutations, FUN = function(x) sample(1:len, size = len, replace = F))

K_list <- c(60, 120)

eg <- expand.grid(1:n_permutations, K_list, stringsAsFactors = F)

if(!dir.exists('temp')) dir.create('temp')

registerDoParallel(cores = 20)

results <- foreach(i = 1:nrow(eg)) %dopar% {
  ind <- indices[[eg[i, 'Var1']]]
  k <- eg[i, 'Var2']
  
  print(paste0('Permuting Indices (', i, ')'))
  print(paste0('Number of topics: ', k))
  
  docvars(dfm_sm_leg)$permuted <- docvars(dfm_sm_leg)$user.screen_name[ind]
  print('New permutation created')
  dfm_permuted <- dfm_group(dfm_sm_leg, groups = permuted)
  print('Grouped dfm by the new permuted handle')
  stm_object <- convert(dfm_permuted, to="stm")
  print('dfm converted to an stm object')

  print('Starting to fit the stm model')
  stm_result <- stm(documents = stm_object$documents,
                    vocab = stm_object$vocab,
                    K=k,
                    data=stm_object$meta,
                    init.type="Spectral",
                    seed = 178)
  
  stm_result
  
}

saveRDS(results, file = 'temp/permutation_results.rds')

