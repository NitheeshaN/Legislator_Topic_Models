
library(readr)
library(ggplot2)
library(stm)

results <- readRDS("../STM_Models/results.rds")


# where plots will be stored
plots <- list()

counter <- 0 
for (obj in results) {
  
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
    geom_text(aes(label = frex, hjust = -0.1), color = df$col2, size = 12.5) +
    ylim(0, ylim) +
    theme_bw() +
    theme(aspect.ratio = 3/4) +
    theme(axis.text.y= element_blank(), axis.ticks.y = element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank() ) +
    xlab('') +
    ylab('Expected Topic Proportions') +
    labs(title = paste0('Top Topics (', n_topics, " Topics and ", n_documents, " Documents)")) +
    theme(plot.title = element_text(size=70), 
          axis.text.x = element_text(size =50), axis.title.x = element_text(size = 70) )
  
  plot_name <- paste0(n_topics, "_topics_", n_documents, "_documents")
  plots[[counter]] <- list(plot = p, plot_name = plot_name)
  
}

for (j in 1:length(plots)) {
  plot_file_name <- paste0(plots[[j]]$plot_name, '.png')
  png(plot_file_name, width = 6000, height = 4500)
  print(plots[[j]]$plot)  
  dev.off()
}
