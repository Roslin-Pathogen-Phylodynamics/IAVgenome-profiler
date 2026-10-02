## Track phylogenetic diversity, per subtype, through time

library(toolkitSeqTree)
library(dplyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)

load('H5Nx_ha_mcc_fortified.RData')


### Options
overall_start <- 2020
overall_end <- 2023.25
increment <- 28/365
window <- 0.25
### Options


plot_dir <- paste0('mcc_diversity_subtype') 
dir.create(plot_dir)


times <- seq(overall_start, overall_end, by = increment)
res <- data.frame(start = times,
                  end = times + window,
                  midpoint = (times + (times + window)) / 2,
                  diversity_h5n1 = NA,
                  diversity_h5n8 = NA)

for (t in 1:length(times)) {
  lower <- times[t]
  upper <- lower + window
    
  res$diversity_h5n1[res$start == lower] <- tree_slice_diversity(tree_dat, tree,
                                                                 lower, upper, 
                                                                 cond_var = 'subtype',
                                                                 cond_val = 'H5N1')
  
  res$diversity_h5n8[res$start == lower] <- tree_slice_diversity(tree_dat, tree,
                                                                 lower, upper, 
                                                                 cond_var = 'subtype',
                                                                 cond_val = 'H5N8')
}

# Melt DF prior to plotting
x <- res[, c('midpoint', 'diversity_h5n1', 'diversity_h5n8')]
x <- melt(x, id.vars = c('midpoint'))
x$subtype_tree <- paste(x$variable, x$tree_id, sep = '_')
head(x)

# Line plot with lines grouped by 'subtype_tree' combo var
ggplot(x, aes(x = midpoint, y = value, colour = variable)) +
  geom_line(aes(group = subtype_tree), lwd = 1, alpha = 1) +
  labs(x = '', y = 'Phylogenetic diversity', colour = '') +
  scale_colour_manual(breaks = c('diversity_h5n1',
                                 'diversity_h5n8'),
                      labels = c('H5N1', 'H5N8'),
                      values = brewer.pal(8, 'Dark2')[c(1,2)]) +
  theme_minimal() +
  theme(text = element_text(size = 15),
        legend.position = c(0.9, 0.9),
        axis.title.x = element_blank())

ggsave(paste0(plot_dir, '/HA_2344b_diversity_time_subtype_', window, '.png'),
       bg = 'white', dpi = 320, height = 7, width = 4)


