## Calculate local branching index using temporally-restricted subtrees

library(reshape2)
library(ggplot2)
library(RColorBrewer)

load('H5Nx_ha_mcc_fortified.Rdata')

### Options
window <- 0.5
increment <- 14/365
lbi_scale <- 0.1
overall_start <- 2020
overall_end <- 2023.25
### Options

plot_dir <- paste0('mcc_lbi_subtype') 
dir.create(plot_dir)


times <- seq(overall_start, overall_end, by = increment)
res <- expand.grid(start = times,
                   label = c('H5N1', 'H5N8'))

res$end <- res$start + window
res$midpoint <- (res$start + res$end) / 2
res$n_h5n1 <- NA
res$lbi_min_h5n1 <- NA
res$lbi_max_h5n1 <- NA
res$n_h5n8 <- NA
res$lbi_min_h5n8 <- NA
res$lbi_max_h5n8 <- NA

tip_threshold <- 10
for (i in 1:nrow(res)) {
  
  tips_in_slice <- tree_dat$node[tree_dat$isTip == T &
                                   tree_dat$date_frac >= res$start[i] &
                                   tree_dat$date_frac < res$end[i]]
  
  n_h5n1_in_slice <- sum('H5N1' == tree_dat$subtype[tree_dat$node %in% tips_in_slice])
  n_h5n8_in_slice <- sum('H5N8' == tree_dat$subtype[tree_dat$node %in% tips_in_slice])
  
  res$n_h5n1[i] <- n_h5n1_in_slice
  res$n_h5n8[i] <- n_h5n8_in_slice
  
  if (length(tips_in_slice) > 2) {
    temp <- local_branching_index_set(tree_dat, tree@phylo,
                                      nodes = tips_in_slice, use_nodes = T,
                                      scale = lbi_scale)
    
    if (n_h5n1_in_slice > tip_threshold) {
      res$lbi_min_h5n1[i] <- min(temp$lbi[temp$subtype == 'H5N1'], na.rm = T)
      res$lbi_max_h5n1[i] <- max(temp$lbi[temp$subtype == 'H5N1'], na.rm = T)
    }
    
    if (n_h5n8_in_slice > tip_threshold) {
      res$lbi_min_h5n8[i] <- min(temp$lbi[temp$subtype == 'H5N8'], na.rm = T)
      res$lbi_max_h5n8[i] <- max(temp$lbi[temp$subtype == 'H5N8'], na.rm = T)
    }
    
  }
  
}


# Highest LBI per subtype through time separated for H5N1 and H5N8
res <- res[, c('midpoint', 'lbi_max_h5n1', 'lbi_max_h5n8')]
res <- melt(res, id.vars = 'midpoint')
names(res) <- c('midpoint', 'label', 'lbi')


res$date <- decimal2Date(res$midpoint)
res$doy <- yday(res$date)

ggplot(res, aes(x = midpoint, y = lbi, colour = label)) +
  geom_line(lwd = 1) +
  labs(x = '', y = 'Max. local branching index', colour = '') +
  scale_colour_manual(breaks = c('lbi_max_h5n1',
                                 'lbi_max_h5n8'),
                      labels = c('H5N1', 'H5N8'),
                      values = brewer.pal(8, 'Dark2')[c(1,2)]) +
  scale_x_continuous(limits = c(2020, 2023.25)) +
  scale_y_continuous(limits = c(0.25, 2.5)) +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9),
        text = element_text(size = 14))
ggsave(paste0(plot_dir, '/HA_2344b_lbi_subtype_w0.5_s0.1.png'),
       bg = 'white', dpi = 320, height = 7, width = 4)
