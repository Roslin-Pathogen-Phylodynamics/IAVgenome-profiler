## Host type phylogenetic persistence  

# Calculate host phylogenetic persistence per subtype 

library(ggplot2)
library(ggtree)
library(RColorBrewer)
library(toolkitSeqTree)

load('H5Nx_ha_mcc_fortified.Rdata')

plot_dir <- paste0('mcc_host_persistence') 
dir.create(plot_dir)


tree_dat <- tree_persistence(tree_dat, 'host', cond_var = 'subtype',
                             output_colname = 'host_persistence')


# Define host breaks
tree_dat$host <- gsub('Mammal', 'Mammalia', tree_dat$host)
host_breaks <- rev(c('Anseriformes', 'Galliformes', 'Charadriiformes',
                     'Accipitriformes', 'Mammalia', 'Falconiformes',
                     'Pelecaniformes', 'Passeriformes', 'Strigiformes',
                     'Suliformes'))

# Subtype breaks with colours
subtype_pal <- brewer.pal(8, 'Set2')[c(1,3,4,5,6,7,2)]
subtype_breaks <- c('H5N1', 'H5N2', 'H5N3', 'H5N4',
                    'H5N5', 'H5N6', 'H5N8')

xlim_min <- 0
xlim_max <- 4.5
text_size <- 14

## Plots with full distribution for individual subtypes

# H5N1 - Plot distribution and median, IQR
ggplot(tree_dat[tree_dat$subtype == 'H5N1',],
       aes(x = host_persistence,
           y = factor(host, level = host_breaks))) + 
  geom_jitter(pch = 21, width = 0, height = 0.3, alpha = 0.5,
              fill = subtype_pal[subtype_breaks %in% c('H5N1')],
              show.legend = F) +
  geom_pointrange(stat = "summary",
                  fun.min = function(z) {quantile(z,0.25)},
                  fun.max = function(z) {quantile(z,0.75)},
                  fun = median,
                  pch = 21, size = 1, show.legend = F,
                  fill = subtype_pal[subtype_breaks %in% c('H5N1')]) +
  scale_x_continuous(breaks = seq(0, 10, 0.5)) +
  scale_y_discrete(limits = host_breaks) +
  xlab('Phylogenetic persistence (years)') +
  coord_cartesian(xlim = c(xlim_min, xlim_max)) + 
  theme_minimal() +
  theme(text = element_text(size = text_size),
        axis.title.y = element_blank())

ggsave(paste0(plot_dir, '/persist_host_h5n1_dist.png'),
       bg = 'white', dpi = 320, height = 7, width = 5)

# H5N8 - Plot distribution and median, IQR
ggplot(tree_dat[tree_dat$subtype == 'H5N8',],
       aes(x = host_persistence,
           y = factor(host, level = host_breaks))) + 
  geom_jitter(pch = 21, width = 0, height = 0.3, alpha = 0.5,
              fill = subtype_pal[subtype_breaks %in% c('H5N8')],
              show.legend = F) +
  geom_pointrange(stat = "summary",
                  fun.min = function(z) {quantile(z,0.25)},
                  fun.max = function(z) {quantile(z,0.75)},
                  fun = median,
                  pch = 21, size = 1, show.legend = F,
                  fill = subtype_pal[subtype_breaks %in% c('H5N8')]) +
  scale_x_continuous(breaks = seq(0, 10, 0.5)) +
  scale_y_discrete(limits = host_breaks) +
  xlab('Phylogenetic persistence (years)') +
  coord_cartesian(xlim = c(xlim_min, xlim_max)) + 
  theme_minimal() +
  theme(text = element_text(size = text_size),
        axis.title.y = element_blank())

ggsave(paste0(plot_dir, '/persist_host_h5n8_dist.png'),
       bg = 'white', dpi = 320, height = 7, width = 5)


## Summary DF and comparison plot
## Create summary df for plot
new_host_breaks <- c('Sphenisciformes', 'Podicipediformes', 'Gruiformes',
                     'Suliformes', 'Passeriformes', 'Falconiformes',
                     'Pelecaniformes', 'Mammalia', 'Accipitriformes',
                     'Charadriiformes', 'Anseriformes', 'Galliformes')

res <- data.frame(host = new_host_breaks,
                  med_h5n1 = NA,
                  lower_h5n1 = NA,
                  upper_h5n1 = NA,
                  n_h5n1 = NA,
                  med_h5n8 = NA,
                  lower_h5n8 = NA,
                  upper_h5n8 = NA,
                  n_h5n8 = NA)

for (i in 1:nrow(res)) {
  curr_host <- res$host[i]
  
  res$n_h5n1[i] <- sum(tree_dat$subtype %in% c('H5N1') & 
                         tree_dat$host == curr_host &
                         tree_dat$isTip)
  res$n_h5n8[i] <- sum(tree_dat$subtype %in% c('H5N8') & 
                         tree_dat$host == curr_host &
                         tree_dat$isTip)
  
  # h5n1
  host_h5n1 <- tree_dat$host_persistence[which(tree_dat$subtype == 'H5N1' &
                                                 tree_dat$host == curr_host)]
  host_h5n1 <- host_h5n1[is.na(host_h5n1) == F]
  
  res$med_h5n1[i] <- median(host_h5n1)
  res$lower_h5n1[i] <- quantile(host_h5n1, 0.25)[[1]]
  res$upper_h5n1[i] <- quantile(host_h5n1, 0.75)[[1]]
  
  # h5n8
  host_h5n8 <- tree_dat$host_persistence[which(tree_dat$subtype == 'H5N8' &
                                                 tree_dat$host == curr_host)]
  host_h5n8 <- host_h5n8[is.na(host_h5n8) == F]
  
  res$med_h5n8[i] <- median(host_h5n8)
  res$lower_h5n8[i] <- quantile(host_h5n8, 0.25)[[1]]
  res$upper_h5n8[i] <- quantile(host_h5n8, 0.75)[[1]]
}

colour <- 'grey50'
ggplot(res) +
  
  # IQR H5N1
  annotate(geom = 'segment', y = 11.9, yend = 11.9, col = colour,
           x = res$lower_h5n1[res$host == 'Galliformes'],
           xend = res$upper_h5n1[res$host == 'Galliformes']) +
  annotate(geom = 'segment', y = 10.9, yend = 10.9, col = colour,
           x = res$lower_h5n1[res$host == 'Anseriformes'],
           xend = res$upper_h5n1[res$host == 'Anseriformes']) +
  annotate(geom = 'segment', y = 9.9, yend = 9.9, col = colour,
           x = res$lower_h5n1[res$host == 'Charadriiformes'],
           xend = res$upper_h5n1[res$host == 'Charadriiformes']) +
  annotate(geom = 'segment', y = 8.9, yend = 8.9, col = colour,
           x = res$lower_h5n1[res$host == 'Accipitriformes'],
           xend = res$upper_h5n1[res$host == 'Accipitriformes']) +
  annotate(geom = 'segment', y = 7.9, yend = 7.9, col = colour,
           x = res$lower_h5n1[res$host == 'Mammalia'],
           xend = res$upper_h5n1[res$host == 'Mammalia']) +
  annotate(geom = 'segment', y = 6.9, yend = 6.9, col = colour,
           x = res$lower_h5n1[res$host == 'Pelecaniformes'],
           xend = res$upper_h5n1[res$host == 'Pelecaniformes']) +
  annotate(geom = 'segment', y = 5.9, yend = 5.9, col = colour,
           x = res$lower_h5n1[res$host == 'Falconiformes'],
           xend = res$upper_h5n1[res$host == 'Falconiformes']) +
  annotate(geom = 'segment', y = 4.9, yend = 4.9, col = colour,
           x = res$lower_h5n1[res$host == 'Passeriformes'],
           xend = res$upper_h5n1[res$host == 'Passeriformes']) +
  annotate(geom = 'segment', y = 3.9, yend = 3.9, col = colour,
           x = res$lower_h5n1[res$host == 'Suliformes'],
           xend = res$upper_h5n1[res$host == 'Suliformes']) +
  annotate(geom = 'segment', y = 2.9, yend = 2.9, col = colour,
           x = res$lower_h5n1[res$host == 'Gruiformes'],
           xend = res$upper_h5n1[res$host == 'Gruiformes']) +
  annotate(geom = 'segment', y = 0.9, yend = 0.9, col = colour,
           x = res$lower_h5n1[res$host == 'Sphenisciformes'],
           xend = res$upper_h5n1[res$host == 'Sphenisciformes']) +
  
  # IQR H5N8
  annotate(geom = 'segment', y = 12.1, yend = 12.1, col = colour,
           x = res$lower_h5n8[res$host == 'Galliformes'],
           xend = res$upper_h5n8[res$host == 'Galliformes']) +
  annotate(geom = 'segment', y = 11.1, yend = 11.1, col = colour,
           x = res$lower_h5n8[res$host == 'Anseriformes'],
           xend = res$upper_h5n8[res$host == 'Anseriformes']) +
  annotate(geom = 'segment', y = 9.1, yend = 9.1, col = colour,
           x = res$lower_h5n8[res$host == 'Accipitriformes'],
           xend = res$upper_h5n8[res$host == 'Accipitriformes']) +
  annotate(geom = 'segment', y = 1.9, yend = 1.9, col = colour,
           x = res$lower_h5n1[res$host == 'Podicipediformes'],
           xend = res$upper_h5n1[res$host == 'Podicipediformes']) +
  
  
  geom_point(aes(x = med_h5n1, y = as.character(host), size = n_h5n1),
             col = 'grey50', pch = 21,
             position = position_nudge(y = -0.1),
             fill = subtype_pal[subtype_breaks == 'H5N1']) +
  geom_point(aes(x = med_h5n8, y = as.character(host), size = n_h5n8),
             col = 'grey50', pch = 21,
             position = position_nudge(y = 0.1),
             fill = subtype_pal[subtype_breaks == 'H5N8']) +
  geom_hline(yintercept = seq(0.5, 20.5, 1), lwd = 0.2, col = 'grey80') +
  scale_size_continuous(breaks = c(1, 10, 100), range = c(2, 12), name = 'N tips') +
  scale_x_continuous(limits = c(0, 3), breaks = seq(0, 5, 0.5)) +
  scale_y_discrete(limits = new_host_breaks) +
  xlab('Phylogenetic persistence (years)') +
  coord_cartesian(clip = 'off') + # stops clipping of large points at top
  theme_minimal() +
  theme(text = element_text(size = 18),
        axis.title.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  guides(size = guide_legend(override.aes = list(shape = 19, col = 'grey50'))) # change pch to 19 to remove fill

ggsave(paste0(plot_dir, '/persist_host_h5n1_h5n8.png'),
       bg = 'white', dpi = 320, height = 8, width = 6.5)
