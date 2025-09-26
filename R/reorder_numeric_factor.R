reorder_numeric_factor <- function(cluster = NA) {
  cluster <- as.data.frame(cluster)
  tab <- as.data.frame(table(cluster$cluster))
  tab <- tab[order(tab$Freq, decreasing = T),]
  cluster$cluster.relabel <- NA
  for (i in 1:nrow(tab)) {
    curr_level <- tab$Var1[i]
    cluster$cluster.relabel <- ifelse(cluster$cluster == curr_level,
                                      i,
                                      cluster$cluster.relabel)
  }
  cluster$cluster.relabel
}