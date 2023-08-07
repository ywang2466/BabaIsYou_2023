library(dplyr)

make.ona.plot <- function(set, 
                          plot.name, 
                          flip_x = FALSE
) {
  if(flip_x == TRUE){
    set$points$MR1 <- (-1)*set$points$MR1
    set$rotation$nodes$MR1 <- (-1)*set$rotation$nodes$MR1
  }
  
  # Grand Mean
  grand_mean <- plot(set, title = paste0(plot.name,"Grand Mean")) %>%
    units(
      points=set$points[set$points$LevelID == 1,],
      points_color = c("red"),
      show_mean = TRUE, show_points = FALSE, with_ci = TRUE) %>%
    units(
      points=set$points[set$points$LevelID == 1,],
      points_color = c("red"),
      show_mean = TRUE, show_points = FALSE, with_ci = TRUE) %>%
    edges(
      weights = set$line.weights,
      # edge_arrow_saturation_multiplier = 2,
      edge_color = c("black")) %>%
    nodes(
      # node_size_multiplier = node_size_multiplier, 
      node_labels = TRUE,
      self_connection_color = c("black"))
  print(grand_mean)
}