scale_weights <- function(weights,scale) {
  if(is.null(scale)) {
    return(weights)
  } else if(length(unique(weights)) == 1) {
    weights <- rep(mean(scale), length(weights))
    return(weights)
  }  else {
    #Scale the values so that they lie in the interval (-1,1)
    weights <- (weights-mean(weights)) / (max(weights)-min(weights)) 
    #Then scale them into the region given by the scale parameter
    #This parameter should be a vector of length 2 with the upper and lower
    #boundaries of the scale
    weights <- (weights * (max(scale)-min(scale)) + mean(scale))
    return(weights)
  }
}
from_coords_to_plot <- function(x,y,domain,number_of_dimensions) {
  #normalize x and y so that they are in [0,1]
  xnorm <- (x-domain$left) / (domain$right-domain$left)
  #The y coordinates are "special" in that we want to show the first plot at the top,
  #but the coordinates start at the bottom, so we have to "invert" y coordinate first
  yinv <- domain$top - y
  ynorm <- (yinv-domain$bottom) / (domain$top-domain$bottom)
  x_plot_number <- as.integer(xnorm / (1/number_of_dimensions)) + 1
  y_plot_number <- as.integer(ynorm / (1/number_of_dimensions)) + 1
  return(c(x_plot_number,y_plot_number))
}
dataframe_row_to_html <- function(row) {
  if(is.null(row)) {
    return("")
  }
  value_strings <- sapply(names(row),function(name){return(paste0(name,": ",row[[name]],"<br>"))})
  return(paste0(value_strings,collapse=""))
}
incorporate_new_evalres <- function(old_evaluation_table,new_evalres) {
  if(is.null(old_evaluation_table)) {
    res <- data.frame(as.list(c(1,new_evalres$values)))
    names(res) <- make.names(c("time",all_eval_measures()))
    return(res)
  } else {
    return(rbind(old_evaluation_table,c(nrow(old_evaluation_table) + 1,new_evalres$values)))
  }
}