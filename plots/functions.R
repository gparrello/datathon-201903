pacman::p_load(
  "rbokeh",
  "dplyr"
)

outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}

f_plot_hist <- function(dt){
  # To plot a grid of histograms
  dtNums <- unlist(lapply(dt, is.numeric))
  # http://bit.ly/2Bl3BSb
  plot_list <- vector(mode = 'list', length = 0)
  for (i in names(dtNums)){
    x <- dt[, ..i]
    #hist_bins <- length(seq(min(x),max(x),by=((max(x) - min(x))/(length(x)-1))))/100
    # http://bit.ly/2QO5KiT
    vx <- unlist(unname(x))
    # hist_bins <- diff(range(vx)) / (2 * IQR(vx) / length(vx)^(1/3))
    # browser()
    # http://bit.ly/2C5TXnN
    plot_list[[i]] <- figure(xlab = NULL) %>%
      # ly_hist(vx, freq=TRUE, breaks=hist_bins) %>%
      ly_hist(vx, freq=TRUE) %>%
      # ly_density(x) %>%
      x_axis(number_formatter = "numeral", format = "0.") %>%
      theme_axis("x", major_label_orientation = 45)
  }
  plot_num <- length(plot_list)
  total_columns = 2
  p <- grid_plot(
    plot_list,
    ncol = total_columns#,
    #nrow = plot_num,
    #height = plot_height*plot_num/total_columns^2,
    # width = plot_width
  )
  return(p)
}
