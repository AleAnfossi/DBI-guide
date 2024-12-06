reorder_plot_data <- function(plot_data) {
  # Reorder the dataframe by the 'Value' column in descending order
  plot_data <- plot_data[order(-plot_data$Value), ]
  return(plot_data)
}