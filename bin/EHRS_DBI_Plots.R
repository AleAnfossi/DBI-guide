# Load the data
#data <- read.csv('Depression_Heart_Failure_DBI.csv', row.names = 1)
#data <- read.csv('Diabetes_type1_DBI.csv', row.names = 1)
#data <- read.csv('Neuroblastoma_DBI.csv', row.names = 1)
#data <- read.csv('Sepsis_DBI.csv', row.names = 1)
data <- read.csv('Spain_cardiac_arrest_DBI.csv', row.names = 1)

# Reshape data from wide to long format
data_long <- data %>%
  rownames_to_column(var = "metric") %>%
  pivot_longer(cols = starts_with("kmeans") | starts_with("hc_") | starts_with("dbscan"),
               names_to = "algorithm",
               values_to = "value")

# Create plots for each metric
plots <- data_long %>%
  group_by(metric) %>%
  group_split() %>%
  map(~ ggplot(.x, aes(x = algorithm, y = value)) +
        geom_line() +
        geom_point() +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::number_format(accuracy = 0.5)) +
        ggtitle(paste("DB Index for", unique(.x$metric))) +
        xlab("Algorithm") +
        ylab("DBI") +
        theme_grey())

# Arrange plots in a grid
plot_grid <- wrap_plots(plots, ncol = 2)  # Adjust `ncol` as needed

# Print the plot grid
print(plot_grid)