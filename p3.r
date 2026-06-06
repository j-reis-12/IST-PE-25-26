library(ggplot2)

file <- "data/Jobs_and_Growth.csv"
data <- read.csv(file, stringsAsFactors = FALSE)

# Filter only the correct indicator and countries
subset_data <- data[
  data$Sub.indicator.Name == "Employment in forestry" &
  data$Member.State.Name %in% c("Germany", "Lithuania"),
]

# Convert Year to factor
subset_data$Time.Period <- factor(subset_data$Time.Period)

# Create the bar chart
p <- ggplot(subset_data, aes(x = Time.Period, y = Data, fill = Member.State.Name)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Employment in Forestry (2008–2022)",
    subtitle = "Comparison between Germany and Lithuania",
    x = "Year",
    y = "Employment in Forestry (% of total employment)",
    fill = "Country"
  ) +
  theme_classic()

# Display the plot
print(p)

# Save as PNG
png("img/p3.png", width = 1800, height = 1200, res = 200)
print(p)
dev.off()