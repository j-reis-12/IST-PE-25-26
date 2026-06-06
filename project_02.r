library(ggplot2)

file <- "data/Petroleum&OtherLiquidFuels.txt"
data <- read.table(file, header = TRUE, sep = "", dec = ",", check.names = FALSE)

# Convert DATE column to Date class
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")

# Create the chronological line chart
p <- ggplot(data, aes(x = Date)) +
  geom_line(aes(y = LADiesel, color = "LADiesel"), linewidth = 0.8) +
  geom_line(aes(y = LAGasoline, color = "LAGasoline"), linewidth = 0.8) +
  geom_line(aes(y = MEAN, color = "MEAN"), linewidth = 0.8) +
  scale_color_manual(
    values = c(
      "LADiesel" = "#1f77b4",
      "LAGasoline" = "#ff7f0e",
      "MEAN" = "#2ca02c"
    )
  ) +
  labs(
    title = "YoY Variation of Petroleum Product Prices",
    subtitle = "Daily Year-over-Year Relative Variation (2020-2026)",
    x = "Date",
    y = "YoY Variation (%)",
    color = "Series"
  ) +
  theme_linedraw()

# Display the plot
print(p)

# Save as PNG
png("img/p2.png", width = 1800, height = 1200, res = 200)
print(p)
dev.off()
