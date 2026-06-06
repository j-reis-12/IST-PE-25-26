library(ggplot2)

file <- "data/hcvdat0.csv"
data <- read.csv(file, stringsAsFactors = FALSE)

# Convert Category to factor
data$Category <- factor(data$Category)

# Create the boxplot
p <- ggplot(data, aes(x = Category, y = CHOL)) +
  geom_boxplot(fill = "#87CEEB", color = "black") +
  labs(
    title = "Distribution of CHOL Levels by Clinical Category",
    x = "Clinical Category",
    y = "CHOL (mg/dL)"
  ) +
  theme_light()

# Display the plot
print(p)

# Save as PNG
png("img/p1.png", width = 1800, height = 1200, res = 200)
print(p)
dev.off()
