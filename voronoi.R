library(ggvoronoi)
library(ggplot2)

# Data
set.seed(1)
x <- sample(1:400, size = 100)
y <- sample(1:400, size = 100)
dist <- sqrt((x - 200) ^ 2 + (y - 200) ^ 2)

df <- data.frame(x, y, dist = dist)

ggplot(df, aes(x, y, fill = dist)) +
  geom_voronoi() +
  stat_voronoi(geom = "path") +
  geom_point() +
  scale_fill_gradient(
    low = "#F9F9F9",
    high = "#444444"
  )+
  theme_void()

