
# install.packages('vioplot')
library("vioplot")

# Sample data set
df <- chickwts

vioplot(
  weight ~ feed
  , data = df
  , col = grey.colors(6, rev = T)
  , border = NA
)

set.seed(2023)
df2 <- df %>% mutate(
  group = sample(c('A','B'), dim(df)[1], T)
) %>% select( -feed )

maximo <- df2$weight %>% max
minimo <- df2$weight %>% min

# Plot each side and join them
vioplot(
  df2 %>% filter(group == 'A')
  , side = "right"   # Right side
  , colMed = "#FFFFFF" # Color of the median point
  , col = grey.colors(2)[1]
  , ylim = c( minimo, maximo )
)  # Color for the right side
vioplot(
  df2 %>% filter(group == 'B')
  , side = "left"     # Left side
  , colMed = "#FFFFFF" # Color of the median point
  , col = grey.colors(2)[2]   # Color for the left side
  , add = TRUE
)        # Add it to the previous plot
legend(
  "bottomright",
  legend = c("Group A", "Group B"),
  fill = c(grey.colors(2)[1], grey.colors(2)[2])
)


