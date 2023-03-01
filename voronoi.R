# install.packages("quantmod")
library(quantmod)

# Dates
start <- "2020-10-01"
end <- "2021-01-01"

# Get the data
getSymbols("^GSPC", 
           from = start, to = end,
           src = "yahoo") 

# Plot the data
chart_Series(GSPC,
             TA = list("add_EMA(n = 20, col = 4,
                                lwd = 2)",
                       "add_EMA(n = 5, col = 2,
                                lwd = 2)")) 


chartSeries(
  GSPC,
  theme = chartTheme("white"),
  name = "SP500",  
  TA = list("addBBands(n = 10)",
            "addVo()",
            "addEMA(20)",
            "addEMA(10, col = 2)")
) 

# install.packages("ggvoronoi")
# install.packages("ggplot2")
library(ggvoronoi)
library(ggplot2)

ggplot(df, aes(x, y, fill = dist)) +
  geom_voronoi() +
  stat_voronoi(geom = "path") +
  geom_point() +
  scale_fill_gradient(
    low = "#F9F9F9",
    high = "#444444"
  )+
  theme_void()

