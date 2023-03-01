
source('https://raw.githubusercontent.com/ribeiropvb/Util/main/install_packages_if_not_exist.R')
install_packages_if_not_exist(list.of.packages = c('ggplot2', 'dplyr', 'hrbrthemes'))

library(ggplot2)
library(dplyr)
library(hrbrthemes)

theme_default <- theme_light()+
  theme(
    panel.grid.major = element_blank()
    , panel.grid.minor = element_blank()
  )

set.seed(1)

df <- data.frame(
  name=c("north","south","south-east","north-west","south-west","north-east","west","east"),
  val=sample(seq(1,10), 8 )
)

# Basic

df %>%
  arrange(val) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(name=factor(name, levels=name)) %>%   # This trick update the factor levels
  ggplot( aes(x=name, y=val)) +
  geom_segment( aes(xend=name, yend=0)) +
  geom_point( size=4, color="orange") +
  coord_flip() +
  theme_bw() +
  xlab("")

# Value

df %>%
  arrange(val) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(name=factor(name, levels=name)) %>%   # This trick update the factor levels
  ggplot( aes(x=name, y=val)) +
  geom_segment( aes(xend=name, yend=0))+
  geom_point(size = 7.5, pch = 21, bg = 'pink', color = 'pink') +
  geom_text(aes(label = val), color = "#444444", size = 3) +
  scale_x_discrete(labels = paste0("G_", 1:10)) +
  coord_flip() +
  theme_minimal() 

# Annotation

# Create data
set.seed(1000)
data <- data.frame(
  x=LETTERS[1:26], 
  y=abs(rnorm(26))
)

# Reorder the data
data <- data %>%
  arrange(y) %>%
  mutate(x=factor(x,x))

# Plot
ggplot(data, aes(x=x, y=y))+
  geom_segment(
    aes(x=x, xend=x, y=0, yend=y), 
    color=ifelse(data$x %in% c("A","D"), "orange", "grey"), 
    size=ifelse(data$x %in% c("A","D"), 1.3, 0.7)
  )+
  geom_point(
    color=ifelse(data$x %in% c("A","D"), "orange", "grey"), 
    size=ifelse(data$x %in% c("A","D"), 5, 2)
  )+
  coord_flip()+
  theme(
    legend.position="none"
  )+
  xlab("")+
  ylab("Value of Y")+
  ggtitle("How did groups A and D perform?")+
  annotate(
    "text", x=grep("D", data$x)
    , y=data$y[which(data$x=="D")]*1.2
    , label="Group D is very impressive"
    , color="orange", size=4 , angle=0
    , fontface="bold", hjust=0
  )+
  annotate(
    "text", x = grep("A", data$x)
    , y = data$y[which(data$x=="A")]*1.2, 
    label = paste("Group A is not too bad\n (val=",data$y[which(data$x=="A")] %>% round(2),")",sep="" )
    , color="orange", size=4 , angle=0, fontface="bold", hjust=0
  )+
  theme_default
