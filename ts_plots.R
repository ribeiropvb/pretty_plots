
# Time Series Plot ####

library(ggplot2)
library(ggpmisc)

# Data
df <- economics[economics$date > as.Date("2000-01-01"), ]
# New column with the corresponding year for each date
df$year <- year(df$date)

theme_default <- theme_light()+
  theme(
    panel.grid.major = element_blank()
    , panel.grid.minor = element_blank()
  )

# Base
## Line

ggplot(df, aes(x = date, y = unemploy)) +
  geom_line()+
  theme_default

## Area

ggplot(df, aes(x = date, y = unemploy)) +
  geom_area(fill = "gray", alpha = 0.9) +
  geom_line()+
  theme_default

# Alteranative x-axis

ggplot(df, aes(x = date, y = unemploy)) +
  geom_line() +
  scale_x_date(date_labels = "%Y-%m-%d")+
  theme_default

# facted by year

ggplot(df, aes(x = date, y = unemploy))+
  geom_line()+
  scale_x_date(date_breaks = "4 months", date_labels = "%b")+
  facet_wrap(~year, scales = "free")+
  theme_default 

# Highlighted by horizontal line

max_date <- df %>% 
  mutate(
    diff = c(NA,unemploy %>% diff())
  ) %>% arrange(desc(diff)) %>% 
  .$date %>% .[2]

ggplot(df, aes(x = date, y = unemploy)) +
  geom_line() +
  geom_vline(
    xintercept = max_date,
    linetype = 2
    , color = 2
    , linewidth = 1
  )+
  theme_default

# Peaks and valleys

## Peaks

ggplot(df, aes(x = date, y = unemploy)) +
  geom_line() +
  stat_peaks(geom = "point", span = 15, color = "steelblue3", size = 2)+
  stat_peaks(geom = "label", span = 15, color = "steelblue3", angle = 0,
             hjust = -0.1, x.label.fmt = "%Y-%m-%d")+
  stat_peaks(geom = "rug", span = 15, color = "steelblue3", sides = "b")+
  theme_default

## Valleys

ggplot(df, aes(x = date, y = unemploy)) +
  geom_line() +
  stat_valleys(geom = "point", span = 11, color = "red", size = 2) +
  stat_valleys(geom = "label", span = 11, color = "red", angle = 0,
               hjust = -0.1, x.label.fmt = "%Y-%m-%d") +
  stat_valleys(geom = "rug", span = 11, color = "red", sides = "b")+
  theme_default

## Both
ggplot(df, aes(x = date, y = unemploy)) +
  geom_line() +
  stat_peaks(geom = "point", span = 15, color = "steelblue3", size = 2)+
  stat_peaks(geom = "label", span = 15, color = "steelblue3", angle = 0,
             hjust = -0.1, vjust = -0.1
             , x.label.fmt = "%Y-%m-%d")+
  stat_peaks(geom = "rug", span = 15, color = "steelblue3", sides = "b")+
  stat_valleys(geom = "point", span = 11, color = "red", size = 2) +
  stat_valleys(geom = "label", span = 11, color = "red", angle = 0,
               hjust = -0.1, x.label.fmt = "%Y-%m-%d") +
  stat_valleys(geom = "rug", span = 11, color = "red", sides = "b")+
  theme_default 

# Plot arima forecast

library(forecast)
d.arima <- auto.arima(df$unemploy %>% ts())

## Parameter of the ARIMA model
arimaorder(d.arima)

## AIC / AICc / BIC

cat('AIC = ', d.arima$aic, ' / AICc = ', d.arima$aicc, ' / BIC = ', d.arima$bic)

# Select model info
accuracy(d.arima)

d.forecast <- forecast(d.arima, level = c(95), h = 10)
autoplot(d.forecast)+
  ggtitle('')+
  ylab('unemploy')+
  theme_default 

