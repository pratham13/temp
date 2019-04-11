
# forecasting by various methods Analytics Vidhya -------------------------
library(tidyverse)
library(glue)
library(forcats)

# Time Series
library(timetk)
library(tidyquant)
library(tibbletime)

# Visualization
library(cowplot)

# Preprocessing
library(recipes)

# Sampling / Accuracy
library(rsample)
library(yardstick) 

# Modeling
library(keras)
library(kerasR)
library(tfruns)
library(ggplot2)
library(forecast)
library(thief)


data = fread("NSE-TATAGLOBAL11.CSV") 

data$Date = ymd(data$Date)

ggplot(data, aes(x = Date, y = Close)) + geom_point() + 
  geom_line()

train  = data[1:1000,]
test = data[1001:1235,]

summary(auto.arima(train$Close))
jj = function(x, option){
  apply(x$Close,2, option)
  
}

multi.func = function(x){
  ets_f = ets(x)
  arima_f = auto.arima(x)
  nnetar_f = nnetar(x)
  
  return(list(ets_f, arima_f))
}




# sunspots data -----------------------------------------------------------

sun_spots <- datasets::sunspot.month %>%
  tk_tbl() %>%
  mutate(index = as_date(index)) %>%
  as_tbl_time(index = index)

sun_spots

p1 <- sun_spots %>%
  ggplot(aes(index, value)) +
  geom_point(color = palette_light()[[1]], alpha = 0.5) +
  theme_tq() +
  labs( x = "time_horizon",
    title = "From 1749 to 2013 (Full Data Set)"
  )

p2 <- sun_spots %>%
  filter_time("start" ~ "1800") %>%
  ggplot(aes(index, value)) +
  geom_line(color = palette_light()[[1]], alpha = 0.5) +
  geom_point(color = palette_light()[[1]]) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE) +
  theme_tq() +
  labs(
    title = "1749 to 1800 (Zoomed In To Show Cycle)",
    caption = "datasets::sunspot.month"
  )

p_title <- ggdraw() + 
  draw_label("Sunspots", size = 18, fontface = "bold", colour = palette_light()[[1]])

plot_grid(p_title, p1, p2, ncol = 1, rel_heights = c(0.1, 1, 1))

# get the ACF values for data
tidy_acf <- function(data, value, lags = 0:20) {
  
  value_expr <- enquo(value)
  
  acf_values <- data %>%
    pull(value) %>%
    acf(lag.max = tail(lags, 1), plot = FALSE) %>%
    .$acf %>%
    .[,,1]
  
  ret <- tibble(acf = acf_values) %>%
    rowid_to_column(var = "lag") %>%
    mutate(lag = lag - 1) %>%
    filter(lag %in% lags)
  
  return(ret)
}

max_lag <- 12 * 50

sun_spots %>%
  tidy_acf(value, lags = 0:max_lag)
