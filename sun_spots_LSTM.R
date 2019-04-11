
# forecasting by various methods Analytics Vidhya -------------------------

if (!require("pacman")) install.packages("pacman")
p_load('tidyverse', 'glue', 'forcats','timetk','tidyquant', 'tibbletime', 'cowplot', 'recipes',
       'rsample', 'yardstick', ' keras', 'kerasR', 'tfruns', 'ggplot2', 'forecast', 'thief','data.table')


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

# to see if high correlation exists beyond 40 years

sun_spots %>%
  tidy_acf(value, lags = 0:max_lag) %>%
  ggplot(aes(lag, acf)) +
  geom_segment(aes(xend = lag, yend = 0), color = palette_light()[[1]]) +
  geom_vline(xintercept = 120, size = 3, color = palette_light()[[2]]) +
  annotate("text", label = "10 Year Mark", x = 130, y = 0.8, 
           color = palette_light()[[2]], size = 6, hjust = 0) +
  theme_tq() +
  labs(title = "ACF: Sunspots")

# use above 120 month mark for autocorrelation

sun_spots %>%
  tidy_acf(value, lags = 115:135) %>%
  ggplot(aes(lag, acf)) +
  geom_vline(xintercept = 120, size = 3, color = palette_light()[[2]]) +
  geom_segment(aes(xend = lag, yend = 0), color = palette_light()[[1]]) +
  geom_point(color = palette_light()[[1]], size = 2) +
  geom_label(aes(label = acf %>% round(2)), vjust = -1,
             color = palette_light()[[1]]) +
  annotate("text", label = "10 Year Mark", x = 121, y = 0.8, 
           color = palette_light()[[2]], size = 5, hjust = 0) +
  theme_tq_green(base_size =15) +
  labs(title = "ACF: Sunspots",
       subtitle = "Zoomed in on Lags 115 to 135")

# filter for optimal lag setting

optimal_lag_setting <- sun_spots %>%
  tidy_acf(value, lags = 115:135) %>%
  filter(acf == max(acf)) %>%
  pull(lag)

optimal_lag_setting

# use of rsample package for training and testing time series data
install.packages("rsample", install.dependencies =T)
library(rsample)

periods_train <- 12 * 50
periods_test  <- 12 * 10
skip_span     <- 12 * 20

rolling_origin_resamples <- rolling_origin(
  sun_spots,
  initial    = periods_train,
  assess     = periods_test,
  cumulative = FALSE,
  skip       = skip_span
)

rolling_origin_resamples

# plot the sampling for back testing
# Plotting function for a single split

plot_split <- function(split, expand_y_axis = TRUE, alpha = 1, size = 1, base_size = 14) {
  
  # Manipulate data
  train_tbl <- training(split) %>%
    add_column(key = "training") 
  
  test_tbl  <- testing(split) %>%
    add_column(key = "testing") 
  
  data_manipulated <- bind_rows(train_tbl, test_tbl) %>%
    as_tbl_time(index = index) %>%
    mutate(key = fct_relevel(key, "training", "testing"))
  
  # Collect attributes
  train_time_summary <- train_tbl %>%
    tk_index() %>%
    tk_get_timeseries_summary()
  
  test_time_summary <- test_tbl %>%
    tk_index() %>%
    tk_get_timeseries_summary()
  
  # Visualize
  g <- data_manipulated %>%
    ggplot(aes(x = index, y = value, color = key)) +
    geom_line(size = size, alpha = alpha) +
    theme_tq(base_size = base_size) +
    scale_color_tq() +
    labs(
      title    = glue("Split: {split$id}"),
      subtitle = glue("{train_time_summary$start} to {test_time_summary$end}"),
      y = "", x = ""
    ) +
    theme(legend.position = "none") 
  
  if (expand_y_axis) {
    
    sun_spots_time_summary <- sun_spots %>% 
      tk_index() %>% 
      tk_get_timeseries_summary()
    
    g <- g +
      scale_x_date(limits = c(sun_spots_time_summary$start, 
                              sun_spots_time_summary$end))
  }
  
  return(g)
}

rolling_origin_resamples$splits[[1]] %>%
  plot_split(expand_y_axis = TRUE) +
  theme(legend.position = "bottom")




