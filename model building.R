library(tidyverse)
library(modelr)
options(na.action = na.warn)

library(nycflights13)
library(lubridate)

# low cut diamonds have a higher price
ggplot(diamonds, aes(cut, price)) + geom_boxplot()
ggplot(diamonds, aes(color, price)) + geom_boxplot()
ggplot(diamonds, aes(clarity, price)) + geom_boxplot()

ggplot(diamonds, aes(carat, price)) + 
  geom_hex(bins = 50)

# filter on carat and take the log value for price and carat
diamonds2 <- diamonds %>% 
  filter(carat <= 2.5) %>% 
  mutate(lprice = log2(price), lcarat = log2(carat))

ggplot(diamonds2, aes(lcarat, lprice)) + 
  geom_hex(bins = 50)

#build the model on the transformed dataset
mod_diamond <- lm(lprice ~ lcarat, data = diamonds2)

grid <- diamonds2 %>% 
  data_grid(carat = seq_range(carat, 20)) %>% 
  mutate(lcarat = log2(carat)) %>% 
  add_predictions(mod_diamond, "lprice") %>% 
  mutate(price = 2 ^ lprice)

ggplot(diamonds2, aes(carat, price)) + 
  geom_hex(bins = 50) + 
  geom_line(data = grid, colour = "red", size = 1)


library(modelr)
library(gapminder)

nz <- filter(gapminder, country == "New Zealand")
nz %>% 
  ggplot(aes(year, lifeExp)) + 
  geom_line() + 
  ggtitle("Full data = ")

nz_mod <- lm(lifeExp ~ year, data = nz)
nz %>% 
  add_predictions(nz_mod) %>%
  ggplot(aes(year, pred)) + 
  geom_line() + 
  ggtitle("Linear trend + ")

nz %>% 
  add_residuals(nz_mod) %>% 
  ggplot(aes(year, resid)) + 
  geom_hline(yintercept = 0, colour = "white", size = 3) + 
  geom_line() + 
  ggtitle("Remaining pattern")

by_country <- gapminder %>% 
  group_by(country, continent) %>% 
  nest()

by_country

country_model <- function(df) {
  lm(lifeExp ~ year, data = df)
}

#mutate model & residual columns

by_country <- by_country %>% 
  mutate(model = map(data, country_model))
by_country

by_country <- by_country %>% 
  mutate(
    resids = map2(data, model, add_residuals)
  )
by_country

# unnest residuals
resids <- unnest(by_country, resids)
resids

resids %>% 
  ggplot(aes(year, resid)) +
  geom_line(aes(group = country), alpha = 1 / 3) + 
  geom_smooth(se = FALSE)

broom::glance(nz_mod)
