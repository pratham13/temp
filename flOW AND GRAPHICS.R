mpg

library(ggplot2)

ggplot(data =mpg) +
geom_point(aes(x = drv, y = hwy, color = class)) +
  facet_grid(drv~ cyl)

# alpha for transperancey

library(dplyr)

mpg %>% filter(year != 1999) %>% arrange(hwy)


l = mpg %>% group_by(class, drv) %>%  mutate(ratio = hwy/cyl)
summarise(n = n())  %>%
  mutate(ratio = hwy/cyl)
  mutate(freq = n/sum(n))


# ggplot(diamonds) %>% geom_bar(aes(x =cut))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))

demo <- tribble(
  ~cut,         ~freq,
  "Fair",       1610,
  "Good",       4906,
  "Very Good",  12082,
  "Premium",    13791,
  "Ideal",      21551
)

ggplot(data = demo) +
  geom_bar(mapping = aes(x = cut, y = freq), stat = "identity")

# learn from here ---------------------------------------------------------



nz <- map_data("nz")

THIS_IS_A_REALLY_LONG_NAME <- 3.5

THIS_IS_A_REALLY_LONG_NAME

ROCK <- 2^3

r_rocks <- 2^3

seq(rnorm(10, mean =10))

x <- "hello world"

(y <-  seq(1,10, length.out =5))
 
ggplot(data = mpg) + 
  geom_jitter(mapping = aes(x = displ, y = hwy))

seq()

ex = data.frame(a = c(rep(NA,10),1:10), b = sample(letters, 20, replace = F))


ex %>% arrange(is.na(a))


flights %>% arrange(flight_len = dep_time - arr_time)

flights %>% select(starts_with("time"))

flights %>% rename(timing_hour = time_hour) %>% select(contains("hour"))
