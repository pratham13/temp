
# Data Transformation -----------------------------------------------------

library(tidyverse)
library(nycflights13)

flights

select(flights, arr_time, carrier, everything())

rename(flights, yr = year)

flights %>%
select( arr_time, arr_time)

vars <- c("year", "month", "day", "dep_delay", "arr_delay")
one_of(vars)


flights %>%
  select(one_of(c("day","year", "cyl")))

select(flights, contains("TIME"))

flights %>%
  select(contains("TIME"))


# use of pipe -------------------------------------------------------------

library(ggplot2)

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay)
  )

ggplot(data = delays, mapping = aes(x = delay)) + geom_freqpoly(aes(color = "delays"), binwidth =5)
  

delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarise(
    delay =mean(arr_delay, na.rm =T),
    n = n()
  )

ggplot(delays, aes(x = n, y = delay)) + geom_point(alpha = 1/10)

