
# EDA and distribution ----------------------------------------------------
library(ggplot2)
library(dplyr)
library(gganimate)

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

diamonds %>%  count(cut)

# distribution of a continuous variable

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

ggplot(diamonds, aes(carat, price,size = clarity, color = color, frame = depth )) +
  geom_point()


diamonds %>% 
  count(cut_width(carat, 0.5))

# for carat < 3 and binwidth  = 0.1

smaller <- diamonds %>% 
  filter(carat < 3)

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.1)

# to get the freq_plot

ggplot(data = smaller, mapping = aes(x = carat, colour = cut)) +
  geom_freqpoly(binwidth = 0.1)

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

ggplot(data = faithful, mapping = aes(x = eruptions)) + 
  geom_histogram(binwidth = 0.25)

# distibution for each diamond

ggplot(diamonds, aes(x =z)) +  geom_histogram(binwidth = 0.5) +
  ylim(c(0,10))
  
  coord_cartesian(ylim = c(0,20))

diamonds %>% filter( z >30) %>%
  select(price,x,y,z) %>% arrange(z)

ggplot(diamonds, aes(x = price)) + geom_histogram(binwidth =100)


# count of diamonds with carat  = 0.99 & carat =1

diamonds %>% filter(carat ==1 | carat ==0.99) %>% group_by(carat) %>% summarise(n =n())

diamonds %>% filter(between(price,1000,15000)) %>%
  group_by(price) %>% summarise(n =n())
  
  mutate(ex_col = case_when(price %% 100 == 57 ~ "gandiv",
                            price %/% 100 == 27 ~"donshe"
                                       ))

# study of covariance
  ggplot(data = diamonds, mapping = aes(x = price)) + 
    geom_freqpoly(mapping = aes(colour = cut), binwidth = 500) + facet_wrap(~price)
 
  
