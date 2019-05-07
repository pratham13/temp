library(tidyverse)
install.packages("furrr", dependencies = T)
install.packages("tictoc", dependencies = T)
library(furrr)
library(tictoc)


j = "hell how are you is this Jim\'s"

x <- c("apple", "banana", "pear")
str_view(x, "an")
str_view(x, ".a")

# To create the regular expression, we need \\
dot <- "\\."

# But the expression itself only contains one:
writeLines(dot)

str_view(c("abc", "a.c", "bef"), "a\\.c")

x <- "a\\b"
writeLines(x)

str_view(x, "\\\\")

l = "a\'\\c"
writeLines(l)
str_view(l,"'")

x <- c("apple pie", "apple", "aaas^hh","apple cake", "as$^$s")
str_view(x,'')

library(tictoc)

# This should take 6 seconds in total running sequentially
plan(multiprocess)
m = tic()
nothingness <- future_map(c(10, 10, 10), ~Sys.sleep(.x))
toc()
#

x = list(runif(1e4,5,53),runif(1e4,5,53),runif(1e4,5,53),runif(1e4,5,53),
         runif(1e4,5,53),runif(1e4,5,53),runif(1e4,5,53)
        )

