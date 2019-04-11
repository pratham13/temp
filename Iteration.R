assignSpecials <- function(x, env = caller_env()){
  x %>% 
    imap(function(.x, nm){
      if(length(.x) > 1) warn(sprintf("Only one special for `%s` is allowed, defaulting to the first usage", nm))
      .x[[1]] %>% 
        imap(function(.x, .y) assign(.y, .x, envir = env))
    })
}

assignSpecials(u)

vignette("tibble")

g = tibble(
  a = lubridate::now() + runif(1e3) * 86400,
  b = lubridate::today() + runif(1e3) * 30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters, 1e3, replace = T)
)


nycflights13::flights %>% 
  options(tibble.print_min = Inf)

names(nycflights13::flights)

df <- data.frame(abc = 1, xyz = "a")
df$x
df[, "xyz"]
df[, c("abc", "xyz")]

annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
)


x = enframe(1.5:3.5)
flatten_dbl(deframe(x))

enframe(c(a = 5, b = 7))


means <- c(0, 1, 2)


x = Sys.time()

output <- double()
for (i in seq_along(means)) {
  n <- sample(100, 1)
  output <- c(output, rnorm(n, means[[i]]))
}
str(output)
y = Sys.time()

y-x

z = Sys.time()

out <- vector("list", length(means))
for (i in seq_along(means)) {
  n <- sample(100, 1)
  out[[i]] <- rnorm(n, means[[i]])
}
str(out)
out

j = Sys.time()

j-z

unlist(out)

dplyr::bind_rows(unlist(out))


i <- 1
while (i <= length(x)) {
  # body
  i <- i + 1 
}

flip <- function() sample(c("T", "H"), 1)

flips <- 0
nheads <- 0

while (nheads < 3) {
  if (flip() == "H") {
    nheads <- nheads + 1
  } else {
    nheads <- 0
  }
  flips <- flips + 1
}
flips


flip <- function() sample(c("H","T"),1)

flips =0
nhead =0

while(nhead < 3){
  if (flip() == "H"){
    nhead = nhead +1
  } else {
    nhead = 0
  }
  flips = flips + 1
} 

flips

mapply(iris, mean)

trans <- list( 
  disp = function(x) x * 0.0163871,
  am = function(x) {
    factor(x, labels = c("auto", "manual"))
  }
)
for (var in names(trans)) {
  mtcars[[var]] <- trans[["disp"]](mtcars[["disp"]])
}


col_summary <- function(df, fun) {
  out <- vector("double", length(df))
    for (i in seq_along(df)){
      is.numeric(df[[i]])
        out[i] <- fun(df[[i]])
   }
    out
  }


col_summary(df,mean)

# using map functions of purrr

models <- mtcars %>% 
  split(.$cyl) %>% 
  map(~lm(mpg ~ wt, data = .))

models %>%
  map(summary) %>% map_dbl("adj.r.squared")


#comparison of apply and map functions

x1 <- list(
  c(0.27, 0.37, 0.57, 0.91, 0.20),
  c(0.90, 0.94, 0.66, 0.63, 0.06), 
  c(0.21, 0.18, 0.69, 0.38, 0.77)
)
x2 <- list(
  c(0.50, 0.72, 0.99, 0.38, 0.78), 
  c(0.93, 0.21, 0.65, 0.13, 0.27), 
  c(0.39, 0.01, 0.38, 0.87, 0.34)
)

threshold <- function(x, cutoff = 0.8) x[x > cutoff]

x1 %>% sapply(threshold) %>% str()
x2 %>% sapply(threshold) %>% str()

map_dbl(mtcars,mean)

nycflights13::flights  %>% map(class)

iris %>% map(unique) %>% map_int(length)

u = c(-10,0,10,100)

m = u %>% map(rnorm, n=10)

output = vector("list", length(u) )

for (i in seq_along(u)){
  output[[i]] = rnorm(n,u[i])
}

mtcars %>% map_lgl(is.factor)

map(-2:2, rnorm, n = 5)
map_dbl(-2:2, rnorm, n = 5)


map(~. lm(mpg ~ wt, data = .))


# get the safely function use to debug code with map use
safe_log <- safely(log)
 str(safe_log(10))

 x <- list(1, 10, "a")
 y <- x %>% map(safely(log))
 str(y)
 
 #get the transpose form of safely function to make the flagging of corrections
 
y = y %>% transpose() 
str(y)

is_ok = y$error %>% map_lgl(is_null)
x[!is_ok]

y$result[is_ok] %>% flatten_dbl()

# multiple arguements in map2

mu = c(1,10,5)
sigma = c(1,5,3)

map_2_f = map2(mu,sigma,rnorm,n =5)

# more than 2 arguements
n <- list(1, 3, 5)
args1 <- list(n, mu, sigma)
args1 %>%
  pmap(rnorm) %>% 
  str()

args2 <- list(mean = mu, sd = sigma, n = n)
args2 %>% 
  pmap(rnorm) %>% 
  str()

params <- tribble(
  ~mean, ~sd, ~n,
  5,     1,  1,
  10,     5,  3,
  -3,    10,  5
)
params %>% 
  pmap(rnorm)

# invoke map function

f <- c("runif", "rnorm", "rpois")
param <- list(
  list(min = -1, max = 1), 
  list(sd = 5), 
  list(lambda = 10)
)

invoke_map(f, param, n = 5) %>% str()

# using tribble to make it easy

sim <- tribble(
  ~f,      ~params,
  "runif", list(min = -1, max = 1),
  "rnorm", list(sd = 5),
  "rpois", list(lambda = 10)
)

sim %>% 
  mutate(sim = invoke_map(f, params, n = 10))

# use of walk function to store the output

library(ggplot2)
plots <- mtcars %>% 
  split(.$cyl) %>% 
  map(~ggplot(., aes(mpg, wt)) + geom_point())
paths <- stringr::str_c(names(plots), ".pdf")


pwalk(list(paths, plots), ggsave, path = tempdir())

# misc functions
 # some and every function

x <- list(1:5, letters, list(10))

x %>% 
  some(is_character)

  #detect & detect_index function
x <- sample(10)
x

x %>% 
  detect(~ . > 5)

x %>% 
  detect_index(~ . > 5)


# reduce and accumulate

dfs <- list(
  age = tibble(name = "John", age = 30),
  sex = tibble(name = c("John", "Mary"), sex = c("M", "F")),
  trt = tibble(name = "Mary", treatment = "A")
)

dfs %>% reduce(full_join)

vs <- list(
  c(1, 3, 5, 6, 10),
  c(1, 2, 3, 7, 8, 10),
  c(1, 2, 3, 4, 8, 9, 10)
)

vs %>% reduce(intersect)
