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
