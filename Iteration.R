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
