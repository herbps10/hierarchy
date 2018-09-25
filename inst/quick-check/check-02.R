library(hierarchy); library(magrittr)
library(Matrix)
options(error = recover)

lvls = sapply(X = 1:1000, 
    FUN = function(x) sample(letters, 10, TRUE) %>% paste(collapse = "")
  ) %>% unique

N = 10000

data = data.frame(
  y = rnorm(N),
  x = rnorm(N),
  grouping_a = sample(lvls, N, TRUE), 
  grouping_b = sample(lvls, N, TRUE)
)

f = y ~ x + grouping_a + grouping_b
mm = model.matrix(f, data)

mml = flat_mm(f, data)

library(rstan)

m = stan(file = './normal.stan', data = mml)

