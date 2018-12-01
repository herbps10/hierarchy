library(hierarchy); library(magrittr)
options(error = recover)

s_grid = sapply(X = 1:400, 
    FUN = function(x) sample(0:9, 10, TRUE) %>% paste(collapse = "")
  ) %>% unique

counties = sapply(X = 1:47, 
    FUN = function(x) sample(letters, 10, TRUE) %>% paste(collapse = "")
  ) %>% unique

N = 2000

data = data.frame(
  y = rnorm(N),
  x = rnorm(N),
  P_tilde_c = sample(counties, N, TRUE), 
  P_tilde_s = sample(s_grid, N, TRUE)
)

f = y ~ x + (1 | P_tilde_c) + (1 | P_tilde_s)

mobj = hierarchy:::fmm_factory(f, data)



