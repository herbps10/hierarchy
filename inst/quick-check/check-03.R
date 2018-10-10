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

f = y ~ x + P_tilde_c + P_tilde_s
mm = model.matrix(f, data)

mml = flat_mm(f, data)

library(rstan)

ms = stan(file = './normal-sparse.stan', data = mml, init = 0)

md = stan(file = './normal-dense.stan', data = list(
  N = mml$N, P = mml$P, y = mml$y, X = mm), init = 0)						
s_betas = rstan::extract(ms, pars = 'betas')[['betas']] %>% apply(2, mean)
d_betas = rstan::extract(md, pars = 'betas')[['betas']] %>% apply(2, mean)
plot(s_betas, d_betas)
hist(s_betas - d_betas)


system.time({ms = stan(file = './normal-sparse.stan', data = mml, init = 0)
})
system.time({md = stan(file = './normal-dense.stan', data = list(
  N = mml$N, P = mml$P, y = mml$y, X = mm), init = 0)                                     
 
})

