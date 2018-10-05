library(hierarchy); library(magrittr)
library(Matrix)
options(error = recover)

lvls = sapply(X = 1:100, 
    FUN = function(x) sample(letters, 10, TRUE) %>% paste(collapse = "")
  ) %>% unique

N = 1000

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

