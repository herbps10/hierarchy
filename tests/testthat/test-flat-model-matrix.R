

context("Function to construct data for a flat model matrix.")

library(hierarchy); library(magrittr)

test_that("test .", {

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
  X_vec_mm = t(mm)[t(mm) != 0]
  mml = flat_mm(f, data)
  X_vec_mml = mml[['X_vec']]

  expect_equal(mml$P, ncol(mm))
  expect_length(X_vec_mml, length(X_vec_mm))
  expect_equivalent(X_vec_mm, X_vec_mml)
  expect_named(mml$groups, c("(Intercept)", "x", "P_tilde_c", "P_tilde_s"))
  expect_true(all(unlist(mml$involves) %in% names(mml$groups)))
  expect_true(
    all((cbind(mml$starts, mml$stops) %>% 
      apply(1, function(x) x[1]:x[2]) %>% 
      unlist) == 1:mml$N_NZE))
  
})
