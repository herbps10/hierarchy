

flat_mm = function(formula = 1, data = NULL, ...) { 
  mm = Matrix::sparse.model.matrix(f, data, ...)
  nze = apply(mm, 1, function(x) which(x != 0))
  N = length(nze)
  stops = sapply(nze, length) %>% cumsum
  starts = c(1, stops[1:(N-1)] + 1)
  nze = unlist(nze)
  X_vec = apply(mm, 1, function(x) x[x != 0])
  mml = list(
    N = N, 
    P = ncol(mm),
    N_NZE = length(nze),
    NZE = nze,
    starts = starts, stops = stops,
    X_vec = unlist(X_vec)
  )
  t = terms(formula)
  pos = attr(t, 'response')
  dn = all.vars(t)[pos]
  mml[[dn]] = data[[dn]]
  return(mml)
}




