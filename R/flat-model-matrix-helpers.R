

#' Inefficiently transpose a sparse matrix
#' 
#' @param m sparse matrix
#' @return transpose of m
st = function(m) Matrix::Matrix(t(as.matrix(m)))

#' Process fixed effects model
#'
#' @param MatrixModels::model.Matrix object
#' @return list with model matrix components and labels
#' @export
m_as_list = function(m) {
  ## Calculate matrix entries
  nze = apply(m, 1, function(x) which(x != 0))
  if (!is.list(nze)) {
    if (is.null(dim(nze)))
      nze = as.list(nze)
    else
      nze = apply(nze, 2, list) 
      nze = lapply(nze, unlist)
  }
  N = length(nze)
  stop = sapply(nze, length)
  skips = which(stop == 0)
  stop = cumsum(stop)
  start = 1
  if (length(stop) > 1)
    start = c(start, stop[1:(N-1)] + 1)
  stop[skips] = 0
  start[skips] = 0
  nze = unlist(nze)
  xv = apply(m, 1, function(x) x[x != 0])
  if (is.matrix(xv))
    xv = as.vector(xv)
  m_list_form = list(
    n_row = N, 
    n_col = ncol(m),
    n_nze = length(nze),
    nze = nze,
    skips = skips,
    start = start, 
    stop = stop,
    xv = unlist(xv)
  )
  return(m_list_form)
}





