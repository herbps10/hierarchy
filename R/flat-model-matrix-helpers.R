

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
  stop = cumsum(stop)
  start = c(1, stop[1:(N-1)] + 1)
  nze = unlist(nze)
  X_vec = apply(m, 1, function(x) x[x != 0])
  if (is.matrix(X_vec))
    X_vec = as.vector(X_vec)
  m_list_form = list(
    n_row = N, 
    n_col = ncol(m),
    n_nze = length(nze),
    nze = nze,
    start = start, stop = stop,
    X_vec = unlist(X_vec)
  )
  return(m_list_form)
}



#' Extract column indexes and names from a formula
#' 
#' @param formula formula used to create the model matrix
#' @param mm model matrix (either from lme4:::lFormula or 
#'        MatrixModels::model.Matrix
#' @param has_re whether the model includes random effects
#' @return list with the column indexes for each term, the
#'         variables involved in each term, and a vector 
#'         with on (term) name for each column.  This does
#'         not (yet) extract the terms on the LHS of the
#'         random effects formulas separately.
extract_groupings = function(formula, mm, has_re = FALSE) {
  ## Get predictor variables:
  terms = terms(formula)
  G  = attr(terms, 'factors')
  if (has_re) {
    col_group = colnames(G)[attr(mm$X, 'assign')]
  } else {
    col_group = colnames(G)[mm@assign]
  }
  if (attr(terms, 'intercept') != 0)
    col_group = c('(Intercept)', col_group)
  col_terms = as.list(col_group)
  group = list()
  group_terms = list()
  for (g in unique(col_group)) {
    group[[g]] = which(col_group == g) 
    if (g != "(Intercept)") 
      group_terms[[g]] = rownames(G)[which(G[,colnames(G) == g] == 1)]
  }
  n_re = 0
  n_re_effects = 0
  re_names = vector(mode = 'character', length = 0)
  re_start = vector(mode = 'numeric', length = 0)
  re_stop = vector(mode = 'numeric', length = 0)
  re_idx = vector(mode = 'numeric', length = 0)
  if (has_re) {
    stop_idx = length(unlist(group))
    re_list = mm$reTrms$Ztlist
    ne_re = length(re_list)
    re_names = names(re_list)
    for (i in seq_along(re_names)) {
      n_re = n_re + 1
      re_name = re_names[i]
      start_idx = stop_idx + 1
      stop_idx = start_idx + dim(re_list[[re_name]])[1] - 1
      group[[re_name]] = start_idx:stop_idx
      group_terms[[re_name]] = parse_re_terms(re_name)
      col_group = c(col_group, rep(re_name, length(group[[re_name]])))
      col_terms = c(col_terms, rep(list(parse_re_terms(re_name)), length(group[[re_name]])))
      n_re_effects = n_re_effects + stop_idx - start_idx + 1
      re_start = c(re_start, start_idx)
      re_stop = c(re_stop, stop_idx)
      re_idx = c(re_idx, rep(i, stop_idx - start_idx + 1))
    }
    lengths = c(lengths, stop_idx - start_idx + 1)
  }
  return(list(group_columns = group, 
	      group_terms = group_terms,
	      col_group = col_group,
	      col_terms = col_terms,
	      group_lengths = lapply(group, length),
	      re_names = re_names, 
	      n_re = n_re,
              n_re_effects = n_re_effects,
	      re_start = re_start,
	      re_stop = re_stop,
	      re_idx = re_idx
  ))
}


