
#' Create a sparse list model matrix 
#'
#' @param formula formula to use
#' @param data where to look up terms
#' @return list with Stan-friendly components
#' @export
flat_mm = function(formula = 1, data = NULL, ...) { 
  has_re = !is.null(lme4::findbars(lme4:::RHSForm(formula)))
  mml = list()
  if (!has_re) {
    mm = MatrixModels::model.Matrix(object = formula, 
      data = data, sparse = TRUE)
    mml = m_as_list(mm)
    mml[['matrix']] = mm
  } else {
    mm = lme4:::lFormula(formula, data = data,
      control = lme4::lmerControl(
        check.nobs.vs.nlev = "ignore",
	check.nobs.vs.nRE = "ignore"))
    mml = list()
    mml[['matrix']] = do.call(what = cbind, 
      args = c(list(mm$X), lapply(mm$reTrms$Ztlist, st)))
    mml = c(m_as_list(mml[['matrix']]), mml)
  }
  mml = c(mml, extract_response(formula, data))
  mml = c(mml, extract_groupings(formula, mm, has_re))
  mml[['names']] = colnames(mml[['matrix']])
  return(mml)
}

#' Inefficiently transpose a sparse matrix
#' 
#' @param m sparse matrix
#' @return transpose of m
st = function(m) Matrix::Matrix(t(as.matrix(m)))

#' Process fixed effects model
#'
#' @param MatrixModels::model.Matrix object
#' @return list with model matrix components and labels
m_as_list = function(m) {
  ## Calculate matrix entries
  nze = apply(m, 1, function(x) which(x != 0))
  if (!is.list(nze)) {
    if (is.null(dim(nze)))
      nze = as.list(nze)
    else
      nze = apply(nze, 2, list) %>% lapply(unlist)
  }
  N = length(nze)
  stops = sapply(nze, length) %>% cumsum
  starts = c(1, stops[1:(N-1)] + 1)
  nze = unlist(nze)
  X_vec = apply(m, 1, function(x) x[x != 0])
  m_list_form = list(
    N = N, 
    P = ncol(m),
    N_NZE = length(nze),
    NZE = nze,
    starts = starts, stops = stops,
    X_vec = as.vector(X_vec)
  )
  return(m_list_form)
}

#' Extract response name
#' 
#' @param formula formula to parse
#' @return name of LHS variable.
extract_response_name = function(formula) {
  t = terms(formula)
  pos = attr(t, 'response')
  dn = all.vars(t)[pos]
  return(dn)
}


#' Extract the formula response column form a data frame
#'
#' @param formula, a formula
#' @param data data frame to fetch from
#' @return vector of the response column
extract_response = function(formula, data) {
  dn = extract_response_name(formula)
  response_list_form = list()
  response_list_form[[dn]] = data[[dn]]
  return(response_list_form)
}

#' Basic parse to get the LHS and RHS of an lme4 random effect formula.
#' 
#' @param name term within parents in formula, e.g., (1 | a).
#' @return names of lhs and rhs terms, e.g., c(lhs = 1, rhs = "a")
parse_re_terms = function(name) {
  o = list()
  s = strsplit(name, '[ ]*\\|[ ]*')[[1]]
  o = list(lhs = strsplit(s[1], '[ ]*[+:*][ ]*'),
           rhs = strsplit(s[2], '[ ]*[+:*][ ]*'))
  return(o)
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
    col_terms = as.list(col_group)
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


