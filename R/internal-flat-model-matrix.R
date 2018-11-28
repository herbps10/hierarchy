
#' Create a sparse list model matrix 
#'
#' @param formula formula to use
#' @param data where to look up terms
#' @return list with Stan-friendly components
#' @export
flat_mm = function(formula = 1, data = NULL, ...) { 
  has_re = !is.null(lme4::findbars(lme4:::RHSForm(formula)))
  if (!has_re) {
    mm = MatrixModels::model.Matrix(object = formula, 
      data = data, sparse = TRUE, ...)
    mml = process_fixed_mm(mm, formula, data)
  } else {
    mm = lme4:::lFormula(formula, data = data,
      control = lme4::lmerControl(
        check.nobs.vs.nlev = "ignore",
	check.nobs.vs.nRE = "ignore"))
    mml = process_mixed_mm(mm, formula, data)
  }

m_as_list = function(m) {
  ## Calculate matrix entries
  nze = apply(m, 1, function(x) which(x != 0))
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
    X_vec = unlist(X_vec)
  )
  return(m_list_form
}

extract_response = function(formula, data) {
  ## Get response variable:
  t = terms(formula)
  pos = attr(t, 'response')
  dn = all.vars(t)[pos]
  response_list_form = list()
  response_list_form[[dn]] = data[[dn]]
}

parse_re_involves = function(name) {
  o = list()
  s = strsplit(name, '[ ]*\\|[ ]*')[[1]]
  o = list(lhs = strsplit(s[1], '[ ]*[+:*][ ]*'),
           rhs = strsplit(s[2], '[ ]*[+:*][ ]*'))
  return(o)
}

extract_groupings = function(terms, mm, has_re = FALSE) {
  ## Get predictor variables:
  G  = attr(terms, 'factors')
  if (has_re) {
    groupings = colnames(G)[attr(mm$X, 'assign')]
  } else {
    groupings = colnames(G)[mm@assign]
  }
  if (attr(terms, 'intercept') != 0)
    groupings = c('(Intercept)', groupings)
  group = list()
  involves = list()
  for (g in unique(groupings)) {
    group[[g]] = which(groupings == g) 
    if (g != "(Intercept)") 
      involves[[g]] = rownames(G)[which(G[,colnames(G) == g] == 1)]
  }
  if (has_re) {
    stop_idx = length(unlist(group))
    re_list = mm$reTrms$Ztlist
    for (re_name in names(re_list)) {
      start_idx = stop_idx + 1
      stop_idx = start_idx + dim(re_list[[re_name]])[1]
      group[[re_name]] = start_idx:stop_idx
      involves[[re_name]] = parse_re_involves(re_name)
      groupings = c(groupings, rep(re_name, length(group[[re_name]])))
    }
  }
  return(list(group = group, involves = involves, colnames = groupings))
}

#' Turn a RE list into flat random effects.
#'
#' Flat random effects structure 
#'
#' @param re random effects list to flatten
#' @return flat list for all these RE
#' @export
flatten_re = function(re) {
  names = sapply(re, `[[`, 'name')
  lengths = sapply(re, function(x) length(x[['col_indexes']]))
  columns = lapply(re, `[[`, 'col_indexes')
  stops = sapply(columns, length) %>% cumsum
  if (length(stops) == 1)
    starts = 1
  else
    starts = c(1, stops[1:(length(stops)-1)] + 1)
  columns = unlist(columns)
  fre = list(names = names, lengths = lengths, columns = columns,
             stops = stops, starts = starts)
  return(fre)
}

