#' A class for creating a flat sparse model matrix
#' representation
#'
#' @field .N
#' @field .P
#' @field .N_NZE
#' @field .NZE
#' @field .starts
#' @field .stops
#' @field .X_vec
#' @field .y
#' @field .groups
#' @field .involves
#' @export fmm_factory
#' @exportClass fmm
fmm_factory = methods::setRefClass(Class = "fmm",
  fields = list(
    .N = "numeric",
    .P = "numeric",
    .N_NZE = "numeric",
    .NZE = "numeric",
    .starts = "numeric",
    .stops = "numeric",
    .X_vec = "numeric",
    .y = "numeric",
    .groups = "list",
    .involves = "list"
  ),
  methods = list(
    initialize = function(formula, data, ...) {
      mml = flat_model_matrix(formula, data, ...)
      .self$.N = mml$N
      .self$.P = mml$P
      .self$.N_NZE = mml$N_NZE
      .self$.NZE = mml$NZE
      .self$.starts = mml$starts
      .self$.stops = mml$stops
      .self$.X_vec = mml$X_vec
      .self$.y = mml$y
      .self$.groups = mml$groups
      .self$.involves = mml$involves
    }
  )
)

#' Create a sparse list model matrix 
#'
#' @param formula formula to use
#' @param data where to look up terms
#' @return list with Stan-friendly components
#' @export
flat_mm = function(formula = 1, data = NULL, ...) { 
  mm = MatrixModels::model.Matrix(f, data, sparse = TRUE, ...)

  ## Calculate matrix entries
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

  ## Get response variable:
  t = terms(formula)
  pos = attr(t, 'response')
  dn = all.vars(t)[pos]
  mml[[dn]] = data[[dn]]

  ## Get predictor variables:
  G  = attr(t, 'factors')
  groupings = colnames(G)[mm@assign]
  if (attr(t, 'intercept') != 0)
    groupings = c('(Intercept)', groupings)
  group = list()
  involves = list()
  for (g in unique(groupings)) {
    group[[g]] = which(groupings == g) 
    involves[[g]] = rownames(G)[which(G[,colnames(G) == g] == 1)]
  }
  mml[['groups']] = group
  mml[['involves']] = involves
  return(mml)
}



