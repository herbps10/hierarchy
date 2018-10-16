#' A class for creating a flat sparse model matrix
#' representation
#'
#' @field .N number of observations in model matrix
#' @field .P number of columns in the model matrix
#' @field .N_NZE number of non-zero entries in the model matrix
#' @field .NZE indexes of non-zero entries in the model matrix (RSC)
#' @field .starts where each row of the model matrix starts in NZE
#' @field .stops where each row of the model matrix ends in NZE
#' @field .X_vec N_NZE NZE entries of the model matrix 
#' @field .y dependent data vector, if applicable
#' @field .groups index into X_vec for each group of parameters produced by the formula
#' @field .involves name of data columns involved in each group
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
    .involves = "list",
    .data = "data.frame"
  ),
  methods = list(
    initialize = function(formula, data, ...) {
      mml = flat_mm(formula = formula, data = data, ...)
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
      .self$.data = data
    },
    expose = function(...) {
      "Extractor that takes a named vector and provides the relevant
       component with (optionally) a new name.  The renaming syntax follows
       dplyr::rename so the new name is taken from the name of the argument and
       the element to extract is taken from the character vector content.

       For example OBJ$expose(phi_P = 'P') would return the number of (implicit)
       model matrix columns with the name 'phi_P'.  This is useful to construct
       lists that are going to be used in, e.g.-Stan."
      fields = names(methods::getRefClass("fmm")$fields())
      args_internal = unlist(list(...))
      args_new = names(args_internal)
      o = list()
      for (i in 1:length(args_internal)) {
        arg = args_internal[i]
        internal_arg = paste0(".", arg)
        if (internal_arg %in% fields) {
          if (length(args_new) != 0 && args_new[i] != "")
            o[[args_new[i]]] = .self[[internal_arg]]
          else
            o[[arg]] = .self[[internal_arg]] 
        }
      }
      return(o)
    },
    components = function() {
      fields = names(methods::getRefClass("fmm")$fields())
      return(fields)
    },
    get_data = function() return(as.list(.self$.data)),
    N = function() .self$.N,
    P = function() .self$.P,
    check_component = function(component) {
      available_components = names(.self$.involves)
      if (is.null(component)) {
        return(available_components) 
      } else {
        bad = component[!(component %in% available_components)]
        if (length(bad) != 0) {
          msg = paste0("Some components are not involved in this ",
                       "model matrix.  Extraneous components are: \n",
                       paste("    ", bad, sep = "", collapse = "\n"), 
                       "\nAvailable components are: \n",
                       paste("    ", available_components, sep = "", collapse = " \n"))
          stop(msg)
        }
        return(component)
      }
    }, 
    N_NZE = function() .self$.N_NZE,
    NZE = function() .self$.NZE,
    starts = function(component = NULL) {
      component = .self$check_component(component)
      return(.self$.starts[component])
    },
    stops = function(component = NULL) {
      component = .self$check_component(component)
      return(.self$.stops[component])
    },
    x = function(component = NULL) {
      component = .self$check_component(component)
      groups = .self$group(component)
      starts = .self$.starts
      stops = .self$.stops
      o = list()
      for (i in 1:length(component))
        o[[c]] = .self$.X_vec[starts[i]:stops[i]]
      return(o)
    },
    groups = function(component = NULL) {
      component = .self$check_component(component)
      return(.self$.groups[component])
    },
    involves = function(component = NULL) {
      component = .self$check_component(component)
      return(.self$.involves[component])
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
  mm = MatrixModels::model.Matrix(object = f, data = data, sparse = TRUE, ...)

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



