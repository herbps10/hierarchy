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
    .data = "data.frame",
    .re = "list",
    .names = "character"
  ),
  methods = list(
    initialize = function(formula, data, ...) {
      "Create the implicit mass matrix and store components."
      mml = flat_mm(formula = formula, data = data, ...)
      .self$.N = mml$N
      .self$.P = mml$P
      .self$.N_NZE = mml$N_NZE
      .self$.NZE = mml$NZE
      .self$.starts = mml$starts
      .self$.stops = mml$stops
      .self$.X_vec = mml$X_vec
      t = terms(formula)
      pos = attr(t, 'response')
      dn = all.vars(t)[pos]
      .self$.y = mml[[dn]]
      .self$.groups = mml$groups
      .self$.involves = mml$involves
      .self$.data = data
      .self$.re = list()
      .self$.names = mml[['names']]
    },
    expose_matrix = function(...) {
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
    expose_re = function(re = NULL, flat = FALSE) {
      "Return a list (or flattened list) representing the 
       random effects specification."
      if (is.null(re))
        return(names(.self$.re))
      else {
        if (!flat) {
          return(.self$.re[re])
        } else {
          return(flatten_re(.self$.re[re]))
        }
      }
    },
    components = function() {
      "Get a list of the object's fields that can be exposed."
      fields = names(methods::getRefClass("fmm")$fields())
      return(fields)
    },
    get_data = function() {
      "Get the data frame used to construct the matrix."
      return(as.list(.self$.data))
    },
    N = function() .self$.N,
    P = function() .self$.P,
    check_component = function(component) {
      "Verify that the requested (formula) component is 
       in the model matrix and return its name.  If none
       is specified (NULL in calling function) then all 
       are returned."
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
    },
    re = function(component = NULL, clear = FALSE) {
      if (clear) {
        .self$.re = list()
        return(NULL)
      }

      component = .self$check_component(component)
      cols = unlist(.self$.groups[component])
      cn = .self$.names[cols]
      name = paste(component, collapse = "__")
      .self$.re[[name]] = list(
        name = name,
        components = component,
        col_names = cn,
        col_indexes = cols
      )
      return(.self$.re[name])
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
  re_terms = lme4::findbars(lme4:::RHSForm(formula))
  if (is.null(re_terms)) {
    mm = MatrixModels::model.Matrix(object = formula, 
      data = data, sparse = TRUE, ...)
  } else {
    mm_obj = lme4:::lFormula(formula, data = data,
      control = lme4::lmerControl(
        check.nobs.vs.nlev = "ignore",
	check.nobs.vs.nRE = "ignore"))
    mm = cbind(mm_obj$X, mm_obj$reTrms$Zt)
  }

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
    if (g != "(Intercept)") 
      involves[[g]] = rownames(G)[which(G[,colnames(G) == g] == 1)]
  }
  mml[['groups']] = group
  mml[['involves']] = involves

  ## Names
  mml[['names']] = colnames(mm)

  return(mml)
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

