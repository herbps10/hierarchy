#' A class for creating a flat sparse model matrix
#' representation
#'
#' @field .n_row number of observations in model matrix
#' @field .n_col number of columns in the model matrix
#' @field .n_nze number of non-zero entries in the model matrix
#' @field .nze indexes of non-zero entries in the model matrix (RSC)
#' @field .start where each row of the model matrix start in nze
#' @field .stop where each row of the model matrix ends in nze
#' @field .X_vec n_nze nze entries of the model matrix 
#' @field .y dependent data vector, if applicable
#' @field .groups index into X_vec for each group of parameters produced by the formula
#' @export fmm_factory
#' @exportClass fmm
fmm_factory = methods::setRefClass(Class = "fmm",
  fields = list(
    .matrix = "list",
    .n_row = "numeric",
    .n_col = "numeric",
    .n_nze = "numeric",
    .nze = "numeric",
    .start = "numeric",
    .stop = "numeric",
    .X_vec = "numeric",
    .y_name = "character",
    .y = "numeric",
    .group_columns = "list",
    .group_terms = "list",
    .col_group = "character",
    .col_terms = "list",
    .col_names = "character",
    .group_lengths = "list",
    .re_names = "character",
    .n_re = "numeric",
    .n_re_effects = "numeric",
    .re_start = "numeric",
    .re_stop = "numeric",
    .re_idx = "numeric",
    .data = "data.frame"
  ),
  methods = list(
    initialize = function(formula, data, ...) {
      "Create the implicit mass matrix and store components."

      simple_formula = simplify(formula)
      std_formula = distribute(simple_formula)
      terms = subterms(std_formula)
      tl = term_list(terms)
      components = imbue(tl[['rhs']], data)
      submatrices = expand(components)
      mm = combine(submatrices)

      .self$.matrix[['mm']] = list(mml$matrix)
      .self$.n_row = mml$n_row
      .self$.n_col = mml$n_col
      .self$.n_nze = mml$n_nze
      .self$.nze = mml$nze
      .self$.start = mml$start
      .self$.stop = mml$stop
      .self$.X_vec = mml$X_vec
      t = terms(formula)
      pos = attr(t, 'response')
      dn = all.vars(t)[pos]
      .self$.y_name = dn
      .self$.y = mml[[dn]]
      .self$.group_columns = mml$group_columns
      .self$.group_terms = mml$group_terms
      .self$.col_group = mml$col_group
      .self$.col_terms = mml$col_terms
      .self$.col_names = mml$names
      .self$.group_lengths = mml$group_lengths
      .self$.re_names = mml$re_name
      .self$.n_re = mml$n_re
      .self$.n_re_effects = mml$n_re_effects
      .self$.re_start = mml$re_start
      .self$.re_stop = mml$re_stop
      .self$.re_idx = mml$re_idx
      .self$.data = data
    },
    expose = function(...) {
      "Extractor that takes a named vector and provides the relevant
       component with (optionally) a new name.  The renaming syntax follows
       dplyr::rename so the new name is taken from the name of the argument and
       the element to extract is taken from the character vector content.

       For example OBJ$expose(phi_n_col = 'n_col') would return the number of (implicit)
       model matrix columns with the name 'phi_n_col'.  This is useful to construct
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
    list_components = function() {
      "Get a list of the object's fields that can be exposed."
      fields = names(methods::getRefClass("fmm")$fields())
      return(fields)
    },
    list_terms = function() {
      return(names(.sefl$.group_terms))
    },
    get_data = function() {
      "Get the data frame used to construct the matrix."
      return(as.list(.self$.data))
    },
    n_row = function() .self$.n_row,
    n_col = function() .self$.n_col,
    check_component = function(component) {
      "Verify that the requested (formula) component is 
       in the model matrix and return its name.  If none
       is specified (NULL in calling function) then all 
       are returned."
      available_components = .self$list_terms()
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
    n_nze = function() .self$.n_nze,
    nze = function() .self$.nze,
    start = function(component = NULL) {
      component = .self$check_component(component)
      return(.self$.start[component])
    },
    stop = function(component = NULL) {
      component = .self$check_component(component)
      return(.self$.stop[component])
    },
    x = function(component = NULL) {
      component = .self$check_component(component)
      groups = .self$group(component)
      start = .self$.start
      stop = .self$.stop
      o = list()
      for (i in 1:length(component))
        o[[c]] = .self$.X_vec[start[i]:stop[i]]
      return(o)
    },
    groups = function(component = NULL) {
      component = .self$check_component(component)
      return(.self$.groups[component])
    }
  )
)
