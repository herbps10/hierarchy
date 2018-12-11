


#' Create setting for a model matrix
#'
#' Calling this function constructs the (sparse) model
#' matrices for all models and combines the data
#' into a single list with all necessary prefixes.  
#'
#' @param models, a list of formulas.  One formula per
#"        model matrix to create.
#' @param data data.frame to pull formula terms from.
#' @return list containing the model matrix for each submodel
#'         as well as plain list-form data.
#' @export
formulate = function(
  models = list(),
  data,
  constants = NULL,
  states = NULL,
  drop = NULL
) {

  response_names = sapply(models, hierarchy:::extract_response_name)
  for (name in response_names) {
    if (name %in% names(data))
      next
    else
      data[[name]] = 1
  }
  names(models) = response_names

  mm_objs = list()
  for (name in response_names) {
    if (!is.null(states) && name %in% names(states)) {
      for (sname in states[[name]]) {
        if (!(sname %in% names(data)))
	  data[[sname]] = 1
	else
	  stop("States must not also be data.")
      }
    }
    if (!is.null(constants) && name %in% names(constants)) {
      for (cname in constants[[name]]) {
	if (!(cname %in% names(data)))
          data[[cname]] = 1
      }
    }
    mm_objs[[name]] = hierarchy:::fmm_factory(
      formula = models[[name]], data = data, 
      drop = drop)
  }

  # These are all the parts we need to pull out from the model matrix
  expose_components = c("n_nze", "n_col", "start", "stop", "nze",
    "n_re", "n_re_effects", "re_start", "re_stop", "re_idx")

  model_inputs = list()
  for (name in response_names) {
    names(expose_components) = paste(name, expose_components, sep='_')
    submodel_inputs = do.call(mm_objs[[name]]$expose, as.list(expose_components))
    submodel_group_columns = list("col_terms")
    if (!is.null(constants)) {
      cnames = constants[[name]]
      ccol = unlist(mm_objs[[name]]$expose("group_columns")[['group_columns']][cnames])
      submodel_inputs[[paste(name, 'n_constants')]] = length(ccol)
      submodel_inputs[[paste(name, 'constants', sep='_')]] = ccol
      submodel_inputs[[paste(name, 'constant_values', sep='_')]] = data[[cnames]]
    }
    model_inputs[[name]] =  submodel_inputs
  }

  return(list(inputs = model_inputs, data = data, matrices = mm_objs))
}



