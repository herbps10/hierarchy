


#' Create setting for a model matrix
#'
#' Calling this function constructs the (sparse) model
#' matrices for all submodels and combines the data
#' into a single list with all necessary prefixes.  
#'
#' @param submodels, a list of formulas.  One formula per
#"        model matrix to create.
#' @param data data.frame to pull formula terms from.
#' @return list containing the model matrix for each submodel
#'         as well as plain list-form data.
#' @export
formulate = function(
  submodels = list(),
  data,
  constants = NULL
) {

  response_names = sapply(submodels, hierarchy:::extract_response_name)
  for (name in response_names) {
    if (name %in% names(data))
      next
    else
      data[[name]] = 1
  }
  names(submodels) = response_names

  mm_objs = list()
  for (name in response_names)
    mm_objs[[name]] = hierarchy:::fmm_factory(
      formula = submodels[[name]], data = data)

  # These are all the parts we need to pull out from the model matrix
  expose_components = c("n_nze", "n_col", "start", "stop", "nze",
    "n_re", "n_re_effects", "re_start", "re_stop", "re_idx")

  model_inputs = list()
  for (name in response_names) {
    names(expose_components) = paste(name, expose_components, sep='_')
    submodel_inputs = do.call(mm_objs[[name]]$expose, as.list(expose_components))
    submodel_group_columns = list("col_terms")
    if (!is.null(constants)) {
      constants = unlist(mm_objs[[name]]$expose("group_columns")[['group_columns']][constants])
      submodel_inputs[[paste(name, 'n_constants')]] = length(constants)
      submodel_inputs[[paste(name, 'constants', sep='_')]] = constants
      submodel_inputs[[paste(name, 'constant_values', sep='_')]] = data[[name]]
    }
    model_inputs[[name]] =  submodel_inputs
  }

  return(list(mm = mm_objs, data = model_inputs))
}



