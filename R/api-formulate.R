


#' Create setting for a model matrix
#'
#' Calling this function constructs the (sparse) model
#' matrices for all models and combines the data
#' into a single list with all necessary prefixes.  
#'
#' @param models, a list of formulas.  One formula per
#"        model matrix to create.
#' @param data data.frame to pull formula terms from.
#' @return list containing 1) the list-format model
#'         matrix for each formula; 2) the data used 
#'         to evaluate terms for the matrix model; 
#'         and 3) the sparse-format model matrix in a
#'         container object.
#'         
#' @export
formulate = function(
  models = list(),
  data
) {

  response_names = as.character(sapply(models, hierarchy:::response_name))
  for (name in response_names) {
    if (name %in% names(data))
      next
    else
      data[[name]] = 1
  }
  names(models) = response_names

  mm_objs = list()
  for (name in response_names) {
    mm_objs[[name]] = hierarchy:::fmm_factory(
      formula = models[[name]], data = data)
  }

  # These are all the parts we need to pull out from the model matrix
  expose_components = c("n_nze", "n_col", "start", "stop", "nze", "skips",
    "state_terms", "n_state_terms",
    "constant_terms", "n_constant_terms",
    "coefficient_terms", "n_coefficient_terms",
    "n_re", "re_start", "re_stop", "xv")

  model_inputs = list()
  for (name in response_names) {
    names(expose_components) = paste(name, expose_components, sep='_')
    submodel_inputs = do.call(mm_objs[[name]]$expose, as.list(expose_components))
    model_inputs[[name]] =  submodel_inputs
  }

  return(list(inputs = model_inputs, data = data, matrices = mm_objs))
}



