#' Load relationship file
#'
#' @param file path to .csv file describing a spatial relationship
#' @return table describing the spatial relationship
load_spatial_relationship = function(file) {
  table = load_csv(file, "spatial_relationship_file")
  ## FIXME: add schema checks here.
  return(table) 
}

#' Process spatial relationship table to a useful format
#'
#' Convert a tabular description of the spatial relationship
#' to easy-to-use objects
#'
#' @param table tabular data to convert
#' @return converted object FIXME: be more specific
#' @export
process_spatial_relationship = function(table) {
  ## FIXME: write the body
  return(sr)
}

#' Generate a list of hierarchies implied by files
#'
#' Each item describes one level of the hierarchy.
#'
#' @param files paths to .csv files describing spatial relationships
#' @return list of one-level hierarchies
#' @export
spatial_hierarchy = function(files) {
  tables = lapply(files, load_spatial_relationships)
  hierarchy = list()
  for (i in seq_along(tables)) {
    hierarchy[[i]] = process_spatial_relatinship(tables[[i]])
  }
  return(hierarchy)
}









