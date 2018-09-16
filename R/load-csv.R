
#' Load a .csv file in a round-trip-friendly way
#' 
#' @param file path to .csv file with read access.
#' @param table_name 
#' @return tibble of file contents.
#' @export
load_csv = function(file, .table_name = NULL) {
  if (is.null(.table_name))
    checkmate::assertFileExists(file, "r", "csv")
  else
    checkmate::assertFileExists(file, "r", "csv", .table_name)
  file = normalizePath(file)
  table = read.csv(file, stringsAsFactors=FALSE, colClasses = "character")
  return(table)
}



