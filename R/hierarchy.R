
#' A class for indexing a hierarchy in a model
#'
#' The class holds hierarchical relationships over all timepoints, and can
#' return indexes for any given level and all parents.
#' 
#' @field .levels names of the (child) levels in the hierarchy
#' @field .table table representing the hierarchy
#' @export hierarchy_factory
#' @exportClass hierarchy
hierarchy_factory = methods::setRefClass(Class = "hierarchy",
  fields = list(
    .levels = "character",
    .table = "data.frame"
  ),
  methods = list(
    initialize = function(files, ...) {
      "Construct hierarchy from given relationship files."
      sh = spatial_hierarchy(files)
      .self$.table = sh
      .self$.levels = region_names(sh)
    },
    get_table = function(timepoint = NULL) {
      "Method to retrieve the state of the hierarchy at a given point in time (date)"
      table = .self$.table
      if (!is.null(timepoint)) {
        filter = 
          (.self$.table[['start_date']] <= timepoint | is.na(.self$.table[['start_date']])) &
          (.self$.table[['end_date']]    > timepoint | is.na(.self$.table[['end_date']]))
        table = table[filter,]
      }
      return(table)
    },
    get_size = function(timepoint = NULL) {
      "Get the number of parameters in the _entire_ hierarchy at a given timepoint, including the zero parameter."
      table = get_table(timepoint)
      size = nrow(table)
      return(size)
    },
    get_level_idxs = function(level_name, timepoint = NULL) {
      "Get the indexes for all parameters at a given level."
      table = get_table(timepoint)
      region_level = table[['region_level']]
      idxs = which(region_level == level_name)     
      return(idxs)

    },
    get_parent_idxs = function(idxs, timepoint = NULL) {
      "Get the indexes of the parents of a given set of indexes.  This is 
       relevant if the hierarchy is not cleanly split into K levels for all
       indexes."
      table = get_table(timepoint)
      parent_id = table[['parent_id']][idxs]
      idxs = sapply(parent_id, get_id_idx, table = table, USE.NAMES=FALSE)
      return(idxs)
    },
    get_hierarchy_idxs = function(timepoint = NULL) {
      "Get a matrix with the indexes of all levels of the hierarchy, 
       with onoe entry per bottom-level node.  This format produces
       redundancy at the higher levels (a given higher-level parameter
       will appear multiple times) because that allows it to represent
       the hierarchy clearly even when not all parts have the same number
       of levels."
      table = get_table(timepoint)
      idxs = get_leaf_idxs(table)
      ml_idxs = idxs
      while (!all(idxs == get_zero_idx())) {
        idxs = get_parent_idxs(idxs)
        ml_idxs = cbind(ml_idxs, idxs)
      }
      nc = ncol(ml_idxs)
      colnames(ml_idxs) = paste("level", 1:nc, sep="-")
      rownames(ml_idxs) = NULL 
      return(ml_idxs[,1:nc])
    },
    get_vectorized_hierarchy = function(timepoint = NULL) {
      "Get a list of three vectors: 1) the matrix of all hierarchy
       indexes collapsed into a single vector; 2) the location in 
       the vector where each level start; and 3) the location (index)
       in the vector where each level ends."
      idxs = get_hierarchy_idxs(timepoint)
      idxs_l = apply(idxs, 1, list)
      idxs_l = lapply(idxs_l, unlist)
      idxs_l = lapply(idxs_l, function(x) x[x != 1])
      lengths = sapply(idxs_l, length, USE.NAMES = FALSE)
      N = length(lengths)
      start = c(1, cumsum(lengths)[1:(N-1)])
      stop = (start + lengths - 1)
      return(list(
        idx = unlist(idxs_l),
        start = start,
        end = stop
      ))
    }
  )
)
      



