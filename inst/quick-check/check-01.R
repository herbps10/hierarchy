library(hierarchy)
options(error = recover)
fs = dir("~/packages/fpem-dp-current/spatial-relationships/", pattern = '.csv', full.names=TRUE)
sh = hierarchy:::hierarchy_factory(fs)
sh$get_size()
c_idx = sh$get_level_idxs("country")
r_idx = sh$get_level_idxs("region")
a_idx = sh$get_level_idxs("major area")
z_idx = sh$get_level_idxs("ZERO_LEVEL")

o = sh$get_hierarchy_idxs()

a_of_r_idx = sh$get_parent_idxs(r_idx)


