

context("Reference class for hierarhical structures.")

library(hierarchy)

test_that("test the class is loaded correctly.", {
  r_path = "test-data/spatial-groupings"
  test_data_path = system.file(r_path, package="hierarchy")
  good_paths = dir(path = test_data_path, 
    pattern = ".*-in-.*\\.csv", full.names = TRUE)
  sh = hierarchy:::hierarchy_factory(good_paths)

  expect_equal(sh$get_size(), 261)
  
  c_idx = sh$get_level_idxs("country")
  for (c in c_idx)
    expect_equal(sh$.table[c, 'region_level'], 'country')

  r_idx = sh$get_level_idxs("region")
  for (r in r_idx) 
    expect_equal(sh$.table[r, 'region_level'], 'region')

  a_idx = sh$get_level_idxs("major area")
  for (a in a_idx)
    expect_equal(sh$.table[a, 'region_level'], 'major area')

  z_idx = sh$get_level_idxs("ZERO_LEVEL")
  expect_equal(length(z_idx), 1)

  o = sh$get_hierarchy_idxs()
  colnames(o) <- NULL
  expect_equal(ncol(o), 4)
  for (r in 1:nrow(o))
    expect_equal(o[r,4], hierarchy:::get_zero_idx())
  expect_equal(
    length(unique(o[o != hierarchy:::get_zero_idx()])),
    sh$get_size() - 1)
  
})
