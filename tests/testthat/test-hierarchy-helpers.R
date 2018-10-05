

context("Helper functions for hierarhical structures.")

library(hierarchy)

test_that("test .csv file is loaded correctly.", {
  r_path = "test-data/spatial-groupings"
  test_data_path = system.file(r_path, package="hierarchy")
  good_path = file.path(test_data_path, "country-in-region.csv")
  bad_path = file.path(test_data_path, "countries.csv")
  sr = load_spatial_relationship(good_path)
  classes = sapply(sr, class)

  expect_error(object = load_spatial_relationship(bad_path), 
    regexp = "Must have exactly 6 cols, but has 5")
  expect_is(sr, "data.frame")
  expect_is(sr[['region_code']], "character")
  expect_is(sr[['region_level']], "character")
  expect_is(sr[['parent_region_code']], "character")
  expect_is(sr[['parent_region_level']], "character")
  expect_is(sr[['start_date']], "Date")
  expect_is(sr[['end_date']], "Date")
  expect_is(sr[['id']], "character")
  expect_is(sr[['parent_id']], "character")

})

test_that("hierarchy can be loaded", {
  r_path = "test-data/spatial-groupings"
  test_data_path = system.file(r_path, package="hierarchy")
  good_paths = dir(path = test_data_path, 
    pattern = ".*-in-.*\\.csv", full.names = TRUE)
  h_table = spatial_hierarchy(good_paths)
  zr = hierarchy:::get_zero_idx()
  expect_equal(h_table[['region_code']][zr], "ZERO")
  expect_equal(h_table[['region_level']][zr], "ZERO_LEVEL")
  expect_true("ZERO_LEVEL" %in% h_table[['parent_region_level']])
})

test_that("indexes are retrieved correctly", {
  r_path = "test-data/spatial-groupings"
  test_data_path = system.file(r_path, package="hierarchy")
  good_paths = dir(path = test_data_path, 
    pattern = ".*-in-.*\\.csv", full.names = TRUE)
  h_table = spatial_hierarchy(good_paths)
  expect_equal(
    h_table[hierarchy:::get_leaf_idxs(h_table), 'region_level'],
    rep("country", 233)
  )
  expect_equal(
    hierarchy:::get_id_idx(paste(920, "region"), h_table),
    250
  )
})




