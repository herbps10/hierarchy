

context("Reloads of .csv files")

library(hierarchy)

test_that("test .csv file is loaded correctly.", {
  r_path = "test-data/spatial-groupings/countries.csv"
  test_data_path = system.file(r_path, package="hierarchy")
  test_data = load_csv(test_data_path, "test_countries_table")

  expect_is(test_data, "data.frame")
  expect_is(test_data[['region_code']], "character")
  expect_is(test_data[['admin_level']], "character")
  expect_is(test_data[['start_date']], "character")
  expect_is(test_data[['end_date']], "character")
  expect_is(test_data[['region_name']], "character")
  expect_equal(test_data[['region_code']], 
    c( "4", "8", "12", "24", "28", "31", "32", "36", "40", "44", "48", 
      "50", "51", "52", "56", "64", "68", "70", "72", "76", "84", "90", 
      "100", "104", "108", "112", "116", "120", "124", "132", "140", 
      "144", "148", "152", "156", "170", "174", "178", "180", "184", 
      "188", "191", "192", "203", "204", "208", "212", "214", "218", 
      "222", "226", "231", "232", "233", "242", "246", "250", "262", 
      "266", "268", "270", "275", "276", "288", "296", "300", "308", 
      "312", "316", "320", "324", "328", "332", "340", "344", "348", 
      "356", "360", "364", "368", "372", "376", "380", "384", "388", 
      "392", "398", "400", "404", "408", "410", "414", "417", "418", 
      "422", "426", "428", "430", "434", "440", "450", "454", "458", 
      "462", "466", "470", "474", "478", "480", "484", "496", "498", 
      "499", "500", "504", "508", "512", "516", "520", "524", "528", 
      "548", "554", "558", "562", "566", "578", "580", "584", "585", 
      "586", "591", "598", "600", "604", "608", "616", "620", "624", 
      "626", "630", "634", "638", "642", "643", "646", "659", "660", 
      "662", "670", "678", "682", "686", "688", "694", "702", "703", 
      "704", "705", "706", "710", "716", "724", "728", "729", "740", 
      "748", "752", "756", "760", "762", "764", "768", "776", "780", 
      "784", "788", "792", "795", "798", "800", "804", "807", "818", 
      "826", "834", "840", "850", "854", "858", "860", "862", "882", 
      "887", "894"))
  expect_true(all(is.na(test_data[['start_date']])))
  expect_true(all(is.na(test_data[['end_date']])))
})

test_that("innapropriate 'load_csv' usage triggers errors.", {
  r_path = "test-data/spatial-groupings"
  test_data_path = system.file(r_path, package="hierarchy")
  bad_path = file.path(test_data_path, "crow-dog.csv")
  
  expect_error(object = load_csv(test_data_path), regexp = "directory in place")
  expect_error(object = load_csv(bad_path), regexp = "File does not exist")
  
})



