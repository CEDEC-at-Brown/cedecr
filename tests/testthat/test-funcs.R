library(testthat)
test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("calculate_distance works correctly", {
  expect_equal(round(calculate_distance(37.77, -122.94, 34.05, -118.37)), 363)
  expect_equal(round(calculate_distance(40.48, -74.0060, 45.22, -118.24)), 2242)
  expect_equal(calculate_distance(0, 0, 0, 0), 0)
})


test_that("get_census_code_vec works correctly", {
  #Swearer Center Coordinates
  #State code
  expect_equal(substr(get_census_code_vec(41.82977778935678, -71.39798786793251),1,2), '44')
  #Tract code
  expect_equal(substr(get_census_code_vec(41.82977778935678, -71.39798786793251),6,11), '003601')
})


test_that("get_ward works correctly", {
  #Swearer Center Coordinates
  #Swearer Center
  expect_equal(get_ward_from_coordinates(41.82977778935678, -71.39798786793251), '2')
  #Tallulah's
  expect_equal(get_ward_from_coordinates(41.82224482910856, -71.39224459072776), '1')
  #Swearer Center
  expect_equal(get_ward_from_address('2 Stimson Avenue, Providence, RI 02906'),'2')
  #Tallulah's
  expect_equal(get_ward_from_address('146 Ives St, Providence, RI 02906'),'1')
  #Federal Hill
  expect_equal(get_ward_from_address('318 Broadway, Providence, RI 02909'),'13')
})

test_that("get_school_district works correctly", {
  #Hope High School
  expect_equal(get_school_district_from_coordinates(41.8356558183232, -71.40204340542262), 'Providence School District')
  #Central Falls High
  expect_equal(get_school_district_from_coordinates(41.887231153672396, -71.3914115349033), 'Central Falls School District')
  #Hope High School
  expect_equal(get_school_district_from_address('324 Hope St, Providence, RI 02906'),'Providence School District')
  #Central Falls
  expect_equal(get_school_district_from_address('24 Summer St, Central Falls, RI 02863'),'Central Falls School District')
  #Barrington
  expect_equal(get_school_district_from_address('220 Lincoln Ave, Barrington, RI 02806'),'Barrington School District')
})


test_that("get_neighborhood works correctly", {
  #Swearer Center
  expect_equal(get_neighborhood_from_coordinates(41.82977778935678, -71.39798786793251), 'College Hill')
  #Tallulah's
  expect_equal(get_neighborhood_from_coordinates(41.82224482910856, -71.39224459072776), 'Fox Point')
  #Swearer Center
  expect_equal(get_neighborhood_from_address('2 Stimson Avenue, Providence, RI 02906'),'College Hill')
  #Tallulah's
  expect_equal(get_neighborhood_from_address('146 Ives St, Providence, RI 02906'),'Fox Point')
  #Federal Hill Restaurant
  expect_equal(get_neighborhood_from_address('441 Atwells Ave, Providence, RI 02909'),'Federal Hill')
})
