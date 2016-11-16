context("spdf_jpn")

test_that("spdf_jpn_pref", {
  expect_s4_class(spdf_jpn_pref(code = 33, district = TRUE), "SpatialPolygonsDataFrame")
#  expect_error(spdf_jpn_pref(admin_name = "\\u5ca1\\u5c71\\u770c", code = 12), info = "provide multiple arguments")
})

test_that("spdf_jpn_cities", {
  expect_s4_class(spdf_jpn_cities(jis_code_pref = 33, jis_code = 33103), "SpatialPolygonsDataFrame")
  expect_equal(nrow(spdf_jpn_cities(jis_code_pref = 33, jis_code = c(33103, 33104, 33205))), 141L)
  expect_equal(nrow(spdf_jpn_cities(jis_code_pref = 33, jis_code = 33205)), 77L)
})
