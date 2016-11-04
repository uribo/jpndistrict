context("spdf_jpn")

test_that("spdf_jpn_pref", {

  df.pref33.name <- spdf_jpn_pref(admin_name = "\u5ca1\u5c71\u770c", district = FALSE)
  df.city33.name <- spdf_jpn_pref(admin_name = "\u5ca1\u5c71\u770c", district = TRUE)
  expect_s4_class(spdf_jpn_pref(admin_name = "\u5ca1\u5c71\u770c", district = FALSE), "SpatialPolygonsDataFrame")
  df.pref33.code <- spdf_jpn_pref(code = 33, district = FALSE)
  expect_error(spdf_jpn_pref(admin_name = "\u5ca1\u5c71\u770c", code = 12), info = "provide multiple arguments")
  expect_equal(df.pref33.code, df.pref33.name)
})

test_that("spdf_jpn_city", {

  expect_s4_class(spdf_jpn_city(spdf_jpn_pref(admin_name = "\u5ca1\u5c71\u770c", district = TRUE),
                                admin_name = "\u7b20\u5ca1\u5e02"), "SpatialPolygonsDataFrame")
  expect_equal(nrow(spdf_jpn_city(spdf_jpn_pref(admin_name = "\u5ca1\u5c71\u770c", district = TRUE), 33103)), 52L)
  expect_equal(nrow(spdf_jpn_city(spdf_jpn_pref(admin_name = "\u5ca1\u5c71\u770c", district = TRUE), admin_name = "\u5009\u6577\u5e02")), 84L)

})

test_that("spdf_jpn_cities", {
  expect_s4_class(spdf_jpn_cities(jis_code_pref = 33, jis_code = 33103), "SpatialPolygonsDataFrame")
  expect_equal(nrow(spdf_jpn_cities(jis_code_pref = 33, jis_code = c(33103, 33104, 33205))), 141L)
  expect_equal(nrow(spdf_jpn_cities(jis_code_pref = 33, admin_name = "笠岡市")), 77L)
})
