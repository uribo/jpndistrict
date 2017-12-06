context("reverse geocoding")

test_that("prefecture", {

  test <- find_pref(longitude = 130.4412895, latitude = 30.2984335)
  expect_s3_class(test, "tbl")
  expect_equal(dim(test), c(1, 3))
  expect_equal(test$pref_code, "46")
  expect_is(test$prefecture, "character")
  expect_identical(sf::st_crs(test), crs_4326)

  test <- find_pref(longitude = 140.1137418, latitude = 36.0533957)
  expect_named(test, c("pref_code", "prefecture", "geometry"))
  expect_equal(test$pref_code, "08")
  expect_equal(test$prefecture, stringi::stri_unescape_unicode("\\u8328\\u57ce\\u770c"))
})

test_that("city", {
  test <- find_city(longitude = 130.4412895, latitude = 30.2984335)
  expect_s3_class(test, "tbl")
  expect_equal(dim(test), c(1, 4))
  expect_named(test, c("prefecture", "city_code", "city", "geometry"))
  expect_equal(test$city_code, "46505")
  expect_is(test$prefecture, "character")

  test <- find_city(longitude = 140.1137418, latitude = 36.0533957)
  expect_equal(test$city_code, "08220")
  expect_equal(test$city, stringi::stri_unescape_unicode("\\u3064\\u304f\\u3070\\u5e02"))
})
