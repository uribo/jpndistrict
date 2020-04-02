context("reverse geocoding")

test_that("prefecture", {
  skip_if_not_installed("lwgeom")
  test <- find_pref(longitude = 130.4412895, latitude = 30.2984335)
  expect_s3_class(test, "tbl")
  expect_equal(dim(test), c(1, 3))
  expect_equal(test$pref_code, "46")
  expect_is(test$prefecture, "character")
  if (utils::packageVersion("sf") <= numeric_version("0.8.1")) {
    expect_identical(sf::st_crs(test), crs_4326)
  } else {
    expect_equal(sf::st_crs(test)$input, "EPSG:4326")
  }
  test <- find_pref(geometry = sf::st_point(c(136.6833, 35.05)))
  expect_equal(test$pref_code, "24")
  test <- find_prefs(longitude = NULL,
                     latitude = NULL,
                     geometry = sf::st_point(c(136.6833, 35.05)))
  expect_equal(dim(test), c(6, 4))
  expect_named(test,
               c("pref_code", "meshcode_80km", "prefecture", "region"))

  test <- find_pref(longitude = 140.1137418, latitude = 36.0533957)
  expect_named(test, c("pref_code", "prefecture", "geometry"))
  expect_equal(test$pref_code, "08")
  expect_false(sf::st_is_empty(test$geometry))

  skip_if_not(l10n_info()$`UTF-8`)
  expect_equal(test$prefecture,
               intToUtf8(c(33576, 22478, 30476), multiple = FALSE))
})

test_that("Failed", {
  skip_if_not_installed("lwgeom")
  expect_message(
    find_pref(125.2468750000, 24.7145833333),
    "Specified coordinates are not included in the polygon.")

  expect_message(
    find_city(longitude = 140.639815, latitude = 36.108976),
    "Specified coordinates are not included in the polygon.")
  test <-
    find_pref(125.2468750000, 24.7145833333)
  expect_identical(test, NULL)

})

test_that("city", {
  skip_if_not_installed("lwgeom")
  test <- find_city(longitude = 130.4412895, latitude = 30.2984335)
  expect_s3_class(test, "tbl")
  expect_equal(dim(test), c(1, 4))
  expect_named(test, c("prefecture", "city_code", "city", "geometry"))
  expect_equal(test$city_code, "46505")
  expect_is(test$prefecture, "character")
  expect_false(sf::st_is_empty(test$geometry))

  test <- find_city(geometry = sf::st_point(c(130.4412895, 30.2984335)))
  expect_equal(test$city_code, "46505")

  test <- find_city(geometry = st_point(c(136.6833, 35.05)))
  expect_equal(test$city_code, "24205")

  test <- find_city(longitude = 140.1137418, latitude = 36.0533957, geometry = NULL) # nolint
  expect_equal(test$city_code, "08220")
  expect_false(sf::st_is_empty(test$geometry))
  skip_if_not(l10n_info()$`UTF-8`)
  expect_equal(test$city,
               intToUtf8(c(12388, 12367, 12400, 24066), multiple = FALSE))
})
