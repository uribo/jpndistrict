context("reverse geocoding")

test_that("prefecture", {
  test <- find_pref(longitude = 130.4412895, latitude = 30.2984335)
  expect_s3_class(test, "tbl")
  expect_equal(dim(test), c(1, 3))
  expect_equal(test$pref_code, "46")
  expect_is(test$prefecture, "character")
  expect_identical(sf::st_crs(test), crs_4326)

  skip_if_not_installed("lwgeom")
  test <- find_pref(longitude = 140.1137418, latitude = 36.0533957)
  expect_named(test, c("pref_code", "prefecture", "geometry"))
  expect_equal(test$pref_code, "08")
  expect_equal(test$prefecture,
               intToUtf8(c(33576, 22478, 30476), multiple = FALSE))

  test <- find_pref(geometry = sf::st_point(c(136.6833, 35.05)))
  expect_equal(test$pref_code, "24")
  test <- find_prefs(geometry = sf::st_point(c(136.6833, 35.05)))
  expect_equal(dim(test), c(6, 4))

  test <-
    find_pref(135.8167, 35.3)
  expect_equal(dim(test), c(1, 3))

})

test_that("Failed", {
  expect_message(
    find_pref(125.2468750000, 24.7145833333),
    enc2native(
      intToUtf8(
        c(
          25351,
          23450,
          12375,
          12383,
          24231,
          27161,
          12364,
          12509,
          12522,
          12468,
          12531,
          12395,
          21547,
          12414,
          12428,
          12414,
          12379,
          12435
        ), multiple = FALSE
      )
    )
  )

  expect_message(
    find_city(longitude = 140.639815, latitude = 36.108976),
    enc2native(intToUtf8(
      c(
        25351,
        23450,
        12375,
        12383,
        24231,
        27161,
        12364,
        12509,
        12522,
        12468,
        12531,
        12395,
        21547,
        12414,
        12428,
        12414,
        12379,
        12435
      ), multiple = FALSE)
    ))
  test <-
    find_pref(125.2468750000, 24.7145833333)
  expect_identical(test, NULL)

})

test_that("city", {
  test <- find_city(longitude = 130.4412895, latitude = 30.2984335)
  expect_s3_class(test, "tbl")
  expect_equal(dim(test), c(1, 4))
  expect_named(test, c("prefecture", "city_code", "city", "geometry"))
  expect_equal(test$city_code, "46505")
  expect_is(test$prefecture, "character")

  test <- find_city(longitude = 140.1137418, latitude = 36.0533957, geometry = NULL)
  expect_equal(test$city_code, "08220")
  expect_equal(test$city,
               intToUtf8(c(12388, 12367, 12400, 24066), multiple = FALSE))

  test <- find_city(geometry = sf::st_point(c(130.4412895, 30.2984335)))
  expect_equal(test$city_code, "46505")

  res <-
    find_city(geometry = st_point(c(136.6833, 35.05)))
  expect_equal(res$city_code, "24205")

})
