context("sf_jpn")

test_that("jpn_pref", {
  df_pref <-
    jpn_pref(
      pref_code = 33,
      district = TRUE,
      drop_sinkyokyoku = FALSE
    )
  expect_s3_class(df_pref, c("sf"))
  expect_s3_class(df_pref, c("tbl"))
  expect_equal(df_pref$prefecture[1],
               paste(intToUtf8(c(23713, 23665, 30476), multiple = TRUE), collapse = ""))

  expect_equal(
    unique(jpn_pref(pref_code = 33)$pref_code),
    "33"
  )

  char_okym <- paste(intToUtf8(c(23713, 23665, 30476), multiple = TRUE), collapse = "")
  expect_identical(
    jpn_pref(pref_code = 33),
    jpn_pref(admin_name = char_okym)
  )

  expect_named(
    df_pref,
    c(
      "pref_code",
      "prefecture",
      "sichyo_sinkyokyoku",
      "city_code",
      "city",
      "geometry"
    )
  )
  expect_identical(sf::st_crs(df_pref), crs_4326)
  expect_named(
    jpn_pref(pref_code = 12, drop_sinkyokyoku = TRUE),
    c("pref_code", "prefecture", "city_code", "city", "geometry")
  )

  df_pref2 <- jpn_pref(pref_code = 14, district = FALSE)
  expect_s3_class(df_pref2, c("sf"))
  expect_s3_class(df_pref2, c("tbl"))
  expect_named(df_pref2,
               c("jis_code", "prefecture", "geometry"))
  expect_identical(sf::st_crs(df_pref2), crs_4326)
})

test_that("jpn_cities", {
  df_city <- jpn_cities(jis_code = 33103)
  expect_s3_class(df_city, c("sf"))
  expect_s3_class(df_city, c("tbl"))
  expect_named(df_city,
               c("city_code", "city", "geometry"))
  expect_equal(
    df_city$city[1],
    paste(intToUtf8(c(23713, 23665, 24066, 32, 26481, 21306), multiple = TRUE), collapse = "")
  )
  expect_equal(nrow(jpn_cities(jis_code = c(
    33103, 33104, 33205
  ))), 3L)
  expect_equal(nrow(jpn_cities(jis_code = 33205)), 1L)
  expect_identical(sf::st_crs(df_city), crs_4326)

  expect_equal(dim(
    jpn_cities(
      jis_code = 33,
      admin_name = paste(intToUtf8(c(23713, 23665, 24066, 32, 26481, 21306), multiple = TRUE), collapse = "") # nolint
    )
  ),
  c(1, 3))

})

test_that("Collect administration offices data", {
  df_admins <- jpn_admins(jis_code = 47)

  expect_s3_class(df_admins, c("sf"))
  expect_equal(dim(df_admins), c(65, 5))
  expect_named(df_admins,
               c("jis_code", "type", "name", "address", "geometry"))
  expect_is(df_admins$jis_code[1], "factor")
})
