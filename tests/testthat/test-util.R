context("utilities")

test_that("Administration code varidation", {

  expect_equal(
    admins_code_validate(1),
    list(administration_type = "prefecture", code = "01")
  )
  expect_identical(
    admins_code_validate(1),
    admins_code_validate("1")
  )
  expect_error(
    admins_code_validate(48)
  )
  expect_length(
    sapply(1:47, admins_code_validate),
    94L
  )
  expect_equal(
    admins_code_validate("33103"),
    list(administration_type = "city", code = "33103")
  )
  expect_error(
    admins_code_validate("49999"),
    "x must be start a integer or as character from 1 to 47."
  )

})

test_that("return prefecture jis code as string", {
  test <-
    collect_prefcode(33)
  expect_is(test, "character")
  expect_equal(test, "33")

  char_okym <-
    paste(intToUtf8(c(23713, 23665, 30476), multiple = TRUE), collapse = "")
  expect_identical(
    collect_prefcode(33),
    collect_prefcode(admin_name = char_okym)
  )
})

test_that("available kjs data", {

  skip_on_os("linux")
  skip_on_os("windows")
  skip_on_travis()
  skip_on_appveyor()
  skip_on_cran()
  test <-
    read_ksj_cityarea(code = 17)
  expect_is(
    test,
    c("sf", "data.frame")
  )
  expect_equal(
    dim(test),
    c(40L, 6L)
  )
  expect_named(
    test,
    c("pref_name",
      "city_name_", "city_name", "city_name_full", "city_code",
      "geometry")
  )
})

test_that("reverge-geo coding", {
  test <-
    which_pol_min(longitude = 130.4412895, latitude = 30.2984335)

  expect_is(test, "list")
  expect_s3_class(test$spdf, c("tbl"))
  expect_gte(nrow(test$spdf), 1L)
  expect_type(test$which, "integer")
})

test_that("input geometry", {

  # expect_null(sfg_point_as_coords(geometry = NULL))

  skip_if_not_installed("sf")
  test <-
    sfg_point_as_coords(sf::st_point(c(130.4412895, 30.2984335)))
  expect_is(test, "list")
  expect_named(test, c("longitude", "latitude"))

})
