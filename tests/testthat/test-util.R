context("utilities")

test_that("return prefecture jis code as string", {
  test <-
    pref_code(33)
  expect_is(test, "character")
  expect_equal(test, "33")
  expect_equal(pref_code(jis_code = 2), "02")
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
