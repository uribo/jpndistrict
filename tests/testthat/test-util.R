context("utilities")

test_that("return prefecture jis code as string", {
  test <- pref_code(33)
  expect_is(test, "character")
  expect_equal(test, "33")
  expect_equal(pref_code(jis_code = 2), "02")
})

test_that("return prefecture jis code as string", {
  test <- collect_prefcode(33)
  expect_is(test, "character")
  expect_equal(test, "33")
})

test_that("reverge-geo coding", {
  test <- which_pol_min(longitude = 130.4412895, latitude = 30.2984335)

  expect_is(test, "list")
  expect_s3_class(test$spdf, c("tbl"))
  expect_gte(nrow(test$spdf), 1L)
  expect_type(test$which, "integer")
})
