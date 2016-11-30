context("util")

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
