context("util")

test_that("multiplication works", {
  expect_equal(pref_code(jis_code = 33), "33")
  expect_equal(pref_code(jis_code = 2), "02")
})
