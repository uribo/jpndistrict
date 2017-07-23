context("spdf_jpn")

df.pref <- spdf_jpn_pref(code = 33, district = TRUE)
df.city <- spdf_jpn_cities(jis_code_pref = 33, jis_code = 33103)
df.admins <- spdf_jpn_admins(code = 47)

test_that("spdf_jpn_pref", {
  expect_s3_class(df.pref, c("sf", "tbl", "data.frame"))
  expect_equal(spdf_jpn_pref(code = 33, district = FALSE)$pref_name, stringi::stri_unescape_unicode("\\u5ca1\\u5c71\\u770c"))
  expect_error(spdf_jpn_pref(admin_name = "\\u5ca1\\u5c71\\u770c", code = 12), info = "provide multiple arguments")
})

test_that("spdf_jpn_cities", {
  expect_s3_class(df.city, c("sf", "tbl", "data.frame"))
  expect_named(df.city,
               c("pref_name", "city_name_", "city_name", "city_name_full", "city_code", "geometry"))
  expect_equal(df.city$pref_name[1], stringi::stri_unescape_unicode("\\u5ca1\\u5c71\\u770c"))
  expect_equal(df.city$city_name_[1], stringi::stri_unescape_unicode("\\u5ca1\\u5c71\\u5e02"))
  expect_equal(df.city$city_name[1], stringi::stri_unescape_unicode("\\u6771\\u533a"))
  expect_equal(df.city$city_name_full[1], stringi::stri_unescape_unicode("\\u5ca1\\u5c71\\u5e02 \\u6771\\u533a"))
  expect_equal(nrow(spdf_jpn_cities(jis_code_pref = 33, jis_code = c(33103, 33104, 33205))), 24L)
  expect_equal(nrow(spdf_jpn_cities(jis_code_pref = 33, jis_code = 33205)), 17L)
})

test_that("Collect administration offices data", {
  expect_s3_class(df.admins, c("sf", "tbl", "data.frame"))
  expect_equal(dim(df.admins), c(65, 5))
  expect_named(df.admins,
               c("jis_code", "type", "name", "address", "geometry"))
  expect_is(df.admins$jis_code[1], "factor")
  expect_equal(dim(spdf_jpn_admins(code = 47, jis_code_city = c("47205", "47209"))), c(6, 5))
})


