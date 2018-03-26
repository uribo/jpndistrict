context("test-export.R")

d <- mesh_district(jis_code = 33101)

test_that("mesh_district", {
  expect_is(
    d,
    c("sf", "data.frame")
  )
  expect_equal(
    dim(d),
    c(11264L, 6L)
  )
  expect_named(
    d,
    c(
      "meshcode", "lng_center", "lat_center", "lng_error", "lat_error",
      "geometry"
    )
  )
})
