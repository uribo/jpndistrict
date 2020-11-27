context("test-export.R")

d <-
  mesh_district(jis_code = "33101", to_mesh_size = 1)
d2 <-
  mesh_district(jis_code = "14401", to_mesh_size = 10)
d3 <-
  mesh_district(jis_code = "08", to_mesh_size = 80)

test_that("mesh_district", {
  skip_on_appveyor()
  expect_is(
    d,
    c("sf", "data.frame", "tbl_df"))
  expect_equal(
    dim(d),
    c(510L, 2L))
  expect_equal(
    dim(d2),
    c(3L, 2L))
  expect_named(
    d,
    c(
      "meshcode",
      "geometry"
    )
  )
  expect_equal(dim(d3), c(5, 2))
})
