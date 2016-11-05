context("ksj_p33")

test_that("Collect datasets", {
  expect_s4_class(read_ksj_p33(code = 47), "SpatialPointsDataFrame")
  expect_equal(dim(read_ksj_p33(code = 47)@data), c(65, 6))
  expect_named(read_ksj_p33(code = 47)@data,
               c("jis_code", "type", "name", "address", "longitude", "latitude"))
})
