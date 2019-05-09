context("utilities")

test_that("Data", {
  expect_length(export_pref_80km_mesh(1), 40L)

  expect_named(jpnprefs,
               c("jis_code", "prefecture", "capital", "region", "major_island",
                 "prefecture_en", "capital_en", "region_en", "major_island_en",
                 "capital_latitude", "capital_longitude"))
  expect_equal(dim(jpnprefs),
               c(47, 11))
  expect_s3_class(jpnprefs,
                  c("data.frame", "tbl_df"))

  expect_s3_class(prefecture_mesh, c("tbl", "data.frame"))
  expect_equal(dim(prefecture_mesh), c(314, 5))
  expect_named(prefecture_mesh, c("prefcode", "meshcode", "name", "type", "geometry"))
})


test_that("Administration code varidation", {

  expect_equal(
    code_reform(list("1", 2, 33, "01", 49, "02201", "33202")),
    c("01", "02", "33", "01", "49", "02201", "33202")
  )
  expect_error(
    code_reform(list("1", 1, 33, "01", 49, 111, "02201", "33202"))
  )

  res <-
    match_city_name(jis_code = "33101")
  expect_is(
    res,
    "data.frame")
  expect_equal(
    dim(res),
    c(1, 2)
  )
  expect_named(
    res,
    c("city_code", "city")
  )
  expect_equal(
    dim(match_city_name(c("08210", "08212", "33101"))),
    c(3, 2)
  )
  expect_message(
    expect_equal(
      dim(match_city_name(c("01101", "01999"))),
      c(1, 2)
    ),
    "1 matching code were not found."
  )
  expect_message(
    match_city_name("01999"),
    "1 matching code were not found."
  )

  expect_equal(
    code_validate(1),
    list(administration_type = "prefecture", code = "01")
  )
  expect_identical(
    code_validate(1),
    code_validate("1")
  )
  expect_equal(
    code_validate("08201")$code,
    "08201"
  )
  expect_error(
    code_validate(48)
  )
  expect_length(
    sapply(1:47, code_validate),
    94L
  )
  expect_equal(
    code_validate("33103"),
    list(administration_type = "city", code = "33103")
  )
  expect_error(
    code_validate("49999"),
    "jis_code must be start a integer or as character from 1 to 47"
  )
})

test_that("return prefecture jis code as string", {
  test <-
    collect_prefcode(33)
  expect_is(test, "character")
  expect_equal(test, "33")


  char_okym <-
    intToUtf8(c(23713, 23665, 30476), multiple = FALSE)
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

  test <-
    sfg_point_as_coords(sf::st_point(c(130.4412895, 30.2984335)))
  expect_is(test, "list")
  expect_named(test, c("longitude", "latitude"))

})

test_that("City name table", {

  skip_if_not(l10n_info()$`UTF-8`)
  x_okayama <-
    enc2native(intToUtf8(c(23713, 23665, 24066),
                         multiple = FALSE))
  x_kurashiki <-
    enc2native(intToUtf8(c(20489, 25975, 24066),
                         multiple = FALSE))
  x_kanagawa <-
    c(enc2native(intToUtf8(c(37772, 20489, 24066),
                           multiple = FALSE)),
      enc2native(intToUtf8(c(23567, 30000, 21407, 24066),
                           multiple = FALSE)))

  expect_equal(
    find_jis_code(33,
                  admin_name = x_kurashiki),
    "33202")
  expect_equal(
    expect_warning(
      find_jis_code(33,
                    admin_name = x_okayama,
                    strict = TRUE)),
    NA_character_)
  expect_length(
    find_jis_code(33,
                  admin_name = x_okayama,
                  strict = FALSE),
   4L)
  expect_setequal(
    find_jis_code(14,
                  admin_name = x_kanagawa,
                  strict = FALSE),
    c("14204", "14206"))

  x_syouou <-
    enc2native(intToUtf8(c(21213, 30000, 37089, 21213, 22830, 30010),
                       multiple = FALSE))
  x_syouou_space <-
    enc2native(intToUtf8(c(21213, 30000, 37089, 32, 21213, 22830, 30010),
                         multiple = FALSE))

  x_okayama_kita_space <-
    enc2native(intToUtf8(c(23713, 23665, 24066, 32, 21271, 21306),
                       multiple = FALSE))

  expect_identical(
    cityname_reform(x_syouou),
    x_syouou_space)
  expect_is(
    cityname_reform(x_okayama),
    "character")

  expect_equal(
    cityname_reform(x_syouou_space),
    x_syouou_space)
  expect_equal(
    cityname_reform(x_okayama_kita_space),
    x_okayama_kita_space)

})
