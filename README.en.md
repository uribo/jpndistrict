
<!-- README.md is generated from README.Rmd. Please edit that file -->
jpndistrict <img src="logo.png" align="right" width="80px" />
=============================================================

[![Travis-CI Build Status](https://travis-ci.org/uribo/jpndistrict.svg?branch=master)](https://travis-ci.org/uribo/jpndistrict) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/jpndistrict)](http://cran.r-project.org/package=jpndistrict) [![codecov](https://codecov.io/gh/uribo/jpndistrict/branch/master/graph/badge.svg)](https://codecov.io/gh/uribo/jpndistrict)

Overview
--------

In this package, the administrative area data to be provided uses the National Land Numerical Information "[National Land Numeral Information](http://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N03.html)". Shinya Uryu is editing and processing this data. Therefore, when preparing a secondary work using this data, it is necessary to follow the term of the National Land Numerical Information.

Installation
------------

Please use the **`{devtool}`** package to install via the GitHub repository.

``` r
install.packages("devtools")
devtools::install_github("uribo/jpndistrict")
```

Usage
-----

``` r
# Load Package
library(jpndistrict)
```

### Administrative area data

``` r
spdf_jpn_pref(14)
spdf_jpn_pref(14, district = FALSE)
spdf_jpn_cities(spdf_jpn_pref(14), admin_name = "海老名市")
spdf_jpn_cities(jis_code_pref = 33, admin_name = c("倉敷市", "笠岡市"))
```

See [vignettes](vignettes/create_map.Rmd) for details.
