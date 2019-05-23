
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jpndistrict <img src="man/figures/logo.png" align="right" width="120px" />

[![Travis-CI Build
Status](https://travis-ci.org/uribo/jpndistrict.svg?branch=master)](https://travis-ci.org/uribo/jpndistrict)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/jpndistrict)](https://cran.r-project.org/package=jpndistrict)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/uribo/jpndistrict?branch=master&svg=true)](https://ci.appveyor.com/project/uribo/jpndistrict)
[![Coverage
status](https://codecov.io/gh/uribo/jpndistrict/branch/master/graph/badge.svg)](https://codecov.io/github/uribo/jpndistrict?branch=master)

## Overview

In this package, the administrative area data to be provided uses the
National Land Numerical Information “National Land Numeral Information”
<http://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N03.html>
<http://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-P34.html>. Shinya
Uryu is editing and processing this data. Therefore, when preparing a
secondary work using this data, it is necessary to follow the term of
the National Land Numerical Information.

This package provide map data is based on the Digital Map 25000 (Map
Image) published by Geospatial Information Authority of Japan (Approval
No.603FY2017 information usage <http://www.gsi.go.jp>).

## Installation

Install from CRAN.

``` r
install.packages("jpndistrict")
```

For developers, please use the **devtool** package to install via the
GitHub repository.

``` r
install.packages("remotes")
remotes::install_github("uribo/jpndistrict")
```

## Usage

``` r
# Load Package
library(jpndistrict)
```

### Administrative area data

``` r
jpn_pref(14)
jpn_pref(14, district = FALSE)
jpn_cities(14, admin_name = "海老名市")
jpn_cities(33, admin_name = c("倉敷市", "笠岡市"))
```

``` r
# Return mesh polygons included in administrative areas.
mesh_district(jis_code = "05")
mesh_district(jis_code = 33101)
```

### Administration office data

``` r
jpn_admins(jis_code = 33)
jpn_admins(jis_code = c("33101", "33212"))
```

### Reverse geocoding in prefecture and city level

``` r
find_pref(longitude = 133.915, latitude = 34.666)
find_city(longitude = 133.915, latitude = 34.666)
# sfg
find_city(geometry = sf::st_point(c(140.112, 36.083)))
```

### City name and code validation

``` r
code_validate(jis_code = 33101)
code_reform(jis_code = c(1, "33", "08201"))
```
