# jpndistrict 0.3.8 (2020-11-27)

- Using memoise

# jpndistrict 0.3.7 (2020-06-12)

- Reviewing how to download files, use curl from now on.

# jpndistrict 0.3.6 (2020-04-20)

- Changes to accomodate sf: st_make_valid no longer in lwgeom ([#32](https://github.com/uribo/jpndistrict/issues/32)).

# jpndistrict 0.3.5 (2020-04-02)

- Follow up sf v0.9.0 update.
- The geometry of the dataset referenced inside the package was encoded by Google's Polyline Encoding Algorithm with googlePolylines. This has reduced the file size.

# jpndistrict 0.3.4

- Fix CRAN package check results with Latin-1 error.
- Refactoring of dependency packages and functions mainly using rlang and tidyverse.
- Renewal pkgdown site and travis ci environment with tic.

# jpndistrict 0.3.3

## API

- Added new functions to validate jis code. (`code_validate()`, `code_reform()` and `cityname_reform()`)
- English column names have been added to the `jpnprefs` data set. [#21](https://github.com/uribo/jpndistrict/pull/21), by Ryo Nakagawara (@Ryo-N7)
- Added new function to find city code (`find_jis_code()`). This function do not consider historical changes. [#23](https://github.com/uribo/jpndistrict/pull/23)

## Fixed Issues

- Mismatching variable names in `jpn_pref()` [#17](https://github.com/uribo/jpndistrict/issues/17)
- Always returns the nearest city or prefecture [#18](https://github.com/uribo/jpndistrict/issues/18)
- Failed in `code_validate()` [#19](https://github.com/uribo/jpndistrict/issues/19)
- Unify behavior due to differences in parameters in `jpn_pref()`. [#22](https://github.com/uribo/jpndistrict/issues/22)

# jpndistrict 0.3.2

- Clarified the arguments that to select administration area. pref_code is jis code from 1 to 47. jis_code is an identifier for a city that range of 5 digits.
- `find_city()` is support sf geometry.

## Fixed Issues

- Remove redundant character from prefecture code 13 data [#13](https://github.com/uribo/jpndistrict/issues/13)
- `mesh_district()` now return only selected administation area [#15](https://github.com/uribo/jpndistrict/issues/15)
- Broken `district_viewer()` [#16](https://github.com/uribo/jpndistrict/issues/16)


# jpndistrict 0.3.1

- Setup tidy development environment.

## Issues

- Fix some bugs.
    - City administration data update [#4](https://github.com/uribo/jpndistrict/issues/4)
    - Reverse geocoding failed behabior [#10](https://github.com/uribo/jpndistrict/issues/10)

# jpndistrict 0.3.0

- Updated citation and licence. 
    - Approval No.603FY2017 information usage

## Issues

- Fix some bugs ([#6](https://github.com/uribo/jpndistrict/issues/6), [#7](https://github.com/uribo/jpndistrict/issues/7)).

## API

- New function a find from coordinates to prefecture and city that `find_pref()` and `find_city()`.
- Add a function `mesh_district()` that export to meshed city area.
- Projected coordinates are unified to WGS84 (espg: 4326).
- Rename `spdf_jpn_pref()` function to `jpn_pref()`.

# jpndistrict 0.2.0

- Added support for sf.
- Remove `gg_jpn_district()`. Use instead `ggplot2::geom_sf()` (ggplot2 > 2.2.1)
- Convert string variables to factor always FALSE.

# jpndistrict 0.1.1

- Modified raw data download method `wget` to `auto`.
- Character encoding for administration area dataset to UTF8. ([#2](https://github.com/uribo/jpndistrict/issues/2))

# jpndistrict 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* Prepare base functions.
* Added `district_viewer()` for shiny gadgets.
