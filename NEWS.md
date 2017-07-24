# jpndistrict 0.2.0

- Added support for sf.
- Remove `gg_jpn_district()`. Use instead `ggplot2::geom_sf()` (ggplot2 > 2.2.1)
- Convert string variables to factor always FALSE.

# jpndistrict 0.1.1

- Modified raw data download method `wget` to `auto`.
- Character encoding for administration area dataset to UTF8. (#2)

# jpndistrict 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* Prepare base functions.
* Added `district_viewer()` for shiny gadgets.
