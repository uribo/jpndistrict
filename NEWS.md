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
