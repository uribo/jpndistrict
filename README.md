
<!-- README.md is generated from README.Rmd. Please edit that file -->
概要: Description
-----------------

本パッケージが提供する行政区域データは国土交通省国土政策局「国土数値情報（行政区域データ 平成27年4月1日時点のデータ） <http://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N03.html> 」をもとに瓜生真也が編集・加工を行ったものです。そのため、このデータを利用した二次著作物を作成する際は、国土数値情報の利用約款に準拠するものとします。

インストール: Install
---------------------

CRANには未登録ですので、**`{devtool}`**パッケージを利用してGitHubのリポジトリ経由でインストールを実行してください。次のコードを実行することでインストールされます。

``` r
install.packages("devtools")
devtools::install_github("uribo/jpndistrict")
```

使い方: How to use
------------------

基本的な使い方を説明します。

``` r
# パッケージの読み込み
library(jpndistrict)
```

### 行政区画データの取得

-   `spdf_jpn_cities()`

``` r
spdf_jpn_cities(jis_code_pref = 33, admin_name = "倉敷市")
#> class       : SpatialPolygonsDataFrame 
#> features    : 84 
#> extent      : 133.6024, 133.8822, 34.41736, 34.66935  (xmin, xmax, ymin, ymax)
#> coord. ref. : +proj=longlat +ellps=GRS80 +no_defs 
#> variables   : 5
#> # A tibble: 84 × 5
#>    pref_name city_name_ city_name city_name_full city_code
#> *      <chr>      <chr>     <chr>          <chr>    <fctr>
#> 1     岡山県       <NA>    倉敷市         倉敷市     33202
#> 2     岡山県       <NA>    倉敷市         倉敷市     33202
#> 3     岡山県       <NA>    倉敷市         倉敷市     33202
#> 4     岡山県       <NA>    倉敷市         倉敷市     33202
#> 5     岡山県       <NA>    倉敷市         倉敷市     33202
#> 6     岡山県       <NA>    倉敷市         倉敷市     33202
#> 7     岡山県       <NA>    倉敷市         倉敷市     33202
#> 8     岡山県       <NA>    倉敷市         倉敷市     33202
#> 9     岡山県       <NA>    倉敷市         倉敷市     33202
#> 10    岡山県       <NA>    倉敷市         倉敷市     33202
#> # ... with 74 more rows
```

### 特徴

-   SpatialPolygonDataframe
-   各種の描画システムに対応
-   都道府県コード、市区町村コードによる指定

### 各種描画システムによる行政区画データのプロット

-   \[x\] `base::plot()`
-   \[x\] **`{ggplot2}`**
-   \[x\] **`{plotly}`**

今後の展望: Roadmap
-------------------

-   \[ \] テストコードの充実
-   \[ \] CI環境の構築
-   \[ \] Vignettes、ドキュメントの整備
-   \[ \] CRANへの登録
-   \[ \] Shinyアプリケーション実装

### 開発履歴: History

-   `2016-10-22` 開発に着手。 `v0.0.9999`

ref
---

<https://github.com/hrbrmstr/albersusa>
