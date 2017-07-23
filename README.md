
<!-- README.md is generated from README.Rmd. Please edit that file -->
jpndistrict
===========

[![Travis-CI Build Status](https://travis-ci.org/uribo/jpndistrict.svg?branch=master)](https://travis-ci.org/uribo/jpndistrict) [![codecov](https://codecov.io/gh/uribo/jpndistrict/branch/master/graph/badge.svg)](https://codecov.io/gh/uribo/jpndistrict)

*English version of README is [here](https://github.com/uribo/jpmesh/blob/master/README.en.md)*

Overview
--------

本パッケージが提供する行政区域データおよび市区町村役場データは国土交通省国土政策局「国土数値情報（行政区域データ 平成27年4月1日時点のデータ） <http://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N03.html> 」、「国土数値情報（市区町村役場データ 平成26年8月31日時点のデータ） <http://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-P34.html>」をもとに瓜生真也が編集・加工を行ったものです。そのため、このデータを利用した二次著作物を作成する際は、国土数値情報の利用約款に準拠するものとします。

Installation
------------

**`{jpndistrict}`**はCRANからインストールが可能です。また**`{devtool}`**パッケージを利用して開発版をインストールすることもできます。

``` r
install.packages("jpndistrict")
```

``` r
install.packages("devtools")
devtools::install_github("uribo/jpndistrict")
```

Usage
-----

基本的な使い方を説明します。

``` r
# パッケージの読み込み
library(jpndistrict)
```

### 行政区域データの取得

-   `spdf_jpn_pref()`... 都道府県全体の行政区域データ。引数`district = FALSE`で市区町村の区域のないデータを返します
-   `spdf_jpn_cities()`... 対象の都道府県にふくまれる特定の市区町村を抽出します

``` r
spdf_jpn_pref(14)
spdf_jpn_pref(14, district = FALSE)
spdf_jpn_cities(14, admin_name = "海老名市")
spdf_jpn_cities(33, admin_name = c("倉敷市", "笠岡市"))
```

### 市区町村役場データの取得

全国の市区町村の事務所に関する所在地等のデータセットを取得します

``` r
spdf_jpn_admins(code = 33)
spdf_jpn_admins(code = 33, jis_code_city = c("33101", "33212"))
```

### 特徴

-   `spdf_jpn_*()`関数が返す行政区域データはsfクラスです
-   都道府県コード、市区町村コードによる指定が可能です

今後の展望: Roadmap
-------------------

-   \[x\] テストコードの充実
-   \[x\] CI環境の構築
-   \[x\] Vignettes、ドキュメントの整備
-   \[x\] CRANへの登録
-   \[ \] Shinyアプリケーション実装

### 開発履歴: History

-   `2016-10-22` 開発に着手。 `v0.0.9999`
-   `2016-11-04` GitHubへのpush
-   `2016-12-03` CRANへの登録
-   `2017-07-23` **sf**パッケージへの対応
