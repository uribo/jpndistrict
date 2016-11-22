creditmsg <- iconv(c("本パッケージが提供する行政区域データおよび市区町村役場データは",
                     "国土交通省国土政策局「国土数値情報（行政区域データ 平成27年4月1日時点のデータ） http://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N03.html 」、",
                     "「国土数値情報（市区町村役場データ 平成26年8月31日時点のデータ） http://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-P34.html」をもとに瓜生真也が編集・加工を行ったものです。",
                     "そのため、このデータを利用した二次著作物を作成する際は、国土数値情報の利用約款に準拠するものとします。"),
             to = "UTF-8")

devtools::use_data(creditmsg, internal = TRUE, overwrite = TRUE)
