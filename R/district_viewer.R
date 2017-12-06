#' District Viewer
#'
#' @description Interactive district map and information tool.
#' @param pref_code prefecture code (default 33)
#' @param color polygon line color for leaflet
#' @import leaflet
#' @import miniUI
#' @import shiny
#' @importFrom dplyr filter
#' @importFrom magrittr use_series
#' @importFrom sf st_transform
#' @importFrom stringi stri_unescape_unicode
#' @examples
#' \dontrun{
#' district_viewer()
#' }
#' @export
district_viewer <- function(pref_code = 33, color = "red") {

  prefecture <- city <- city_code <- jis_code <- geometry <- NULL

  # UI ----------------------------------------------------------------------
  ui <- miniPage(
    gadgetTitleBar("District Viewer"),
    miniTabstripPanel(
      miniTabPanel("Parameters", icon = icon("sliders"),
                   miniContentPanel(
                     selectInput(inputId = "pref",
                                 label = stringi::stri_unescape_unicode("\\u90fd\\u9053\\u5e9c\\u770c\\u3092\\u9078\\u629e:"),
                                 choices = as.character(jpnprefs$prefecture),
                                 selected = jpnprefs$prefecture[1],
                                 multiple = FALSE),
                     conditionalPanel("input.pref !== null",
                                      selectInput("cities", "select", choices = NULL,
                                                  multiple = TRUE))
                   )
      ),

      miniTabPanel("Map", icon = icon("map-o"),
                   miniContentPanel(padding = 0,
                                    leafletOutput("my.map", height = "100%")
                   )
      ),
      miniTabPanel("Data", icon = icon("table"),
                   miniContentPanel(
                     dataTableOutput("my.table")
                   )
      )
    )
  )

  # Server ------------------------------------------------------------------
  server <- function(input, output, session) {

    observe({
      input$pref
      updateSelectInput(session, "cities",
                        stringi::stri_unescape_unicode("\\u5e02\\u533a\\u753a\\u6751\\u3092\\u9078\\u629e"),
                        choices = unique(magrittr::use_series(jpn_pref(admin_name = input$pref), city))
      )
    })


    output$my.table <- renderDataTable({

      d <- jpn_pref(admin_name = input$pref) %>%
        dplyr::select(city_code, city, geometry)


      if (!is.null(input$cities)) {
        d %>% dplyr::filter(city %in% c(input$cities))
      } else {
        d
      }

    })

    output$my.map <- renderLeaflet({

      prefcode <- jpnprefs %>% dplyr::filter(prefecture == as.character(input$pref)) %>% magrittr::use_series(jis_code)

      if (is.null(input$cities)) {
        map.data <- jpn_pref(admin_name = input$pref, district = FALSE)

      } else {
        map.data <- jpn_cities(jis_code = prefcode, admin_name = c(input$cities))
      }

      leaflet() %>%
        addTiles() %>%
        addPolylines(data = map.data, color = color,
                     label = ~map.data$city)


    })
  }

  runGadget(ui, server, viewer = dialogViewer("district_viewer", width = 650, height = 500))
}
