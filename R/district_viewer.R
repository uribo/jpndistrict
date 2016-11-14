#' District Viewer
#'
#' @description Interactive district map and information tool.
#' @param code prefecture code (default 33)
#' @param color polygon line color for leaflet
#' @param ... other parameter to leaflet functions
#' @import leaflet
#' @import miniUI
#' @import shiny
#' @importFrom stringi stri_unescape_unicode
#' @examples
#' \dontrun{
#' district_viewer()
#' }
#' @export
district_viewer <-  function(code = 33, color = "red", ...) {

  jpnprefs <- city_name_full <- city_code <- jpnprefs <- NULL
  prefecture <- jis_code <- NULL

  # UI ----------------------------------------------------------------------
  ui <- miniPage(
    gadgetTitleBar("District Viewer"),
    miniTabstripPanel(
      miniTabPanel("Parameters", icon = icon("sliders"),
                   miniContentPanel(
                     selectInput("pref",
                                 stringi::stri_unescape_unicode("\\u90fd\\u9053\\u5e9c\\u770c\\u3092\\u9078\\u629e:"),
                                 choices = c(levels(jpnprefs$prefecture)),
                                 selected = code,
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
                        choices = unique(use_series(spdf_jpn_pref(admin_name = input$pref), city_name_full))
      )
    })


    output$my.table <- renderDataTable({

      d <- spdf_jpn_pref(admin_name = input$pref)@data %>%
        dplyr::select(city_name_full, city_code)


      if (!is.null(input$cities)) {
        d %>% dplyr::filter(city_name_full %in% c(input$cities))
      } else {
        d
      }

    })

    output$my.map <- renderLeaflet({

      prefcode <- jpnprefs %>% filter(prefecture == as.character(input$pref)) %>% use_series(jis_code)

      if (is.null(input$cities)) {
        map.data <- spdf_jpn_pref(admin_name = input$pref, district = FALSE)

      } else {
        map.data <- spdf_jpn_cities(jis_code_pref = prefcode, admin_name = c(input$cities))
      }

      leaflet(...) %>%
        addTiles(...) %>%
        addPolylines(data = map.data, color = color, ...)


    })
  }

  runGadget(ui, server, viewer = dialogViewer("district_viewer", width = 650, height = 500))
}
