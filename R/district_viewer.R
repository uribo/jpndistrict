#' District Viewer
#'
#' @description Interactive district map and information tool.
#' @param color polygon line color for leaflet
#' @import leaflet
#' @import miniUI
#' @import shiny
#' @importFrom dplyr filter mutate pull
#' @importFrom sf st_transform
#' @examples
#' \dontrun{
#' district_viewer()
#' }
#' @export
district_viewer <- function(color = "red") {
  # nolint start
  # nocov start
  . <-
    prefecture <- city <- city_code <- jis_code <- geometry <- NULL
  # UI ----------------------------------------------------------------------
  ui <- miniPage(
    gadgetTitleBar("District Viewer"),
    miniTabstripPanel(
      miniTabPanel(
        "Parameters",
        icon = icon("sliders"),
        miniContentPanel(
          selectInput(
            inputId = "pref",
            label = paste(intToUtf8(c(37117, 36947, 24220, 30476, 12434, 36984, 25246, 58), multiple = TRUE), collapse = ""),
            choices = as.character(jpnprefs$prefecture),
            selected = jpnprefs$prefecture[1],
            multiple = FALSE
          ),
          conditionalPanel(
            "input.pref !== null",
            selectInput("cities",
                        "select",
                        choices = NULL,
                        multiple = TRUE)
          )
        )
      ),
      miniTabPanel(
        "Map",
        icon = icon("map-o"),
        miniContentPanel(padding = 0,
                         leafletOutput("my.map", height = "100%"))
      ),
      miniTabPanel("Data", icon = icon("table"),
                   miniContentPanel(dataTableOutput("my.table")))
    )
  )
  # Server ------------------------------------------------------------------
  server <- function(input, output, session) {
    observe({
      input$pref
      updateSelectInput(
        session,
        "cities",
        paste(intToUtf8(c(24066, 21306, 30010, 26449, 12434, 36984, 25246), multiple = TRUE), collapse = ""),
        choices = unique(dplyr::pull(
          jpn_pref(admin_name = input$pref),
          city
        ))
      )
    })
    output$my.table <- renderDataTable({
      d <- jpn_pref(admin_name = input$pref) %>%
        dplyr::select(city_code, city, geometry)
      if (!is.null(input$cities)) {
        dplyr::filter(d, city %in% c(input$cities))
      } else {
        d
      }
    })
    output$my.map <- renderLeaflet({
      prefcode <-
        jpnprefs %>%
        dplyr::filter(prefecture == as.character(input$pref)) %>%
        dplyr::pull(jis_code)

      if (is.null(input$cities)) {
        map_data <- jpn_pref(pref_code = prefcode, district = TRUE)
      } else {
        map_data <-
          jpn_cities(jis_code = prefcode,
                     admin_name = c(input$cities))
      }
      leaflet() %>%
        addTiles() %>%
        addPolylines(data = map_data,
                     color = color,
                     label = ~ map_data$city)
    })
  }
  runGadget(ui,
            server,
            viewer = dialogViewer("district_viewer", width = 650, height = 500))
  # nolint end
  # nocov end
}
