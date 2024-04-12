box::use(
  shiny.fluent[Text, fluentPage],
  shiny[div, tags, NS, moduleServer],
)

box::use(
  app/view/components/layouts,
  app/view/modules/home_data_overview,
  app/view/modules/home_recent,
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  fluentPage(
    layouts$home_layout(
      home_data_overview$ui(ns("app_overview")),
      home_recent$ui(ns("app_recent"))
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}
