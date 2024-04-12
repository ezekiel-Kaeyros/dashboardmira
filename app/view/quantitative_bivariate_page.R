box::use(
  shiny.fluent[Text, fluentPage],
  shiny[div, tags, NS, moduleServer, tagList],
)

box::use(
  app/view/components/layouts,
  app/view/modules/quantitative_bivariate/age_discrimination,
  app/view/modules/quantitative_bivariate/age_influence,
  app/view/modules/quantitative_bivariate/gender_discrimination,
  app/view/modules/quantitative_bivariate/map_topic,
)

box::use(
  app/logic/import_data
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  items <- shiny.fluent::DefaultButton.shinyInput("export_data", "Export data",
                                                  iconProps = list(iconName = "Download"))
  layouts$quantitative_bivariate_layout(age_discrimination$ui(ns("age_discrimination")),
                                        age_influence$ui(ns("age_of_affected_person")),
                                         map_topic$ui(ns("map")),
                                        gender_discrimination$ui(ns("gender_discrimination")))
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    age_discrimination$server("age_discrimination")
    age_influence$server("age_of_affected_person")
    gender_discrimination$server("gender_discrimination")
    map_topic$server("map")

  })
}
