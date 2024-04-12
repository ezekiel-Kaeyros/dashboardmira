box::use(
  shiny[div, NS, moduleServer, img],
  shiny.fluent[Text]
)

box::use(
  app/view/components/ui/cards,
  app/logic/import_data
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(class = "cards_app_overview__list",
    cards$app_overview_card("Total Reports",
                                img(src = "totalDatasets.svg"),
                                nrow(import_data$data)),
    cards$app_overview_card("New Reports",
                                img(src = "dataBusinessProcess.svg"),
                                "50"),
    cards$app_overview_card("Recent Activities",
                                img(src = "recentActivities.svg"), "+15%"),
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session){
  })
}
