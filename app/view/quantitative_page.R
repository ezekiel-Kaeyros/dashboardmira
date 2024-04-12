box::use(
  shiny.fluent[Text, fluentPage, Dropdown.shinyInput],
  shiny[div, tags, NS, moduleServer, tagList, h1, h3],
)

box::use(
  app/view/components/layouts,
  app/view/modules/quantitative_page/affected_person,
  app/view/modules/quantitative_page/age_of_affected_person,
  app/view/modules/quantitative_page/gender_identity,
  app/view/modules/quantitative_page/date_of_occurance,
  app/view/modules/quantitative_page/map,
  app/view/modules/quantitative_page/previous_measures,
  app/view/modules/quantitative_page/location_f,
  app/view/modules/quantitative_page/area_location,
  app/logic/import_data
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()

  layouts$quantitative_page_layout(div(class = "head_section",
    #h1(class = "quantitative_page__title", ""), #Quantitative statistics
    div(style="display: flex",
        h3("Filter by identity:", style="font-family: 'Times New Roman', sans-serif; font-size: 1.25em; color: #333; margin-right: 10px;"),
        div(style="width: 200px; margin-top: 15px",
            Dropdown.shinyInput(ns("filter"),
                                value = import_data$options_filter[[1]]$key,
                                options = import_data$options_filter
                                ))
        ),
    ),

    affected_person$ui(ns("affected_person")), age_of_affected_person$ui(ns("age_of_affected_person")),
    map$ui(ns("map")),location_f$ui(ns("location_f")),gender_identity$ui(ns("gender_identity")),
    date_of_occurance$ui(ns("date_of_occurance")), previous_measures$ui(ns("previous_measures")),
    area_location$ui(ns("area_location"))) #,location$ui(ns("location"))
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observeEvent(input$filter, {
      affected_person$server("affected_person", input$filter)
    })

    shiny::observeEvent(input$filter, {
      age_of_affected_person$server("age_of_affected_person", input$filter)
    })

    shiny::observeEvent(input$filter, {
      gender_identity$server("gender_identity", input$filter)
    })

    shiny::observeEvent(input$filter, {
      date_of_occurance$server("date_of_occurance", input$filter)
    })


    shiny::observeEvent(input$filter, {
      previous_measures$server("previous_measures", input$filter)
    })

    shiny::observeEvent(input$filter, {
      location_f$server("location_f", input$filter)
    })

    shiny::observeEvent(input$filter, {
      area_location$server("area_location", input$filter)
    })

    shiny::observeEvent(input$filter, {
      map$server("map", input$filter)
    })

  })
}
