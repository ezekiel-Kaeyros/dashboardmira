box::use(
  shiny.fluent[Text, fluentPage],
  shiny[div, tags, NS, moduleServer, h3,h5, observe, parseQueryString,
        observeEvent, reactive,column],
  shiny.router,upstartr[not.na]
)

box::use(
  app/view/components/layouts,
  app/logic/import_data
)

options <- list(
  list(key = "1", text = "Option 1"),
  list(key = "2", text = "Option 2"),
  list(key = "3", text = "Option 3"),
  list(key = "4", text = "Option 4"),
  list(key = "5", text = "Option 5"),
  list(key = "6", text = "Option 6")
)

#' @export
overview_ui <- function(id) {
  ns <- NS(id)
  div( class = "text_summary",
    shiny::h1(class = "text_summary__title", "Data Info"),
    shiny.fluent::Link(href="#!/home", "Go Back"),
    div(class = "text_summary__content",
      div(class = "left",
          h3("  Summary"),
          div( class ="left_inside",
            shiny::textOutput(ns("report")),
            h3("Person affected"),
            shiny::textOutput(ns("pers_af")),
            h3("Gender identity"),
            shiny::textOutput(ns("gender")),
            h3("Age"),
            shiny::textOutput(ns("age")),
            h3("Date"),
            shiny::textOutput(ns("date")),
            shiny::conditionalPanel(
              condition = not.na(shiny::textOutput(ns("date"))),
              tags$h5("Incident lasted for a period: ",
                      shiny::textOutput(ns("startdate")),
                      "-",
                      shiny::textOutput(ns("enddate")))
            ),
            h3("Place of incident"),
            shiny::textOutput(ns("location")),
            h3("What happened"),
            shiny::textOutput(ns("description")),
            h3("Iinfluential Characteristics on Discrimination"),
            shiny::textOutput(ns("inf_disc")),
            h3("Is this another form of discrimination?"),
            shiny::textOutput(ns("form_disc")),
            h3("Previous measures"),
            shiny::textOutput(ns("previous_measures"))
          )

        ),
      div(class = "right",
          h3("Categorize Data"),
          div(class = "card_summary",
              h3("Category 1"),
              shiny.fluent::ChoiceGroup.shinyInput(ns("choice"), value = "B",
                                                   options = options,
                                                   style = "	display: flex; flex-direction: row;")
          ),
          shiny::br(),
          div(class = "card_summary",
              h3("Category 2"),
              shiny.fluent::ChoiceGroup.shinyInput(ns("choice"), value = "B",
                                                   options = options,
                                                   style = "	display: flex; flex-direction: row;")
          ),
          shiny::br(),
          div(class = "card_summary",
              h3("Category 3"),
              shiny.fluent::ChoiceGroup.shinyInput(ns("choice"), value = "B",
                                                   options = options,
                                                   style = "	display: flex; flex-direction: row;")
          ),
          shiny::br(),
          div(class = "card_summary",
              h3("Category 4"),
              shiny.fluent::ChoiceGroup.shinyInput(ns("choice"), value = "B",
                                                   options = options,
                                                   style = "	display: flex; flex-direction: row;")
          ),
          shiny::br(),
          div(class = "card_summary",
              h3("Category 5"),
              shiny.fluent::ChoiceGroup.shinyInput(ns("choice"), value = "B",
                                                   options = options,
                                                   style = "	display: flex; flex-direction: row;")
          ),
          shiny::br(),
          div(class = "card_summary",
              h3("Category 6"),
              shiny.fluent::ChoiceGroup.shinyInput(ns("choice"), value = "B",
                                                   options = options,
                                                   style = "	display: flex; flex-direction: row;")
          )
        )
    ),
  )
}

#' @export
overview_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    current_id <- reactive({
      id <- shiny.router::get_query_param("id", session)
      if(is.null(id)){
          "Report_1"
      }else{
        id
      }
    })
    output$report <- shiny::renderText({
      current_id()
    })
    output$pers_af <- shiny::renderText({
      report <- subset(import_data$data, report_id == current_id())
      report$person_affected
    })
    output$gender <- shiny::renderText({
      report <- subset(import_data$data, report_id == current_id())
      report$gender
    })
    output$age <- shiny::renderText({
      report <- subset(import_data$data, report_id == current_id())
      as.character(report$age_cat)[1]
    })
    output$date <- shiny::renderText({
      report <- subset(import_data$data, report_id == current_id())
      if (not.na(report$time_incident$exactDate)) {
        format(as.Date(report$time_incident$exactDate),"%d %B %Y")
      }
    })
    output$startdate <- shiny::renderText({
      report <- subset(import_data$data, report_id == current_id())
      if (not.na(report$time_incident$startDate)) {
        format(as.Date(report$time_incident$startDate),"%d %B %Y")
      }
    })
    output$enddate <- shiny::renderText({
      report <- subset(import_data$data, report_id == current_id())
      if (not.na(report$time_incident$startDate)) {
        format(as.Date(report$time_incident$endDate),"%d %B %Y")
      }
    })

    output$location <- shiny::renderText({
      report <- subset(import_data$data, report_id == current_id())
      if( report$location=="Online") {
        report$location
      } else {
        report$place_discrimination$city
      }
    })
    output$description <- shiny::renderText({
      report <- subset(import_data$data, report_id == current_id())
      report$description$description
    })
    output$inf_disc <- shiny::renderText({
      report <- subset(import_data$data, report_id == current_id())
      report$influence_of_the_discrimination[[1]]
      paste(report$influence_of_the_discrimination[[1]], collapse = ", ")
    })
    output$form_disc <- shiny::renderText({
      report <- subset(import_data$data, report_id == current_id())
      paste(report$another_discriminations[[1]], collapse = ", ")
    })
    output$previous_measures <- shiny::renderText({
      report <- subset(import_data$data, report_id == current_id())
      if (isTRUE(report$previous_measures)) {
        paste(report$previous_measures, " Measure taken.",sep = "")
      } else {
        paste("No previous measure")
      }
    })




  })
}
