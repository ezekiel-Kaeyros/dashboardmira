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
  shiny::uiOutput(ns("ui"))

}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    current_token <- shiny::reactive({
      token <- shiny.router::get_query_param("token", session)
      if(is.null(token)){
        token <- "404"
      }else{
        token <- token
      }
      token
    })
    output$ui <- shiny::renderUI({
      ############# Decode JWT
      token_json_data <- jose::jwt_decode_hmac(current_token(), secret = import_data$key)

      ############ Detect validity time of token
      converted_time <- as.POSIXct(token_json_data$exp, origin="1970-01-01", tz="Africa/Lagos")

      if(token_json_data$email %in% import_data$login_data$email & token_json_data$role == import_data$role & converted_time > Sys.time()){
        layouts$quantitative_page_layout(
          div(style="display: flex;justify-content: space-between;  align-items: center; width:100%;", #class = "head_section",
              #h1(class = "quantitative_page__title", ""), #Quantitative statistics
              div(style="display: flex; align-items: center; gap:5px;", #gap: 0.1rem; align-items: center; float: right;
                  h3(style="margin-left:20px;","Identität auswählen: "),
                  div(style="width: 200px; ",
                      shiny.fluent::Dropdown.shinyInput(ns("filter"),
                                                        value = import_data$options_filter[[1]]$key,
                                                        options = import_data$options_filter
                      ))
              ),
              div(
                style = "display: flex; gap: 0.1rem; align-items: center; ",
                div(#style = "float: right;  gap: 0.5rem; margin-top: 10px;",#28px
                  shiny.fluent::DefaultButton.shinyInput("refresh", "Daten aktualisieren",
                                                         iconProps = list(iconName = "Refresh"),
                                                         style = "background-color: #000; text-decoration:none; padding: 1.5em 1.5em;
                            text-align: center; border-color: #fff; border-radius: 12px;
                            border: 1px solid black;height:60px;
                           color: #fff; font-weight: bold;"
                  )),
                shiny.fluent::Link(
                  href = paste("#!/quantitative_bivariate?token=", current_token(), sep = ""),
                  "Weiter zu Bivariate",
                  style = "background-color: #000; text-decoration: none; padding: 1.5em 1.5em;
             text-align: center; border-color: #fff; border-radius: 12px;
             border: 1px solid black; color: #fff; font-weight: bold; margin-top:50px; margin-left:5px"
                ),
                shiny.fluent::DefaultButton.shinyInput(
                  "export_quantitative",
                  "Daten exportieren",
                  iconProps = list(iconName = "Download"),
                  style="margin-top:15px"
                  # iconProps = list(
                  #   imageProps = list(
                  #     src = "www/logo.svg", # Path to your image
                  #     style = list(width = "16px", height = "16px") # Adjust as necessary
                  #   ))
                ),
              )
          ),

        affected_person$ui(ns("affected_person")), age_of_affected_person$ui(ns("age_of_affected_person")),
        map$ui(ns("map")),location_f$ui(ns("location_f")),gender_identity$ui(ns("gender_identity")),
        date_of_occurance$ui(ns("date_of_occurance")), previous_measures$ui(ns("previous_measures")),
        area_location$ui(ns("area_location")))#, current_token()) #,location$ui(ns("location"))
      } else{
        shiny::h3("Error 500 - Internal Server Error")
      }
    })

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
