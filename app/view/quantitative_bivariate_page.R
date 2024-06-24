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
  app/logic/import_data
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  shiny::uiOutput(ns("ui"))
  # items <- shiny.fluent::DefaultButton.shinyInput("export_data", "Export data",
  #                                                 iconProps = list(iconName = "Download"))
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

      if(token_json_data$email %in% import_data$login_data$email & token_json_data$role == import_data$role &
         converted_time > Sys.time()){
        layouts$quantitative_bivariate_layout(age_discrimination$ui(ns("affected_person")),
                                              age_influence$ui(ns("age_of_affected_person")),
                                              map_topic$ui(ns("map")),
                                              gender_discrimination$ui(ns("gender_discrimination")), current_token()) #,location$ui(ns("location"))
      } else{
        shiny::h3("Error 500 - Internal Server Error")
      }
    })


    age_discrimination$server("affected_person")
    age_influence$server("age_of_affected_person")
    gender_discrimination$server("gender_discrimination")
    map_topic$server("map")
  })
}
