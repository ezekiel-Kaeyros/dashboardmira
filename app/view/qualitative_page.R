box::use(
  shiny.fluent[Text, fluentPage],
  shiny[div, tags, NS, moduleServer, tagList, h3],
  shiny[tags,renderUI, uiOutput, NS,htmlOutput,moduleServer,tagList,sliderInput,observe,addResourcePath],
  LDAvis[createJSON, TwentyNewsgroups,visOutput,renderVis,serVis],
  tm[stopwords,],
  readxl[read_excel],
  quanteda[corpus,corpus_reshape,dfm,dfm_trim,convert],
  topicmodels[LDA,posterior],
  stats[terms], utils[data], shiny.fluent[Slider.shinyInput]
)

box::use(
  app/view/components/layouts,
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

      if(token_json_data$email %in% import_data$login_data$email & token_json_data$role == import_data$role &
         converted_time > Sys.time()){
        tagList(
          layouts$qualitative_layout(htmlOutput(ns('myChart')), current_token())
        )
      } else{
        shiny::h3("Error 500 - Internal Server Error")
      }
    })

    output$myChart <- renderUI({
      root <- getwd()
      # file_path <- "C:/Users/LENOVO/Downloads/incidents_description.xlsx"
      path_data <- paste(root, "/", "app/data", sep="")
      route <- paste(path_data,"/Topic_modelling", sep="")
      #addResourcePath("lda", "C:/Users/LENOVO/Desktop/Projets/antid_rhino/antid/anti-d-dashboard/Topic_modelling")
      addResourcePath("lda", route)
      url = "lda/index.html"
      lda <- tags$iframe(src=url, height=700, width=1200)
      lda
    })

  })
}
