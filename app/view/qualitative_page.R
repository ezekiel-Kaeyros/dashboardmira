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

        div(style = "background-color: #f6f6f6;",
            tagList(
              div(style = "justify-content: flex-end; align-items: center; gap: 0.5rem;  margin-right: 35px; display: flex; ",
                  shiny.fluent::DefaultButton.shinyInput("refresh", "Daten aktualisieren",
                                                         iconProps = list(iconName = "Refresh"),
                                                         style = "height: 62px; margin: 5px; background-color: #000; color: #fff; border-radius: 12px; display: flex; align-items: center;"),
                  shiny.fluent::Link(href=paste("#!/wordcloud?token=", current_token(), sep = ""), "Siehe Wordcloud",
                                     style = "background-color: #000; text-decoration:none; padding: 1.5em 1.5em; text-align: center; border-color: #000; border-radius: 12px; border: 1px solid black; color: #fff; font-weight: bold; display: flex; align-items: center;"),
                  shiny.fluent::DefaultButton.shinyInput("export_quantitative", "Daten exportieren", style = "margin-top: 10px;",
                                                         iconProps = list(iconName = "Download"))),

              div(style = "height: 3em;"),
              layouts$qualitative_layout(htmlOutput(ns('myChart')))
            )
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
      lda <- tags$iframe(src=url, height=890, width=1300)
      lda
    })

  })
}
