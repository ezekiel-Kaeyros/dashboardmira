box::use(
  shiny.fluent[Text, fluentPage],
  shiny[div, tags, NS, moduleServer, tagList, h3],
  shiny[tags,renderUI, uiOutput, NS,htmlOutput,moduleServer,tagList,sliderInput,observe,addResourcePath],
  LDAvis[createJSON, TwentyNewsgroups,visOutput,renderVis,serVis],
  tm[stopwords,],
  readxl[read_excel],
  quanteda[corpus,corpus_reshape,dfm,dfm_trim,convert],
  topicmodels[LDA,posterior],
  stats[terms], utils[data], shiny.fluent[Slider.shinyInput],
  wordcloud2[wordcloud2Output,renderWordcloud2]
)

box::use(
  app/view/components/layouts,
  app/logic/import_data
)

#' @export
wordcloud_ui <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  shiny::uiOutput(ns("ui"))
}

#' @export
wordcloud_server <- function(id) {
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
        div(style = "background-color: #f6f6f6; height: 55em;",
           tagList(
             div(style = "justify-content: flex-end; align-items: center; gap: 0.5rem; margin-right: 35px; display: flex; ",
                 shiny.fluent::DefaultButton.shinyInput("refresh", "Daten aktualisieren",
                                                        iconProps = list(iconName = "Refresh"),
                                                        style = "height: 62px; margin: 5px; background-color: #000; color: #fff; border-radius: 12px; display: flex; align-items: center;"),
                 shiny.fluent::Link(href=paste("#!/qualitative?token=", current_token(), sep = ""), "Siehe Qualitativ",
                                    style = "background-color: #000; text-decoration:none; padding: 1.5em 1.5em; text-align: center; border-color: #000; border-radius: 12px; border: 1px solid black; color: #fff; font-weight: bold; display: flex; align-items: center;"),
                 shiny.fluent::DefaultButton.shinyInput("export_quantitative", "Daten exportieren", style = "margin-top: 10px;",
                                                        iconProps = list(iconName = "Download"))),

             #div(style = "height: 3em;"),

             layouts$wordcloud_layout(shiny::uiOutput(ns('wordcloud')))#, current_token())
           )

        )

      } else{
        shiny::h3("Error 500 - Internal Server Error")
      }
    })

    output$wordcloud <- shiny::renderUI({
        import_data$generate_wordcloud(import_data$data$description, min_freq = 5, language = "german")
    })
  })
}





#'
#' wordcloud2[wordcloud2Output,renderWordcloud2]
#'
#'
#' box::use(
#'   app/view/components/layouts,
#'   app/logic/import_data
#' )
#'
#' #' @export
#' wordcloud_ui <- function(id) {
#'   ns <- NS(id)
#'   tagList(
#'     layouts$wordcloud_layout(wordcloud2Output(ns('wordcloud')))
#'   )
#'
#' }
#'
#' #' @export
#' wordcloud_server <- function(id) {
#'   moduleServer(id, function(input, output, session) {
#'     output$wordcloud <- renderWordcloud2({
#'       import_data$generate_wordcloud(import_data$data$description)
#'     })
#'
#'   })
#' }
#'
