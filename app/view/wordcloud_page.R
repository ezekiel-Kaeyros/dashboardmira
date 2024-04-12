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
  tagList(
    layouts$wordcloud_layout(shiny::uiOutput(ns('wordcloud')))
  )

}

#' @export
wordcloud_server <- function(id) {
  moduleServer(id, function(input, output, session) {
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
