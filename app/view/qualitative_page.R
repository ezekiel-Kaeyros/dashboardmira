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
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    layouts$qualitative_layout(htmlOutput(ns('myChart')))
  )

}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
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
