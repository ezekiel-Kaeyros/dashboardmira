box::use(
  shiny[div, NS, moduleServer, h1, observeEvent,verbatimTextOutput],
  shiny.fluent[Text,ActionButton.shinyInput],
  plotly[plotlyOutput, renderPlotly, plot_ly, add_pie, layout],
  magrittr[`%>%`], shinyjs,shiny.react
)

box::use(
  app/view/components/ui/cards,
  app/logic/import_data
)

#' @export
ui <- function(id) {
  shinyjs::useShinyjs()
  ns <- NS(id)
  #cards$recent_card(Sys.Date())
  div(#id=ns("page1"),
    h1(class = "recent_page__subtitle", "Recent"),
    div(class = "recent_page__list",
        items <- lapply(1:5, function(i) {
        #Accéder à la date correspondante dans report_date
        report_date <- import_data$data$report_date[i]
        # Créer le contenu de la carte
        cards$recent_card(import_data$data$report_id[i],
                          paste("#!/overview?id=",import_data$data$report_id[i], sep = ""),
                          paste(report_date))
        #shiny.fluent::ActionButton.shinyInput(inputId=ns("Report_1"),"Report_1", iconProps=list("iconName" = "InfoSolid"))

                                #shiny.react::onclick =JS("() => alert('Calendar event clicked')"))
        }),
        #verbatimTextOutput("urlText")
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    #shinyjs::onclick("Report_1", shinyjs::runjs("window.open('http://google.com', '_blank')"))

    })
  }
