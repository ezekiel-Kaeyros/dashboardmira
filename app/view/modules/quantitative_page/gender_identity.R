
box::use(
  shiny[moduleServer, div,NS, h1,h3, p, uiOutput,
        observeEvent,reactiveValues, renderUI,reactiveVal, renderText],
  shiny.fluent[ActionButton.shinyInput,updateActionButton.shinyInput],
  plotly[plotlyOutput, renderPlotly, add_trace, layout, plot_ly, config],
  magrittr[`%>%`]
)

box::use(
  app/view/components/ui/cards,
  app/logic/import_data,
  app/logic/quantitative/gender_logic,
  app/logic/functions
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  cards$card_ui(shiny::textOutput(ns("title")) ,
                ActionButton.shinyInput(ns("toggleButton"), iconProps = list("iconName" = "BarChart4")),
                div(class = "card_content",
                    h3(class = "subtitle", shiny::textOutput(ns("text"))),
                    # Graph goes here
                    uiOutput(ns("plot_persongen"))
                )
  )

}



#' @export
server <- function(id, filter) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    #ns <- NS(id)
    data_gender <- gender_logic$data_gender(filter)
    button_state <- reactiveVal(FALSE)

    observeEvent(input$toggleButton, {
      button_state(!button_state())
      if (button_state()) {
        updateActionButton.shinyInput(session, "toggleButton", iconProps = list("iconName" = "PieSingle"))
      } else {
        updateActionButton.shinyInput(session, "toggleButton", iconProps = list("iconName" = "BarChart4"))
      }
    })

    toggle <- reactiveValues(barplot = TRUE)
    output$plot_persongen <- renderUI({
      if (toggle$barplot) {
        plotlyOutput(ns("barplot"))
      } else {
        plotlyOutput(ns("piechart"))
      }
    })


    if (filter== "Organisation/Institution"){
      output$title <- shiny::renderText({
        paste0("Häufigkeit der betroffenen Organisationen")
      })

      output$text <- shiny::renderText({
        paste("Höhere Gruppe:", paste0(data_gender$Var1[which.max(data_gender$Freq)]))
      })

      output$barplot <- renderPlotly({
        functions$generate_barplot(data_gender,"Organisation")

      })

      output$piechart <- renderPlotly({
        functions$generate_piechart(data_gender,"Organisation")

      })
    } else {
      output$title <- shiny::renderText({
        paste0("Geschlechtsidentität")
      })

      output$text <- shiny::renderText({
        paste("Höhere Gruppe:", paste0(data_gender$Var1[which.max(data_gender$Freq)]))
      })

      output$barplot <- renderPlotly({
        functions$generate_barplot(data_gender,"Geschlecht")

      })

      output$piechart <- renderPlotly({
        functions$generate_piechart(data_gender,"Geschlecht")

      })
    }

    observeEvent(input$toggleButton, {
      toggle$barplot <- !toggle$barplot
    })

  })
}
