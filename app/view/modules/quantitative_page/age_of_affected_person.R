box::use(
  shiny[moduleServer, div,NS, h3, h5,p, uiOutput,
        observeEvent,reactiveValues, renderUI,reactiveVal, renderText],
  shiny.fluent[ActionButton.shinyInput,updateActionButton.shinyInput],
  plotly[plotlyOutput, renderPlotly, add_trace, layout, plot_ly, config],
  magrittr[`%>%`]
)

box::use(
  app/view/components/ui/cards,
  app/logic/import_data,
  app/logic/quantitative/age_affected_person_logic,
  app/logic/functions
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  cards$card_ui(shiny::textOutput(ns("title")) ,
                ActionButton.shinyInput(ns("toggleButton"), iconProps = list("iconName" = "PieSingle")),
                div(class = "card_content",
                    h3(class = "subtitle", shiny::textOutput(ns("text"))),
                    # Graph goes here
                    uiOutput(ns("plot_personage"))
                )
  )

}

#' @export
server <- function(id, filter) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    #ns <- NS(id)
    data_personage <- age_affected_person_logic$data_age(filter)

    button_state <- reactiveVal(FALSE)

    observeEvent(input$toggleButton, {
      button_state(!button_state())
      if (button_state()) {
        updateActionButton.shinyInput(session, "toggleButton", iconProps = list("iconName" = "BarChart4"))
      } else {
        updateActionButton.shinyInput(session, "toggleButton", iconProps = list("iconName" = "PieSingle"))
      }
    })

    toggle <- reactiveValues(piechart = TRUE)
    output$plot_personage <- renderUI({
      if (toggle$piechart) {
        plotlyOutput(ns("piechart"))
      } else {
        plotlyOutput(ns("barplot"))
      }
    })

    if (filter== "Organisation/Institution"){
      output$title <- shiny::renderText({
        paste0("Anzahl der in der Organisation beschäftigten Personen")
        })

      output$text <- shiny::renderText({
        paste("Höhere Gruppe:", paste0(data_personage$Var1[which.max(data_personage$Freq)]))
      })

      output$barplot <- renderPlotly({
        functions$generate_barplot(data_personage,"Anzahl Beschäftigte")
      })

      output$piechart <- renderPlotly({
        functions$generate_piechart(data_personage,"Anzahl Beschäftigte")
      })

    } else {
      output$title <- shiny::renderText({
        paste0("Anzahl der in der Organisation beschäftigten Personen")
      })

      output$text <- shiny::renderText({
        paste("Höhere Gruppe:", paste0(data_personage$Var1[which.max(data_personage$Freq)]))
      })

      output$barplot <- renderPlotly({
        functions$generate_barplot(data_personage,"Alter")
      })

      output$piechart <- renderPlotly({
        functions$generate_piechart(data_personage,"Alter")
      })
    }

    observeEvent(input$toggleButton, {
      toggle$piechart <- !toggle$piechart
    })

  })
}

