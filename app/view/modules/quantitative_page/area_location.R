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
  app/logic/quantitative/area_location_logic,
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
    area_location <- area_location_logic$area_location(filter)

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
        paste0("Art der Diskriminierung, die von der Organisation gesehen wird")
      })

      output$text <- shiny::renderText({
        paste("Höhere Gruppe:", paste0(area_location$Var1[which.max(area_location$Freq)]))
      })

      output$barplot <- renderPlotly({
        functions$generate_barplot(area_location,"Diskriminierung")
      })

      output$piechart <- renderPlotly({
        functions$generate_piechart(area_location,"Diskriminierung")
      })

    } else {
      output$title <- shiny::renderText({
        paste0("Sphäre, in der Diskriminierung entsteht")
      })

      output$text <- shiny::renderText({
        paste("Höhere Gruppe:", paste0(area_location$Var1[which.max(area_location$Freq)]))
      })

      output$barplot <- renderPlotly({
        functions$generate_barplot(area_location,"Sphäre")
      })

      output$piechart <- renderPlotly({
        functions$generate_piechart(area_location,"Sphäre")
      })
    }

    observeEvent(input$toggleButton, {
      toggle$piechart <- !toggle$piechart
    })

  })
}

