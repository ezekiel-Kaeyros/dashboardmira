
box::use(
    shiny[moduleServer, div,NS, h3, p, uiOutput,
           observeEvent,reactiveValues, renderUI,reactiveVal, renderText],
    shiny.fluent[ActionButton.shinyInput,updateActionButton.shinyInput],
    plotly[plotlyOutput, renderPlotly, add_trace, layout, plot_ly, config,style,ggplotly],
    magrittr[`%>%`],
    ggplot2[ggplot,geom_tile,geom_text,scale_fill_gradient,labs,aes,theme,element_text]
)

box::use(
  app/view/components/ui/cards,
  app/logic/import_data,
  app/logic/quantitative_bivariate/age_inf_logic
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  cards$card_ui("Häufigkeit des Auftretens von Diskriminierung nach Gruppenalter",
                ActionButton.shinyInput(ns("toggleButton"), iconProps = list("iconName" = "BarChart4")),
                div(class = "card_content",
                  # Graph goes here
                  uiOutput(ns("plot_personaf"))
                )
              )

}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    #ns <- NS(id)
    #output$plot_personaf <- render
    button_state <- reactiveVal(FALSE)
    yiord_palette <- c("#FFFFCC", "#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#BD0026", "#800026")

    observeEvent(input$toggleButton, {
      button_state(!button_state())
      if (button_state()) {
        updateActionButton.shinyInput(session, "toggleButton", iconProps = list("iconName" = "table"))
      } else {
        updateActionButton.shinyInput(session, "toggleButton", iconProps = list("iconName" = "BarChart4"))
      }
    })

    toggle <- reactiveValues(barplot = TRUE)
    output$plot_personaf <- renderUI({
      if (toggle$barplot) {
        plotlyOutput(ns("barplot"))
      } else {
        plotlyOutput(ns("piechart"))
      }
    })

    output$barplot <- renderPlotly({
      plotly::plot_ly(age_inf_logic$table_age_inf, x = ~category_age, y = ~Freq, color = ~influence, type = "bar", colors = yiord_palette, text = ~paste("Altersgruppe: ", category_age, "<br>Frequenz: ", Freq, "<br>Manifestation: ", influence)) %>%
        layout(showlegend = T,
               xaxis = list(title = "Altersgruppe"),
               yaxis = list(title = "Manifestation"),
               barmode = "group") %>%
        style(hoverinfo = "text")
    })

    output$piechart <- renderPlotly({
      gg <- ggplot(age_inf_logic$table_age_inf, aes(category_age, influence)) +
        geom_tile(aes(fill = Freq)) +
        geom_text(aes(label = round(Freq, 1),
                      text = paste("Altersgruppe:", category_age, "\nManifestation:", influence, "\nFrequenz:", Freq)),
                  show.legend = FALSE) +
        scale_fill_gradient(low = "#FED976", high = "red") +
        labs(#title = "Frequency of Different Types of Influence by Age Group",
             x = "Altersgruppe",
             y = "Manifestation",
             fill = "Count") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      ggplotly(gg, tooltip = "text")
    })

    observeEvent(input$toggleButton, {
      toggle$barplot <- !toggle$barplot
    })


  })
}
