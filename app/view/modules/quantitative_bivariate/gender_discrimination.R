
box::use(
  shiny[moduleServer, div,NS, h3, p, uiOutput,
        observeEvent,reactiveValues, renderUI,reactiveVal, renderText],
  shiny.fluent[ActionButton.shinyInput,updateActionButton.shinyInput],
  plotly[plotlyOutput, renderPlotly, add_trace, layout, plot_ly, config, style, ggplotly],
  magrittr[`%>%`],
  ggplot2[ggplot,geom_tile,geom_text,scale_fill_gradient,labs,aes,theme,element_text]
)

box::use(
  app/view/components/ui/cards,
  app/logic/import_data,
  app/logic/quantitative_bivariate/gender_disc_logic
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  cards$card_ui("Anzahl der Personen, die einer anderen Form der Diskriminierung ausgesetzt waren, gruppiert nach Geschlecht",
                ActionButton.shinyInput(ns("toggleButton"), iconProps = list("iconName" = "BarChart4")),
                div(class = "card_content",
                    # Graph goes here
                    uiOutput(ns("plot_date"))
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
    yiord_palette <- c("#fecbb3", "#feb999", "#fea880", "#fe9667", "#fe854d",
                       "#fd7334", "#fd621a", "#fd5001", "#e44801", "#ca4001", "#b13801", "#983001", "#7f2801",
                       "#652000", "#4c1800")

    observeEvent(input$toggleButton, {
      button_state(!button_state())
      if (button_state()) {
        updateActionButton.shinyInput(session, "toggleButton", iconProps = list("iconName" = "table"))
      } else {
        updateActionButton.shinyInput(session, "toggleButton", iconProps = list("iconName" = "BarChart4"))
      }
    })

    toggle <- reactiveValues(barplot = TRUE)
    output$plot_date <- renderUI({
      if (toggle$barplot) {
        plotlyOutput(ns("barplot"))
      } else {
        plotlyOutput(ns("piechart"))
      }
    })

    output$barplot <- renderPlotly({
      plotly::plot_ly(gender_disc_logic$table_gender_disc, x = ~gender, y = ~Freq, color = ~discrimination, type = "bar", colors = yiord_palette,
                      text = ~paste("Geschlecht: ", gender, "<br>Frequenz: ", Freq, "<br>Diskriminierung: ", discrimination)) %>%
        layout(showlegend = T,
               xaxis = list(title = "Geschlecht"),
               yaxis = list(title = "Diskriminierung"),
               barmode = "group") %>%
        style(hoverinfo = "text")
    })

    output$piechart <- renderPlotly({
      gg <- ggplot(gender_disc_logic$table_gender_disc, aes(gender, discrimination)) +
        geom_tile(aes(fill = Freq)) +
        geom_text(aes(label = round(Freq, 1), text = paste("Geschlecht :", gender, "\nDiskriminierung:", discrimination, "\nFrequenz:", Freq))) +
        scale_fill_gradient(low = "#FED976", high = "red") +
        labs(#title = "Frequency of Different Types of Discrimination by Gender",
             x = "Geschlecht",
             y = "Diskriminierung",
             fill = "Count") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      ggplotly(gg, tooltip = "text")
    })

    observeEvent(input$toggleButton, {
      toggle$barplot <- !toggle$barplot
    })


  })
}
