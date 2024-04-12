box::use(
  shiny[moduleServer, div,NS, h3, p, uiOutput,
        observeEvent,reactiveValues, renderUI,reactiveVal, renderText],
  shiny.fluent[ActionButton.shinyInput,updateActionButton.shinyInput],
  plotly[plotlyOutput, renderPlotly, add_trace, layout, plot_ly, config],
  magrittr[`%>%`]
)

box::use(
  app/view/components/ui/cards,
  app/logic/import_data,
  app/logic/quantitative/location_logic,
  app/logic/functions
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  cards$card_ui("Standort",
                ActionButton.shinyInput(ns("toggleButton"), iconProps = list("iconName" = "BarChart4")),
                div(class = "card_content",
                    h3(class = "subtitle", shiny::textOutput(ns("text"))),
                    # Graph goes here
                    uiOutput(ns("plot_loc"),width="500px", height = 500)
                )
  )

}

#' @export
server <- function(id, filter) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    #ns <- NS(id)
    data_onreal <- location_logic$data_onreal(filter)

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
    output$plot_loc <- renderUI({
      if (toggle$barplot) {
        plotlyOutput(ns("barplot"))
      } else {
        plotlyOutput(ns("piechart"))
      }
    })

    output$barplot <- renderPlotly({
      #functions$generate_barplot(data_onreal,"Standort")

      plotly::plot_ly(data_onreal, x = ~Var1,
                      type = "bar",
                      y = ~percentage,
                      marker =list(color="#85C2FF"),
                      text = paste(data_onreal$pct1, sep = ""), textposition = 'outside',
                      textfont = list(size = 10), # size is defined here
                      hovertext = paste("Betroffene Person: ", data_onreal$Var1,
                                        "<br>Anzahl der Personen:", data_onreal$Freq,
                                        "<br>Prozentsatz:",data_onreal$pct1), #) %>%
                      hoverinfo = 'text') %>%
        layout(title = "",#margin = list(l=25, r=50, b=50, t=50, pad=4),
               uniformtext=list(minsize=10, mode='show'),
               xaxis = list(title = "<b> </b>", #font = list(size = 0),
                            # change x-axix size
                            tickfont = list(size = 11),
                            # change x-title size
                            titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
                            tickangle= -45, showgrid = FALSE),
               yaxis = list(title = "<b> Prozentsatz </b>",
                            titlefont = list(size = 12),
                            # change x-axix size
                            tickfont = list(size = 12),
                            ticksuffix = "%", showgrid = FALSE)
        ) %>%
        config(displayModeBar = T,displaylogo = FALSE, modeBarButtonsToRemove = list(
          'sendDataToCloud',
          #'toImage',
          'autoScale2d',
          'zoomIn2d',
          "zoomOut2d",
          'toggleSpikelines',
          'resetScale2d',
          'lasso2d',
          'zoom2d',
          'pan2d',
          'select2d',#,
          'hoverClosestCartesian',#,
          'hoverCompareCartesian'),
          scrollZoom = T)
    })

    output$piechart <- renderPlotly({
      #functions$generate_piechart(data_onreal,"Standort")

      plot_ly(data_onreal, labels= ~Var1,
              values= ~Freq, type="pie",
              hoverinfo = 'text',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              text = ~paste("Betroffene Person:", Var1,
                            "<br>Anzahl der Personen:", Freq,
                            "<br>Prozentsatz:", pct1),
              marker = list(colors = c("#85C2FF", "#85C2FF","#85C2FF","#85C2FF","#85C2FF"),
                            line = list(color = '#FFFFFF', width = 1),showlegend = FALSE)) %>%
        layout(title="",
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
        layout(showlegend = FALSE) %>%
        config(displayModeBar = T,displaylogo = FALSE, modeBarButtonsToRemove = list(
          'sendDataToCloud',
          'hoverClosestPie',
          #'toImage',
          'autoScale2d',
          'zoomIn2d',
          "zoomOut2d",
          'toggleSpikelines',
          'resetScale2d',
          'lasso2d',
          'zoom2d',
          'pan2d',
          'select2d',#,
          'hoverClosestCartesian',#,
          'hoverCompareCartesian'),
          scrollZoom = T)
    })

    observeEvent(input$toggleButton, {
      toggle$barplot <- !toggle$barplot
    })
    output$text <- shiny::renderText({

      paste("Häufigster Ort: ", paste0(data_onreal$Var1[which.max(data_onreal$Freq)]))
    })

  })
}
