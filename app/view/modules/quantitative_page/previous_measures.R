box::use(
  shiny[moduleServer, div,NS, h3, p, uiOutput,
        observeEvent,reactiveValues, renderUI,reactiveVal, renderText,plotOutput,
        renderPlot],
  shiny.fluent[ActionButton.shinyInput,updateActionButton.shinyInput],
  plotly[plotlyOutput, renderPlotly, add_trace, layout, plot_ly, config],
  magrittr[`%>%`], dplyr,graphics[axis, text],RColorBrewer
)

box::use(
  app/view/components/ui/cards,
  app/logic/quantitative/previous_measures_logic,
  app/logic/functions
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  cards$card_ui("Bereits ergriffene Maßnahmen",#"",
                ActionButton.shinyInput(ns("toggleButton"), iconProps = list("iconName" = "PieSingle")),
                div(class = "card_content",
                    h3(class = "subtitle",  shiny::textOutput(ns("text"))),
                    # Graph goes here
                    uiOutput(ns("previous_measures"))#,width="500px", height = 485) #,width="500px"
                )
  )

}

#' @export
server <- function(id, filter) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    previous_measures <- previous_measures_logic$previous_measures(filter)
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
    output$previous_measures <- renderUI({
      if (toggle$piechart) {
        plotlyOutput(ns("piechart"))
      } else {
        plotlyOutput(ns("barplot"))
      }
    })

    output$barplot <- renderPlotly({
      #functions$generate_barplot(previous_measures_f,"Aktion")
      plotly::plot_ly(previous_measures, x = ~Var1,
                      type = "bar",
                      y = ~percentage,
                      marker =list(color="#F9AFC5"),
                      text = paste(previous_measures$pct1, sep = ""), textposition = 'outside',
                      textfont = list(size = 10), # size is defined here
                      hovertext = paste("Aktion: ", previous_measures$Var1,
                                        "<br>Anzahl der Personen:", previous_measures$Freq,
                                        "<br>Prozentsatz:",previous_measures$pct1), #) %>%
                      #"<br>Percentage :", data_marsta()$pct1),
                      hoverinfo = 'text') %>%
        layout(title = "",
               uniformtext=list(minsize=10, mode='show'),
               xaxis = list(title = "<b> </b>", #font = list(size = 0),
                            # change x-axix size
                            tickfont = list(size = 12),
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
      #functions$generate_piechart(previous_measures_f,"Action")
      plot_ly(previous_measures, labels= ~Var1,
              values= ~Freq, type="pie",
              hoverinfo = 'text',
              textinfo = 'label+percent',
              texttemplate = '<b>%{label}</br></br>%{percent}</b>',
              insidetextfont = list(color = '#000',size = 8),
              text = ~paste("Aktion:", Var1,
                            "<br>Anzahl der Personen:", Freq,
                            "<br>Prozentsatz:", pct1),
              marker = list(colors = c("#F9AFC5", "#F9AFC5","#F9AFC5","#F9AFC5","#F9AFC5"),
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
      toggle$piechart <- !toggle$piechart
    })
    output$text <- shiny::renderText({
      #previous_measures$Var1[which.max(previous_measures$percentage)]
      paste("Die meisten bisherigen Maßnahmen: ", paste0(previous_measures$Var1[which.max(previous_measures$Freq)]))
    })

  })
}
