
box::use(
  shiny[moduleServer, div,NS, h3, p, uiOutput,
        observeEvent,reactiveValues, renderUI,reactiveVal, renderText],
  shiny.fluent[ActionButton.shinyInput,updateActionButton.shinyInput,Dropdown.shinyInput],
  plotly[plotlyOutput, renderPlotly, add_trace, layout, plot_ly, config],
  magrittr[`%>%`], shinyjs
)

box::use(
  app/view/components/ui/cards,
  app/logic/quantitative/affected_person_logic,
  app/logic/import_data,
  app/logic/functions,
  app/view/components/layouts
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  cards$card_ui("Betroffene Person",
                ActionButton.shinyInput(ns("toggleButton"), iconProps = list("iconName" = "BarChart4")),
                div(class = "card_content",
                    h3(class = "subtitle", shiny::textOutput(ns("subtitle"))),
                  # Graph goes here
                  uiOutput(ns("plot_personaf"))
                )
              )

}

#' @export
server <- function(id, filter) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    #ns <- NS(id)
    #output$plot_personaf <- render
    data_personaf <- affected_person_logic$data_personaf(filter)

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
    output$plot_personaf <- renderUI({
      if (toggle$barplot) {
        plotlyOutput(ns("barplot"))
      } else {
        plotlyOutput(ns("piechart"))
      }
    })

    output$barplot <- renderPlotly({
      #functions$generate_barplot(data_personaf,"Betroffene Person")
      plotly::plot_ly(data_personaf, x = ~Var1,
                      type = "bar",
                      y = ~percentage,
                      marker =list(color="#85C2FF"),
                      text = paste(data_personaf$pct1, sep = ""), textposition = 'outside',
                      textfont = list(size = 10), # size is defined here
                      hovertext = paste("Betroffene Person:", data_personaf$Var1,
                                        "<br>Anzahl der Personen:", data_personaf$Freq,
                                        "<br>Prozentsatz:",data_personaf$pct1), #) %>%
                      hoverinfo = 'text') %>%
        layout(title = "",#margin = list(l=25, r=50, b=50, t=50, pad=4),
               uniformtext=list(minsize=10, mode='show'),
               xaxis = list(title = "<b> </b>", #font = list(size = 0),
                            tickfont = list(size = 11),
                            titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
                            tickangle= -45, showgrid = FALSE),
               yaxis = list(title = "<b> Prozentsatz </b>",
                            titlefont = list(size = 12),
                            tickfont = list(size = 12),
                            ticksuffix = "%", showgrid = FALSE)
        ) %>%
        config(displayModeBar = F,
               scrollZoom = T)
    })

    output$piechart <- renderPlotly({
      plot_ly(data_personaf, labels= ~Var1,
              values= ~Freq, type="pie",
              hoverinfo = 'text',
              textinfo = 'label+percent',
              texttemplate = '<b>%{label}</br></br>%{percent}</b>',
              insidetextfont = list(color = '#000',size = 8),
              text = ~paste("Betroffene Person:", Var1,
                            "<br>Anzahl der Personen:", Freq,
                            "<br>Prozentsatz:", pct1),
              marker = list(colors = c("#85C2FF", "#85C2FF","#85C2FF","#85C2FF","#85C2FF"),
                            line = list(color = '#FFFFFF', width = 1),showlegend = FALSE)) %>%
        layout(title="",
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%
        layout(showlegend = FALSE)
    })

    observeEvent(input$toggleButton, {
      toggle$barplot <- !toggle$barplot
    })
    output$subtitle <- shiny::renderText({
      data <- affected_person_logic$person_af_sub(filter)
      paste("Anzahl der betroffenen Personen:", nrow(unique(data)))
    })

  })
}
