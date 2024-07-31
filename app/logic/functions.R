box::use(
  shiny[moduleServer, div,NS, h3, p, uiOutput,
        observeEvent,reactiveValues, renderUI,reactiveVal, renderText],
  shiny.fluent[ActionButton.shinyInput,updateActionButton.shinyInput],
  plotly[plotlyOutput, renderPlotly, add_trace, layout, plot_ly, config,style,ggplotly],
  magrittr[`%>%`],
  ggplot2[ggplot,geom_tile,geom_text,scale_fill_gradient,labs,aes,theme,element_text]
)

#' @export
generate_barplot <- function(data, text) {
  plotly::plot_ly(data, x = ~Var1,
                  type = "bar",
                  y = ~percentage,
                  marker = list(color =  couleurs <- c("#fecbb3", "#feb999", "#fea880", "#fe9667", "#fe854d",
                                                       "#fd7334", "#fd621a", "#fd5001", "#e44801", "#ca4001", "#b13801", "#983001", "#7f2801",
                                                       "#652000", "#4c1800", "#331000", "#190800", "#000000")

                  ),
                  text = paste(data$pct1, sep = ""), textposition = 'outside',
                  textfont = list(size = 10), # size is defined here
                  hovertext = paste(paste(text,":",data$Var1),
                                    "<br>Anzahl der Personen:", data$Freq,
                                    "<br>Prozentsatz:",data$pct1), #) %>%
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
}

#' @export
generate_piechart <- function(data, text) {
  plotly::plot_ly(data, labels = ~Var1,
                  values = ~Freq, type = "pie",
                  hoverinfo = 'text',
                  textinfo = 'label+percent',
                  texttemplate = '<b>%{label}</br></br>%{percent}</b>',
                  insidetextfont = list(color = '#000',size = 8),
                  text = ~paste(paste(text, ":", Var1),
                                "<br>Anzahl der Personen:", Freq,
                                "<br>Prozentsatz:", pct1),
                  marker = list(colors = c("#fecbb3", "#feb999", "#fea880", "#fe9667", "#fe854d",
                                           "#fd7334", "#fd621a", "#fd5001", "#e44801", "#ca4001", "#b13801", "#983001", "#7f2801",
                                           "#652000", "#4c1800", "#331000", "#190800", "#000000"),
                                line = list(color = '#FFFFFF', width = 1)),
                  showlegend = FALSE) %>%
    plotly::layout(title = "",
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   showlegend = FALSE) %>%
    plotly::config(displayModeBar = TRUE,
                   displaylogo = FALSE,
                   modeBarButtonsToRemove = list(
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
                   scrollZoom = TRUE)

}

#' @export
generate_groupedbarplot <- function(data, text1,text2){
  yiord_palette <- c("#fecbb3", "#feb999", "#fea880", "#fe9667", "#fe854d",
                     "#fd7334", "#fd621a", "#fd5001", "#e44801", "#ca4001", "#b13801", "#983001", "#7f2801",
                     "#652000", "#4c1800")
  plotly::plot_ly(data, x = ~Var1, y = ~Freq, color = ~Var2, type = "bar", colors = yiord_palette,
                  text = ~paste(paste(text1,":"), Var1, "<br>Frequenz: ", Freq, "<br>",paste(text2,":"), Var2)) %>%
    layout(#title = "Frequency of Different Forms of Discrimination by Age Group",
      xaxis = list(title = text1),
      yaxis = list(title = "Frequenz"),
      barmode = "group")%>%
    style(hoverinfo = "text") %>%
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
}

#' @export
generate_table <- function(data, text1, text2) {
  gg<-ggplot(data, aes(Var1, Var2)) +
    geom_tile(aes(fill = Freq)) +
    geom_text(aes(label = round(Freq, 1), text = paste(paste(text1,":"), Var1, "\n",paste(text2,":"), Var2, "\nZählen Sie:", Freq))) +
    scale_fill_gradient(low = "#fecbb3", high = "#4c1800") +
    labs(#title = "Frequency of Different Forms of Discrimination by Age Group",
      x = text1,#Age Group
      y = text2,#Discrimination
      fill = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggplotly(gg, tooltip = "text") %>%
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
}
