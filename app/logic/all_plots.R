# ------------> wordcloud
box::use(
  shiny[renderPlot,plotOutput],
  wordcloud[wordcloud],
  RColorBrewer[brewer.pal]
)

box::use(
  app/logic/wordcloud

)
#' @export
ui <- function(id){
  ns <- NS(id)
  plotOutput(ns("wordcloud"))
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$wordcloud <- renderPlot({
      wordcloud(words = wordcloud$data_wordcloud$word, freq = wordcloud$data_wordcloud$freq, min.freq = 1,
                max.words=200, random.order=FALSE, rot.per=0.35,
                colors=brewer.pal(8, "Dark2"))
    })
  })
}


#------------------> another form of discrimination
box::use(
  plotly[renderPlotly,plotlyOutput,config,layout],
  magrittr[`%>%`],
)

box::use(
  app/logic/import_data,

)

#' @export
ui <- function(id){
  ns <- NS(id)
  plotlyOutput(ns("disc"))
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$disc <- renderPlotly({
      plotly::plot_ly(import_data$data_disc, x = ~Var1,
                      type = "bar",
                      y = ~percentage,
                      #marker = list(color = c("#0B5345", "#148F77", "#196F3D", "#52BE80", "#7DCEA0", "#CA6F1E")),
                      # marker = list(color = c("#483D8B", "slateblue","#0077BE", "#5696CC",
                      #                         "#76B7DA",  "#A6DAFF")),
                      marker =list(color="#85C2FF"),
                      #colors = "darkviolet",
                      #colors = c("darkgoldenrod", "#663399", "darkblue", "darkgreen"),
                      text = paste(import_data$data_disc$pct1, sep = ""), textposition = 'outside',
                      textfont = list(size = 10), # size is defined here
                      hovertext = paste("Another form of discrimination: ", import_data$data_disc$Var1,
                                        "<br>Number of persons :", import_data$data_disc$Freq1,
                                        "<br>Percentage :",import_data$data_disc$pct1), #) %>%
                      #"<br>Percentage :", data_marsta()$pct1),
                      hoverinfo = 'text') %>%
        layout(title = "",
               #legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b> </b>')),
               uniformtext=list(minsize=10, mode='show'),
               xaxis = list(title = "<b> </b>", #font = list(size = 0),
                            # change x-axix size
                            tickfont = list(size = 11),
                            # change x-title size
                            titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
                            tickangle= -45, showgrid = FALSE),
               yaxis = list(title = "<b> Percentage </b>",
                            titlefont = list(size = 12),
                            # change x-axix size
                            tickfont = list(size = 12),
                            ticksuffix = "%", showgrid = FALSE)
        ) %>%
        config(displayModeBar = F,
               scrollZoom = T)
    })
  })
}

# --------------> influence of the discrimination
box::use(
  plotly[renderPlotly,plotlyOutput,config,layout],
  magrittr[`%>%`],
)
box::use(
  app/logic/import_data
)

#' @export
ui <- function(id){
  ns <- NS(id)
  plotlyOutput(ns("inf_disc"))
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$inf_disc <- renderPlotly({
      plotly::plot_ly(import_data$data_inf_disc, x = ~percentage,
                      type = "bar",
                      y = ~Var1,
                      marker =list(color="#85C2FF"),
                      text = paste(import_data$data_inf_disc$pct1, sep = ""), textposition = 'outside',
                      textfont = list(size = 10), # size is defined here
                      hovertext = paste("Influence of discrimination: ", import_data$data_inf_disc$Var1,
                                        "<br>Number of persons :", import_data$data_inf_disc$Freq1,
                                        "<br>Percentage :",import_data$data_inf_disc$pct1), #) %>%
                      #"<br>Percentage :", data_marsta()$pct1),
                      hoverinfo = 'text', orientation ="h") %>%
        layout(title = "",
               #legend = list(x = 100, y = 0.95, title=list(color= "blue", text='<b> </b>')),
               #uniformtext=list(minsize=10, mode='show'),
               # xaxis = list(title = "<b> </b>", #font = list(size = 0),
               #              # change x-axix size
               #              tickfont = list(size = 11),
               #              # change x-title size
               #              titlefont = list(size = 16), #type="date", tickformat="%Y%B",  tickformat = "%b-%Y",
               #              tickangle= 0, showgrid = FALSE),
               yaxis = list(title = "<b>  </b>",
                            titlefont = list(size = 12),
                            # change x-axix size
                            tickfont = list(size = 12),
                            showgrid = FALSE) #ticksuffix = "%",
        ) %>%
        config(displayModeBar = F,
               scrollZoom = T)
    })
  })
}

library(vcd)
library(ggmosaic)
mosaic( ~ location+gender, data = data,
        highlighting = "gender", highlighting_fill = c("lightblue", "pink"),
        direction = c("v","h","v"))

d <- data %>% #select(location, gender) %>%
  group_by(location, gender) %>%
  summarise(Freq=n())

ggplot(data = d) +
  geom_mosaic(aes(weight=Freq, x=product(gender), fill=location))




