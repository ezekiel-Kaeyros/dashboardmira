box::use(
  shiny.fluent[Text, fluentPage, reactOutput, renderReact, Dialog, JS,
               DialogFooter, PrimaryButton.shinyInput, DefaultButton.shinyInput,
               Dropdown.shinyInput,DatePicker.shinyInput,Modal,Stack,ActionButton.shinyInput],
  shiny[div, tags, NS, moduleServer, tagList, h2, h5, p, actionButton,h3,
        reactiveVal, observeEvent, radioButtons,showModal,basicPage,reactiveValues,
        modalDialog,modalButton, uiOutput, renderUI,removeModal,fluidRow,column,reactive,
        tableOutput, renderTable, HTML],
  htmltools[tagList],lubridate,
  magrittr[`%>%`],dplyr,htmltools,
  reactable[reactableOutput, renderReactable, reactable], plotly[config,layout]
)

box::use(
  app/view/components/layouts,
  app/view/components/ui/cards,
  app/logic/import_data,
  app/view/compare_page,
  app/view/components/ui/cards

)

date1 <- shiny::reactiveFileReader(1000, NULL, "app/data/date1.rds", readRDS)
date2 <- shiny::reactiveFileReader(1000, NULL, "app/data/date2.rds", readRDS)
date3 <- shiny::reactiveFileReader(1000, NULL, "app/data/date3.rds", readRDS)
date4 <- shiny::reactiveFileReader(1000, NULL, "app/data/date4.rds", readRDS)
choice_data <- shiny::reactiveFileReader(1000, NULL, "app/data/choice_data.rds", readRDS)
data <- shiny::reactiveFileReader(1000, NULL, "app/data/data.rds", readRDS)
data2 <- shiny::reactiveFileReader(1000, NULL, "app/data/data2.rds", readRDS)


#' @export
ui <- function(id){
  ns <- NS(id)
  shinyjs::useShinyjs()
  shiny::uiOutput(ns("ui"))

}


#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    current_token <- shiny::reactive({
      token <- shiny.router::get_query_param("token", session)
      if(is.null(token)){
        token <- "404"
      }else{
        token <- token
      }
      token
    })
    output$ui <- shiny::renderUI({
      ############# Decode JWT
      token_json_data <- jose::jwt_decode_hmac(current_token(), secret = import_data$key)

      ############ Detect validity time of token
      converted_time <- as.POSIXct(token_json_data$exp, origin="1970-01-01", tz="Africa/Lagos")

      if(token_json_data$email %in% import_data$login_data$email & token_json_data$role == import_data$role & converted_time > Sys.time()){
        Stack(
          div(shiny.fluent::DefaultButton.shinyInput("refresh", "Daten aktualisieren",
                                                     iconProps = list(iconName = "Refresh"),
                                                     style = "float: left; height: 48px; top: 1px; margin: 5px;
                                           background-color: #2B8049; color: #fff; border-radius: 12px;")),
          tokens = list(childrenGap = 10), horizontal = TRUE,
          cards$makeCard(div(class="text1",""
                             #Text("From ",shiny::textOutput(ns("date1")), "to ", shiny::textOutput(ns("date3")))
          ),
          #Text("From ",shiny::textOutput(ns("date1")), "to ", shiny::textOutput(ns("date3"))),
          #paste("From ",shiny::textOutput(ns("date1")), "to ", shiny::textOutput(ns("date3"))),

          div(style="max-height: 500px;", uiOutput(ns("table1")))),
          div(class="margin_l"),
          div(class="more_compare_page"),
          div(class="margin_r"),
          cards$makeCard(div(class="text1",""
                             #Text("From ",shiny::textOutput(ns("date1")), "to ", shiny::textOutput(ns("date3")))
          ),
          #Text("From ",shiny::textOutput(ns("date1")), "to ", shiny::textOutput(ns("date3"))),
          #paste("From ",shiny::textOutput(ns("date1")), "to ", shiny::textOutput(ns("date3"))),
          div(style="max-height: 500px;", uiOutput(ns("table2")))),

          div(
            shiny.fluent::Link(href=paste("#!/compare?token=", current_token(), sep = ""), "Zurückgehen",
                               style = "float: right; background-color: #fff; text-decoration:none; padding: 1em 1.5em;
                            text-align: center; border-color: #000; border-radius: 12px;
                            border: 1px solid black; color: #000; font-weight: bold;"),
          )
        )
      } else{
        shiny::h3("Error 500 - Internal Server Error")
      }
    })

    output$date1 <- shiny::renderText({
      date <- as.Date(date1()) + 1
      format(lubridate::ymd(date),"%d %B %Y")
    })

    output$date1_t <- shiny::renderText({
      date1()
    })

    output$date2 <- shiny::renderText({
      date <- as.Date(date2()) + 1
      format(lubridate::ymd(date),"%d %B %Y")
    })

    output$date2_t <- shiny::renderText({
      date2()
    })

    output$date3 <- shiny::renderText({
      date <- as.Date(date3()) + 1
      format(lubridate::ymd(date),"%d %B %Y")
    })

    output$date3_t <- shiny::renderText({
      date3()
    })

    output$date4 <- shiny::renderText({
      date <- as.Date(date4()) + 1
      format(lubridate::ymd(date),"%d %B %Y")
    })

    output$date4_t <- shiny::renderText({
      date4()
    })


    output$choice_data <- shiny::renderText({
      choice_data()
    })

    output$table1 <- renderUI ({
      if (nrow(data())==0) {
        shiny::textOutput(ns("text1"))
      } else {
        plotly::plotlyOutput(ns("plot1"))
      }
    })

    output$text1 <- shiny::renderText({
      paste("There's no data for this period")
    })

    output$plot1 <- plotly::renderPlotly({
      plotly::plot_ly(data(), x = ~Var1,
                      type = "bar",
                      y = ~percentage,
                      marker =list(color="#85C2FF"),
                      text = paste(data()$pct1, sep = ""), textposition = 'outside',
                      textfont = list(size = 10), # size is defined here
                      hovertext = paste(#"Person affected: ", import_data$data_personaf$Var1,
                        "Number of persons :", data()$Freq,
                        "<br>Percentage :",data()$pct1), #) %>%
                      hoverinfo = 'text') %>%
        layout(title = paste("From",format(lubridate::ymd(as.Date(date1())),"%d %B %Y") , "to", format(lubridate::ymd(as.Date(date3())),"%d %B %Y")),
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
               # margin=list(l = 100, r = 50,
               #             b = 0, t = 100,
               #             pad = 0)
        ) %>%
        config(displayModeBar = F,
               scrollZoom = T)

    })

    output$table2 <- renderUI ({
      if (nrow(data2())==0) {
        shiny::textOutput(ns("text2"))
      } else {
        plotly::plotlyOutput(ns("plot2"))
      }
    })

    output$text2 <- shiny::renderText({
      paste("There's no data for this period")
    })

    output$plot2 <- plotly::renderPlotly({
      plotly::plot_ly(data2(), x = ~Var1,
                      type = "bar",
                      y = ~percentage,
                      marker =list(color="#85C2FF"),
                      text = paste(data2()$pct1, sep = ""), textposition = 'outside',
                      textfont = list(size = 10), # size is defined here
                      hovertext = paste(#"Person affected: ", import_data$data_personaf$Var1,
                        "Number of persons :", data2()$Freq,
                        "<br>Percentage :",data2()$pct1), #) %>%
                      hoverinfo = 'text') %>%
        layout(title = paste("From",format(lubridate::ymd(as.Date(date2())),"%d %B %Y") , "to", format(lubridate::ymd(as.Date(date4())),"%d %B %Y"))  ,
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
               # margin=list(l = 100, r = 50,
               #             b = 0, t = 100,
               #             pad = 0)
        ) %>%
        config(displayModeBar = F,
               scrollZoom = T)
    })


  })
}
