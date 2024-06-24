box::use(
  shiny.fluent[Text, fluentPage, reactOutput, renderReact, Dialog, Link,
               DialogFooter, PrimaryButton.shinyInput, DefaultButton.shinyInput,
               Dropdown.shinyInput,DatePicker.shinyInput,Modal,Stack,ActionButton.shinyInput,
               updateDatePicker.shinyInput],
  shiny[div, NS, moduleServer, a, tagList, h2, h5, p, actionButton,h3,
        reactiveVal, observeEvent, radioButtons,showModal,basicPage,reactiveValues,
        modalDialog,modalButton, uiOutput, renderUI,removeModal,fluidRow,column,HTML,bootstrapPage,
        icon, tags,observe,eventReactive,req,reactive],
  htmltools[tagList],
  htmlwidgets,
  shinyjs, utils,
  wordcloud[wordcloud],
  RColorBrewer[brewer.pal],
  plotly[renderPlotly,plotlyOutput,config,layout],
  magrittr[`%>%`], dplyr, stats, utils
)

box::use(
  app/view/components/layouts,
  app/view/components/ui/cards,
  app/logic/import_data,

)

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

    shinyjs::useShinyjs()

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
        div(class = "compare_page",
            div(class="compare_page1",
                div(class= "compare_page2",
                    tagList(
                      #shinyjs::useShinyjs(),
                      h3("Choose to compare"),
                      paste("Click to choose variable"),
                      tags$br(),
                      DefaultButton.shinyInput(ns("showmodal"), text = "Choose",styles = list("background: ##F0FFF0")),
                      reactOutput(ns("modal"))
                      #reactOutput(ns("modal")),
                      #reactOutput(ns("page_ok"))
                    )
                )
            )
        )
      } else{
        shiny::h3("Error 500 - Internal Server Error")
      }
    })

    modalVisible <- reactiveVal(FALSE)

    output$modal <- renderReact({
      Modal(isOpen = modalVisible(),
            Stack(tokens = list(padding = "15px", childrenGap = "10px"),
                  div(style = list(display = "flex"),
                      Text("Choose inputs to compare data", variant = "large"),
                      div(style = list(flexGrow = 1))
                  ),
                  div(
                    h5("Choose variable"),
                    Dropdown.shinyInput(ns("choice_data"), value = import_data$options_var[[1]]$key, options = import_data$options_var),
                    tags$br(),
                    div(class="periods",
                        h5("First period"),
                        h5(class = "period_right", "Second period")
                        # style = "display:flex; justify-content:space-around; padding:0px 10px;",
                        # h5("First period"),
                        # h5("Second period")
                    ),
                    div(
                      style = "display: flex; flex-direction: row; justify-content: space-between; column-gap:30px;",
                      DatePicker.shinyInput(ns("date1"), value = NULL, placeholder = "Start date"),
                      DatePicker.shinyInput(ns("date2"), value = NULL, placeholder = "Start date")
                    ),
                    tags$br(),
                    div(
                      style = "display: flex; flex-direction: row; justify-content: space-between;
                      column-gap:8px;",
                      DatePicker.shinyInput(ns("date3"), value = NULL, placeholder = "End date"),
                      DatePicker.shinyInput(ns("date4"), value = NULL, placeholder = "End date")
                    ),
                    tags$br(),
                    tags$br(),
                    div(
                      style = "display: flex; justify-content: flex-end;",
                      div(
                        style = "margin-right: 10px;",
                        DefaultButton.shinyInput(ns("cancel"), "Cancel")
                      ),
                      div(
                        shinyjs::useShinyjs(),
                        DefaultButton.shinyInput(ns("okay1"), "OK")
                      )
                    )
                  )
            )
      )

    })

    observeEvent(input$cancel, modalVisible(FALSE))
    observeEvent(input$showmodal, modalVisible(TRUE))

    observeEvent(input$okay1, {
      modalVisible(FALSE)
      shinyjs::runjs(paste("window.location.href = '#!/more_compare?token=", current_token(), "';", sep = ""))
    })

    date1_react <-reactiveVal(NULL)
    observe({
      date1_react(input$date1)
      saveRDS(date1_react(), paste("app/data/","date1",".rds", sep = ""))
    })


    date2_react <-reactiveVal(NULL)
    observe({
      date2_react(input$date2)
      saveRDS(date2_react(), paste("app/data/","date2",".rds", sep = ""))
    })


    date3_react <-reactiveVal(NULL)
    observe({
      date3_react(input$date3)
      saveRDS(date3_react(), paste("app/data/","date3",".rds", sep = ""))
    })

    date4_react <-reactiveVal(NULL)
    observe({
      date4_react(input$date4)
      saveRDS(date4_react(), paste("app/data/","date4",".rds", sep = ""))
    })

    choice_data_react <-reactiveVal(NULL)
    observe({
      choice_data_react(input$choice_data)
      saveRDS(choice_data_react(), paste("app/data/","choice_data",".rds", sep = ""))
    })

    data <- reactive({
      req(input$choice_data, input$date1, input$date3)
      date1_t <- shiny::reactiveFileReader(1000, NULL, "app/data/date1.rds", readRDS)
      date3_t <- shiny::reactiveFileReader(1000, NULL, "app/data/date3.rds", readRDS)
      choice_data <- shiny::reactiveFileReader(1000, NULL, "app/data/choice_data.rds", readRDS)
      start_date <- as.Date(date1_t())
      end_date <- as.Date(date3_t())
      data <- import_data$data %>%
        dplyr::filter(updatedAt >= start_date & updatedAt <= end_date) %>%
        dplyr::select(choice_data())
      data <- as.data.frame(table(unlist(data)))
      data <- data %>%
        dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
                      pct1 = paste0(percentage, "%"))
      if (choice_data() == "influence of the discrimination") {
        data$Var1 <- gsub("Text \\(free entry\\)",
                          "Others", data$Var1)
        data
      } else if (choice_data() == "another discriminations") {
        data$Var1 <- gsub("Other form, namely",
                          "Others", data$Var1)
        data
      } else {
        data
      }

    }) #,ignoreNULL = TRUE,ignoreInit = TRUE
    observe({
      data()
      saveRDS(data(), paste("app/data/","data",".rds", sep = ""))


    })

    data2 <- reactive({
      req(input$choice_data, input$date2, input$date4)
      date2_t <- shiny::reactiveFileReader(1000, NULL, "app/data/date2.rds", readRDS)
      date4_t <- shiny::reactiveFileReader(1000, NULL, "app/data/date4.rds", readRDS)
      choice_data <- shiny::reactiveFileReader(1000, NULL, "app/data/choice_data.rds", readRDS)
      start_date <- as.Date(date2_t())
      end_date <- as.Date(date4_t())
      data <- import_data$data %>%
        dplyr::filter(updatedAt >= start_date & updatedAt <= end_date) %>%
        dplyr::select(choice_data())
      data <- as.data.frame(table(unlist(data)))
      data <- data %>%
        dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
                      pct1 = paste0(percentage, "%"))
      if (choice_data() == "influence of the discrimination") {
        data$Var1 <- gsub("Text \\(free entry\\)",
                          "Others", data$Var1)
        data
      } else if (choice_data() == "another discriminations") {
        data$Var1 <- gsub("Other form, namely",
                          "Others", data$Var1)
        data
      } else {
        data
      }

    })

    observe({
      data2()
      saveRDS(data2(), paste("app/data/","data2",".rds", sep = ""))
    })

    observe({
      if (!is.null(input$date1) && !is.null(input$date2) && !is.null(input$date3) && !is.null(input$date4)) {
        shinyjs::enable("okay1")
      } else {
        shinyjs::disable("okay1")
      }
    })



  })
}

