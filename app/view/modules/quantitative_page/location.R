box::use(
  shiny[moduleServer, div,NS, h3, p, uiOutput,
        observeEvent,reactiveValues, renderUI,reactiveVal, renderText,plotOutput,
        renderPlot],
  shiny.fluent[ActionButton.shinyInput,updateActionButton.shinyInput],
  plotly[plotlyOutput, renderPlotly, add_trace, layout, plot_ly, config],
  magrittr[`%>%`],
  dplyr,
  graphics[axis, text],
  RColorBrewer,
)

box::use(
  app/view/components/ui/cards,
  app/logic/import_data,
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  cards$card_ui("Location by gender","",
                div(class = "card_content",
                    h3(class = "subtitle", ""),
                    p(class = "description", ""),
                    # Graph goes here
                    plotOutput(ns("gen_loc"),width="500px", height = 460) #,width="500px"
                )
  )

}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$gen_loc <- renderPlot({
      data_buble <- import_data$data %>% dplyr::select(gender, location)
      data_buble$gender <- as.factor(data_buble$gender)
      #data_buble$location <- as.factor(data_buble$location)
      my.tab <- table(data_buble$gender, data_buble$location)
      rownames(my.tab) <- levels(data_buble$gender)
      plot(
        0, 0,
        pch = "",
        xlim = c(0.5, 2.5),
        ylim = c(0.5, 6.5),
        axes = FALSE,
        xlab = "",
        ylab = ""
      )
      for (i in 1:dim(my.tab)[1]) {
        for (i in 1:dim(my.tab)[1]) {
          graphics::symbols(
            c(1:dim(my.tab)[2]),
            rep(i, dim(my.tab)[2]),
            circle = sqrt(my.tab[i, ] / 7000 / pi),
            add = TRUE,
            inches = FALSE,
            fg = RColorBrewer::brewer.pal(5, "PRGn"),
            bg = RColorBrewer::brewer.pal(5, "PRGn")
          )
        }
      }
      axis(1,
           col = "white",
           col.axis = "black",
           at = c(1, 2),
           label = colnames(my.tab))
      axis(
        2,
        at = c(1:5),
        label = rownames(my.tab),
        las = 1,
        col.axis = "black",
        col = "white"
      )

      # Ajouter les nombres
      for (i in 1:5) {
        text(c(1, 2), rep(i, 2), my.tab[i, ])
      }
    })


  })
}
