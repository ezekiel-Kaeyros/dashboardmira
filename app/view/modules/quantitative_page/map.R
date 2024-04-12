box::use(
  shiny[NS,moduleServer,div, h1, p],
  leaflet[colorFactor,leafletOutput,renderLeaflet,addProviderTiles,addPolygons,
          addLegend, leaflet, setView,removeScaleBar,labelOptions,highlightOptions],
  magrittr[`%>%`]
)

box::use(
  app/view/components/ui/cards,
  app/logic/import_data,
  app/logic/quantitative/map_logic,
  app/view/components/layouts
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  #leafletOutput("map_plot")
  cards$card_ui("Karte der betroffenen Personen nach Bundesländern",
                "",
                div(class = "card_content",
                    h1(class = "subtitle", ""),
                    p(class = "description", ""),
                    # Graph goes here
                    leafletOutput(ns("map_plot"), width="800px", height=650) #450
                )
  )

}

#' @export
server <- function(id, filter) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$map_plot <- renderLeaflet({
      sum_by_province <- map_logic$sum_by_province(filter)

      labels <- sprintf(
        "<strong>%s</strong><br/>%g betroffene Personen </sup>",
        sum_by_province$Province, sum_by_province$Value
      ) %>% lapply(htmltools::HTML)

      pal=colorFactor(palette=c("#FCD18C", "#CFAACB", "#93C89A"), domain=sum_by_province$cat)
      map <- leaflet() %>%
        addProviderTiles("CartoDB.Positron") # Vous pouvez choisir un autre style de tuile si vous le souhaitez

      map <- leaflet(data = sum_by_province) %>%
        addProviderTiles("CartoDB.VoyagerLabelsUnder") %>%
        addPolygons(
          fillColor =  ~colorFactor(palette=c("#FCD18C", "#CFAACB", "#93C89A"), domain= sum_by_province$cat)(sum_by_province$cat),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlightOptions = highlightOptions(
            weight = 2,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.4,
            bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(
                           style = list("font-weight" = "normal", padding = "3px 8px"),
                           textsize = "15px",
                           direction = "auto")
        )%>%
        addLegend(position = "bottomright",
                  pal=pal,
                  values=~cat, title="Anzahl der betroffenen Personen"

        ) %>%
        setView(lng=7.661594, lat=51.433237, zoom=7.5)

      map
    })

  })
}
