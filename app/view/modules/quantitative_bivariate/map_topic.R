box::use(
  shiny[NS,moduleServer,div, h1, p],
  leaflet[colorFactor,leafletOutput,renderLeaflet,addProviderTiles,addPolygons,
          addLegend, leaflet, setView,removeScaleBar,labelOptions,highlightOptions],
  magrittr[`%>%`],
  sf[st_drop_geometry],
  stats[quantile, aggregate],
  leaflet.minicharts[addMinicharts],
)

box::use(
  app/view/components/ui/cards,
  app/logic/import_data
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  #leafletOutput("map_plot")
  cards$card_ui("Karte der Themen nach Bundesländern",
                "",
                div(class = "card_content",
                    h1(class = "subtitle", ""),
                    p(class = "description", ""),
                    # Graph goes here
                    leafletOutput(ns("map_plot")) #450
                )
  )

}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$map_plot <- renderLeaflet({

      # Vérifier les colonnes disponibles
      colonnes_disponibles <- colnames(import_data$Allemagne2)[grepl("^topic \\d+$", colnames(import_data$Allemagne2))]

      # Créer les étiquettes en fonction des colonnes disponibles
      labels_topic <- lapply(1:nrow(import_data$Allemagne2), function(i) {
        province <- import_data$Allemagne2$Province[i]
        topics <- sapply(colonnes_disponibles, function(col) import_data$Allemagne2[[col]][i])
        labels <- sprintf(
          "<strong>%s</strong>
    <br/>%s",
          province,
          paste(paste(colonnes_disponibles, topics, sep = ": "), collapse = "<br/>")
        )
        htmltools::HTML(labels)
      })

      # View map
      topic_map <- leaflet(width = "100%", height = "400px") %>%
        addProviderTiles("CartoDB.Positron")%>%
        addPolygons(
          data = import_data$Allemagne2,
          weight = 2,
          opacity = 1,
          color = "#666",
          fillOpacity = 0.1 ,
          label = labels_topic,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")
        )%>%
        addMinicharts(
          lng = import_data$Allemagne2$longitude,
          lat = import_data$Allemagne2$latitude,
          type = "bar",
          chartdata = st_drop_geometry(import_data$Allemagne2[colonnes_disponibles]),
          width = 25, height = 25,
        ) %>%
        setView(lng=7.661594, lat=51.433237, zoom=7.4)

      topic_map

    })

  })
}
