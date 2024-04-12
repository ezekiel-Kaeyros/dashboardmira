box::use(
  dplyr[mutate, if_else, first, summarise, summarize,group_by, select, n, rename,inner_join, right_join,left_join, filter, ungroup],
  magrittr[`%>%`],
  leaflet[colorFactor,addProviderTiles,addPolygons,
          addLegend, leaflet, setView,removeScaleBar,labelOptions,highlightOptions],
  stats[quantile, aggregate, as.formula]
)

box::use(
  app/logic/import_data
)



sum_by_province <- function(filter){
  if(filter=="Alle"){
    data_loc <- import_data$data
  } else {
    data_loc <- subset(import_data$data, identity==filter)
  }


  data_location <- as.data.frame(table(data_loc$location)) %>%
    dplyr::rename(location = Var1, Value = Freq)

  # Filtrer et sélectionner les données uniquement pour l'État de Nordrhein-Westfalen
  data1<- import_data$Allemagne %>%
    dplyr::filter(NAME_1 == "Nordrhein-Westfalen") %>%
    dplyr::select(NAME_1, Province, location) %>%
    dplyr::left_join(data_location, by = "location") %>%
    dplyr::mutate(Value = if_else(is.na(Value), 0, Value))

  # Agréger les données par province et calculer la somme des valeurs
  sum_by_province <- aggregate(data1["Value"], by = list(Province = data1$Province), FUN = sum)
  interval1 <- c(0, 20, 40, Inf)
  categories1 <- c(paste("unter",20), paste(20,"-",40, sep=""),paste("Über", 40))
  sum_by_province$cat <- cut(sum_by_province$Value, breaks = interval1, labels = categories1, right = FALSE)

  return(sum_by_province)
}
