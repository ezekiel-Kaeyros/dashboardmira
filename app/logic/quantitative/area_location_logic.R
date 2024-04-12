box::use(
  dplyr, magrittr[`%>%`],
)

box::use(
  app/logic/import_data
)

area_location <- function(filter){

  extract_elements <- function(x) {
    elements <- strsplit(gsub("[c()]", "", x), ", ")[[1]]
    trim <- function(x) gsub("^\\s+|\\s+$|\"", "", x)
    return(trim(elements))
  }
  if(filter=="Alle"){
    data <- import_data$data
    data <- subset(data, !areaLocation == "character(0)")
    evaluated_vectors <- lapply(data$areaLocation, extract_elements)
    area_location <- as.data.frame(table(unlist(evaluated_vectors)))
    area_location <- area_location %>%
      dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
                    pct1 = paste0(percentage, "%"))
    } else if(filter=="Organisation/Institution") {
    data <- import_data$data
    area_location <- as.data.frame(table(unlist(data$typeOfRacism)))
    area_location <- area_location %>%
      dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
                    pct1 = paste0(percentage, "%"))
    } else {
      data <- subset(import_data$data, identity== filter, select = c(areaLocation))
      evaluated_vectors <- lapply(data$areaLocation, extract_elements)
      area_location <- as.data.frame(table(unlist(evaluated_vectors)))
      area_location <- area_location %>%
        dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
                      pct1 = paste0(percentage, "%"))
  }


  return(area_location)
}

