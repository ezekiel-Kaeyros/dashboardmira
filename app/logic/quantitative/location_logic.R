box::use(
  dplyr, magrittr[`%>%`],
)

box::use(
  app/logic/import_data
)

data_onreal <- function(filter){
  if (filter=='Alle'){
    data <- import_data$data
  } else {
    data <- subset(import_data$data, identity==filter)
  }
  data_onreal <- as.data.frame(table(data$happenedOnline))
  data_onreal <- data_onreal %>%
    dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
                  pct1 = paste0(percentage, "%"))

  return(data_onreal)
}

data_onreal1 <- function(filter){
  if (filter=='Alle'){
    data <- import_data$data
  } else {
    data <- subset(import_data$data, identity==filter)
  }
  return(data)
}

