box::use(
  dplyr, magrittr[`%>%`],
)

box::use(
  app/logic/import_data
)


data_personaf <- function(filter){
  if(filter=="Alle"){
    data_person_aff <- import_data$data
  } else {
    data_person_aff <- subset(import_data$data, identity==filter)
  }

  data_personaf <- as.data.frame(table(data_person_aff$identity))
  data_personaf <- data_personaf %>%
    dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
                  pct1 = paste0(percentage, "%"))

  return(data_personaf)
}

person_af_sub <- function(filter){
  if(filter=="Alle"){
    data <- import_data$data
  } else {
    data <- subset(import_data$data, identity==filter)
  }
  return (data)
}
#
