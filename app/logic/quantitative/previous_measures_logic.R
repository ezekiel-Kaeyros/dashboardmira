box::use(
  dplyr, magrittr[`%>%`],
)

box::use(
  app/logic/import_data
)

previous_measures <- function(filter){
  if (filter=="Alle"){
    data <- import_data$data
  } else {
    data <- subset(import_data$data, identity==filter)
  }
  previous_measures <-as.data.frame(table(data$haveYouTakenMeasures))
  previous_measures <- previous_measures %>%
    dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
                  pct1 = paste0(percentage, "%"))
}
