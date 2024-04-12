box::use(
  dplyr,
  magrittr[`%>%`]

)

box::use(
  app/logic/import_data
)

data_gen <- function(data) {
  data_gen <- as.data.frame(table(data$gender))
  data_gen <- data_gen %>%
    dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
                  pct1 = paste0(percentage, "%"))
  return(data_gen)
}
