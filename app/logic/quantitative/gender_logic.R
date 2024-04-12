box::use(
  dplyr, magrittr[`%>%`],
)

box::use(
  app/logic/import_data
)

data_gender <- function(filter){
  if (filter=="Alle"){
    data <- import_data$data
    data_gen <- as.data.frame(table(unlist(data$gender)))
    data_gen <- data_gen %>%
      dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
                    pct1 = paste0(percentage, "%"))
  }
  if(filter=="Organisation/Institution"){
    data_gen <- as.data.frame(table(unlist(import_data$data$organizationType)))
    data_gen <- data_gen[data_gen$Var1 != "FALSE", ]
    data_gen$Var1 <- as.character(data_gen$Var1)
    data_gen$Var1[data_gen$Var1 == "Anderes, und zwar:"] <- "Anderes"
    data_gen$Var1[data_gen$Var1 == "Nichtregierungsorganisation (NGO)"] <- "Nichtregierungsorganisation"
    data_gen <- data_gen %>%
      dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
                    pct1 = paste0(percentage, "%"))
  } else {
    data <- subset(import_data$data, identity==filter)
    data_gen <- as.data.frame(table(unlist(import_data$data$gender)))
    data_gen <- data_gen %>%
      dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
                    pct1 = paste0(percentage, "%"))
  }

  #data_gen <- subset(data_gen, Var1!="FALSE")
  return(data_gen)
}

