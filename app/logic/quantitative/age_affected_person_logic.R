box::use(
  dplyr[mutate], magrittr[`%>%`],
)

box::use(
  app/logic/import_data
)


data_age <- function(filter){
  if(filter=="Alle"){
    data_person_age <- import_data$data
    data_age <- as.data.frame(table(data_person_age$age)) %>%
      dplyr::mutate(Var1 = factor(Var1, levels = c("Unter 18 Jahre ", "18-27 Jahre", "28-40 Jahre", "41-65 Jahre", "Über 65 Jahre ")))
    data_age <- data_age %>%
      dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
                    pct1 = paste0(percentage, "%"))
  }
  else if (filter=="Organisation/Institution"){
    data_person_age <- import_data$data
    data_age <- as.data.frame(table(data_person_age$numberOfEmployees)) %>%
      dplyr::mutate(Var1 = factor(Var1, levels = c("Unter 10", "10 - 49", "50 - 250", "Über 250")))
    data_age <- data_age %>%
      dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
                    pct1 = paste0(percentage, "%"))
  }
  else {
    data_person_age <- subset(import_data$data, identity==filter, select = c(age))
    data_age <- as.data.frame(table(data_person_age$age)) %>%
      dplyr::mutate(Var1 = factor(Var1, levels = c("Unter 18 Jahre ", "18-27 Jahre", "28-40 Jahre", "41-65 Jahre", "Über 65 Jahre ")))
    data_age <- data_age %>%
      dplyr::mutate(percentage = round(100*(Freq/sum(Freq)),2),
                    pct1 = paste0(percentage, "%"))
  }

  return(data_age)
}

data_age1 <- function(filter){
  if (filter=="Alle"){
    data <- import_data$data
    return(data)
  } else {
    data <- subset(import_data$data, identity==filter)
    return(data)
  }
}
