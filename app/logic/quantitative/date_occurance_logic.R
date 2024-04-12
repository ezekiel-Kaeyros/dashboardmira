box::use(
  dplyr[mutate, if_else, summarise, group_by, select, n, rename,inner_join, filter],
  magrittr[`%>%`],stats
)

box::use(
  app/logic/import_data
)

data_temp <- function(filter){
  if(filter=="Alle"){
    data <- import_data$data
  } else {
    data <- subset(import_data$data, identity==filter)
  }
  data_temp <- data %>%
    dplyr::select(dateState, endDate, createdAt) %>%
    dplyr::mutate(
      dateState = as.Date(dateState, format = "%d.%m.%Y"),
      endDate = as.Date(endDate, format = "%d.%m.%Y"),
      createdAt = as.Date(createdAt, format = "%d.%m.%Y")
    ) %>%
    dplyr::mutate(
      temporal_distance = if_else(is.na(endDate), as.numeric(difftime(createdAt, dateState, units = "days")),
                                  as.numeric(difftime(createdAt, endDate, units = "days")))
    )

  data_months <- data_temp %>%
    dplyr::mutate(
      temporal_class = cut(temporal_distance,breaks = c(-Inf,90,180,270,Inf),
                           labels = c("(0-3)Monate","(4-6)Monate","(7-9)Monate","(10+)Monate"))
    ) %>%
    dplyr::select(temporal_class) %>%
    dplyr::group_by(temporal_class) %>%
    dplyr::summarise(total = n()) %>%
    dplyr::mutate(percentage=round(100*(total/sum(total)),2),
           pct1=paste0(percentage,"%"))
  data_months <- stats::na.omit(data_months)

  return(data_months)
}


data_temp1 <- function(filter){
  if (filter=='Alle'){
    data <- import_data$data
  } else {
    data <- subset(import_data$data, identity==filter)
  }
  data_temp <- data %>%
    select(dateState, endDate, createdAt) %>%
    dplyr::mutate(
      dateState = as.Date(dateState, format = "%d.%m.%Y"),
      endDate = as.Date(endDate, format = "%d.%m.%Y"),
      createdAt = as.Date(createdAt, format = "%d.%m.%Y")
    ) %>%
    dplyr::mutate(
      temporal_distance = if_else(is.na(endDate), as.numeric(difftime(createdAt, dateState, units = "days")),
                                  as.numeric(difftime(createdAt, endDate, units = "days")))
    )

  data_months <- data_temp %>%
    dplyr::mutate(
      temporal_class = cut(temporal_distance,breaks = c(-Inf,90,180,270,Inf),
                           labels = c("(0-3)Monate","(4-6)Monate","(7-9)Monate","(10+)Monate"))
    )
  return(data_months)
}
