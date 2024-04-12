box::use(
  rjson[fromJSON],
  jsonlite[fromJSON],
  magrittr[`%>%`],
  dplyr[filter, group_by, count,rename],
  stats[aggregate]
)

box::use(
  app/logic/import_data
)

l1<-list()
for (i in 1:16){
  v <- import_data$geojson3$features[[i]]$properties$NAME_1
  l1[i] <-v
  i<- i+1
}

d1 <- data.frame(city = unlist(l1))
d1$n <-0

  states <- import_data$data %>%
    filter(place_discrimination$city !="NA") %>%
    group_by(place_discrimination$city) %>%
    count(place_discrimination$city) %>%
    rename(city = `place_discrimination$city`)
  states$city[states$city == "Munich"] <- "München"
  states$city[states$city == "Frankfurt"] <- "Frankfurt am Main"

  states$city <- ifelse(states$city %in% c("Cologne", "Dortmund", "Düsseldorf", "Essen"), "Nordrhein-Westfalen", states$city)
  states$city <- ifelse(states$city %in% c("Frankfurt am Main"), "Hessen", states$city)
  states$city <- ifelse(states$city %in% c("Leipzig"), "Sachsen", states$city)
  states$city <- ifelse(states$city %in% c("Stuttgart"), "Baden-Württemberg", states$city)
  state_sum <-aggregate(n ~ city, data = states, FUN = sum)

  states_delete <- c(state_sum$city)
  states1 <- d1[!(d1$city %in% states_delete), ]
  states_f <- rbind(state_sum, states1)

g <- list(
  fitbounds = "locations",
  visible = FALSE)

