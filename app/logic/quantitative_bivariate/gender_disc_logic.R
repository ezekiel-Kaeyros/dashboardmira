box::use (
  dplyr[mutate, arrange, if_else, first, summarise, summarize,group_by, select, n, rename,inner_join, right_join,left_join, filter, ungroup],
  magrittr[`%>%`],
  stats[aggregate],
  tm[stopwords, Corpus, VectorSource, tm_map, content_transformer, removeWords, removePunctuation,
     removeNumbers, stemDocument, stripWhitespace, DocumentTermMatrix],
  sf[st_sfc, st_centroid, st_coordinates, st_drop_geometry],
  topicmodels[LDA, posterior],
)

box::use(
  app/logic/import_data
)

# Filtrer les éléments NULL ou vides
#filtered_data <- import_data$data$newFormOfRacismYes[sapply(import_data$data$newFormOfRacismYes, length) > 0]
# filtered_data <- import_data$data$newFormOfRacismYes[!sapply(import_data$data$newFormOfRacismYes, function(x) is.null(x) || length(x) == 0 || all(is.na(x)))]
#
# # Construire le data.frame avec les éléments filtrés
# data_gender_disc <- data.frame(gender = rep(import_data$data$gender, sapply(filtered_data, length)),
#                                discrimination = unlist(filtered_data))

data <-import_data$data
all <- expand.grid(gender = unlist(data$gender), discrimination = unlist(data$newFormOfRacismYes))
table_gender_disc <- as.data.frame(table(all))

# data_gender_disc <- data.frame(gender = rep(import_data$data$gender, sapply(import_data$data$newFormOfRacismYes, length)),
#                                discrimination = import_data$data$newFormOfRacismYes)

# table_gender_disc <- with(data_gender_disc, table(gender, discrimination))
# table_gender_disc <- as.data.frame(table_gender_disc)

