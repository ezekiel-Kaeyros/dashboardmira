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

data_age_disc <- data.frame(category_age = rep(import_data$data$age, sapply(import_data$data$newFormOfRacismYes, length)),
                            discrimination = import_data$data$newFormOfRacismYes)
table_age_disc <- with(data_age_disc, table(category_age, discrimination))

order <- c("Unter 18 Jahre ", "18-27 Jahre", "28-40 Jahre", "41-65 Jahre", "Über 65 Jahre ")
table_age_disc <- as.data.frame(table_age_disc) %>%
  mutate(category_age = factor(category_age, levels = order)) %>%
  arrange(category_age)
