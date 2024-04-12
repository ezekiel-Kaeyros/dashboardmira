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

data_age_inf <- data.frame(category_age = rep(import_data$data$age, sapply(import_data$data$racismManifestation, length)),
                           influence = import_data$data$racismManifestation)
table_age_inf <- with(data_age_inf, table(category_age, influence))

order <- c("Unter 18 Jahre ", "18-27 Jahre", "28-40 Jahre", "41-65 Jahre", "Über 65 Jahre ")
table_age_inf <- as.data.frame(table_age_inf) %>%
  mutate(category_age = factor(category_age, levels = order)) %>%
  arrange(category_age)
