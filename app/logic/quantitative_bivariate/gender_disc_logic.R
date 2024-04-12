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

data_gender_disc <- data.frame(gender = rep(import_data$data$gender, sapply(import_data$data$newFormOfRacismYes, length)),
                               discrimination = import_data$data$newFormOfRacismYes)
table_gender_disc <- with(data_gender_disc, table(gender, discrimination))
table_gender_disc <- as.data.frame(table_gender_disc)

