# box::use(
#   dplyr[n], magrittr[`%>%`],
#   leaflet[colorFactor,addProviderTiles,addPolygons,
#           addLegend, leaflet, setView,removeScaleBar,labelOptions,highlightOptions],
#   tm[stopwords, Corpus, VectorSource, tm_map, content_transformer, removeWords, removePunctuation,
#      removeNumbers, stemDocument, stripWhitespace, DocumentTermMatrix],
#   sf[st_sfc, st_centroid, st_coordinates],
#   leaflet.minicharts[addMinicharts]
# )
# 
# box::use(
#   app/logic/import_data
# )
# 
# data <- import_data$data
# data1 <- import_data$data1
# 
# textdata <- data$description
# ################## Data to generate map topic ##############
# german_stopwords <- stopwords("german")
# # create corpus object
# corpus <- Corpus(VectorSource(textdata))
# # preprocessing chain
# processedCorpus <- tm_map(corpus, content_transformer(tolower))
# processedCorpus <- tm_map(processedCorpus, removeWords, german_stopwords)
# processedCorpus <- tm_map(processedCorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
# processedCorpus <- tm_map(processedCorpus, removeNumbers)
# processedCorpus <- tm_map(processedCorpus, stemDocument, language = "german")
# processedCorpus <- tm_map(processedCorpus, stripWhitespace)
# 
# # compute document term matrix with terms >= minimumFrequency
# minimumFrequency <- 3
# DTM <- DocumentTermMatrix(processedCorpus, control = list(bounds = list(global = c(minimumFrequency,Inf))))
# 
# # set.seed(61)
# # K <- 5
# # topicModel <- LDA(DTM, K, method="Gibbs", control = list(iter = 500, verbose = 25))
# # lda_fit <- topicModel
# 
# file_save_lda_model <- paste(import_data$path_data, "/", "lda_model.rds", sep="")
# # Sauvegarde du modèle au format rds
# # saveRDS(lda_fit, file = file_save_lda_model)
# # Charger le modèle LDA à partir du fichier RDS
# lda_fit <- readRDS(file_save_lda_model)
# #
# # prediction <- c()
# # for (i in 1: length(data$description)){
# #   text_to_categorize <- data$description[i]
# #   corpus_to_categorize <- Corpus(VectorSource(text_to_categorize))
# #   text_to_categorize <- tm_map(corpus_to_categorize, content_transformer(tolower))
# #   text_to_categorize <- tm_map(text_to_categorize, removeWords, german_stopwords)
# #   text_to_categorize <- tm_map(text_to_categorize, removePunctuation, preserve_intra_word_dashes = TRUE)
# #   text_to_categorize <- tm_map(text_to_categorize, removeNumbers)
# #   text_to_categorize <- tm_map(text_to_categorize, stemDocument, language = "german")
# #   text_to_categorize <- tm_map(text_to_categorize, stripWhitespace)
# #
# # #   # Créer une Document-Term Matrix (DTM) pour le texte
# #   dtm_to_categorize <- DocumentTermMatrix(corpus_to_categorize)
# #
# #   test.topics <- posterior(lda_fit,dtm_to_categorize)
# #   index_max_probability <- which.max(test.topics$topics)
# #   prediction <- c(prediction,index_max_probability)
# # }
# 
# file_save_prediction <- paste(import_data$path_data, "/", "prediction.rds", sep="")
# #saveRDS(prediction, file_save_prediction)
# prediction <- readRDS(file_save_prediction)
# data$prediction <- prediction
# data$prediction[data$prediction==1] <- "topic 1"
# data$prediction[data$prediction==2] <- "topic 2"
# data$prediction[data$prediction==3] <- "topic 3"
# data$prediction[data$prediction==4] <- "topic 4"
# data$prediction[data$prediction==5] <- "topic 5"
# 
# # ajout d'une colonne province sur medar avec des valeurs aleatoire des regions du Nordrhein-Westfalen
# # Générer un échantillon aléatoire des indices de data1$Province
# # sample_Province_indices <- sample(length(data1$Province), length(data$description), replace = TRUE)
# # data$Province <- data1$Province[sample_Province_indices]
# data_topic <- data[stats::complete.cases(data$place), ]
# data_topic$Province <- data_topic$place
# 
# # Création d'un dataframe des fréquences des sujets par province et jointure avec data1
# province_topics <- as.data.frame.matrix(table(data_topic$Province, data_topic$prediction))
# 
# # Réinitialiser les noms de lignes
# province_topics$Province <- rownames(province_topics)
# rownames(province_topics) <- NULL
# 
# #innner_join entre province_topics et data1
# province_topics_data1 <-  data1 %>%
#   dplyr::inner_join(province_topics, by = "Province")
# 
# latitude <- c()
# longitude <- c()
# for (i in 1:nrow(province_topics_data1)) {
#   # Récupérer les coordonnées du centre du polygone
#   geometry_sf <- st_sfc(province_topics_data1[i,]$geometry)
#   centroid <- st_centroid(geometry_sf)
#   latitude <- c(latitude, st_coordinates(centroid)[, "Y"])
#   longitude <- c(longitude, st_coordinates(centroid)[, "X"])
# }
# province_topics$latitude <- latitude
# province_topics$longitude <- longitude
# 
# # Afficharge du map
# labels_topic <- sprintf(
#   "<strong>%s</strong>
#   <br/>topic 1: %g
#   <br/>topic 2: %g
#   <br/>topic 3: %g
#   <br/>topic 4: %g
#   <br/>topic 5: %g",
#   province_topics_data1$Province,
#   province_topics_data1$`topic 1`,
#   province_topics_data1$`topic 2`,
#   province_topics_data1$`topic 3`,
#   province_topics_data1$`topic 4`,
#   province_topics_data1$`topic 5`
# ) %>% lapply(htmltools::HTML)
# 
# topic_map <- leaflet(width = "100%", height = "400px") %>%
#   addProviderTiles("CartoDB.Positron")%>%
#   addPolygons(
#     data = province_topics_data1,
#     weight = 2,
#     opacity = 1,
#     color = "#666",
#     fillOpacity = 0.1 ,
#     label = labels_topic,
#     labelOptions = labelOptions(
#       style = list("font-weight" = "normal", padding = "3px 8px"),
#       textsize = "15px",
#       direction = "auto")
#   )%>%
#   addMinicharts(
#     lng = province_topics$longitude,
#     lat = province_topics$latitude,
#     type = "bar",
#     chartdata = province_topics[1:5],
#     width = 25, height = 25,
#   ) %>%
#   setView(lng=7.661594, lat=51.433237, zoom=7.4)
