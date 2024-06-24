box::use(
  jsonlite[fromJSON],
  rjson[fromJSON],
  magrittr[`%>%`],
  tidyr[replace_na],
  dplyr[mutate, if_else, first, summarise, summarize,group_by, select, n, rename,inner_join, right_join,left_join, filter, ungroup],
  leaflet[colorFactor,leafletOutput,renderLeaflet,addProviderTiles,addPolygons,
          addLegend, leaflet, setView,removeScaleBar,labelOptions,highlightOptions],

  tm[stopwords, Corpus, VectorSource, tm_map, content_transformer, removeWords, removePunctuation,
     removeNumbers, stemDocument, stripWhitespace, DocumentTermMatrix, removeSparseTerms],
  sf[st_sfc, st_centroid, st_coordinates, st_drop_geometry],
  topicmodels[LDA, posterior],
  wordcloud2[wordcloud2],
  leaflet.minicharts[addMinicharts],
  stats[quantile, aggregate, as.formula],
  #lubridate[format],
  utils[head, tail],
  httr[GET, content, http_status],
  slam[row_sums],
  janitor[clean_names], stats, tidyr
)

box::use(
  mongolite[mongo]
)

# connection_string <- "mongodb+srv://dina-user:LZtL1VtD0KTyW4pZ@dina-db.rzr5ytg.mongodb.net/?retryWrites=true&w=majority&appName=dina-db"
# collection <- "test"
# db <- "reports"
fetch_mongodb <- function(connection_string, collection, db) {
  reports <- mongo(collection=collection, db=db, url=connection_string)
  reports <- as.data.frame(reports$find())
}

#""""""""""""""""""""""""""""""""""""""""""""""""""""
root <- getwd()
path_data <- paste(root, "/", "app/data/", sep="")


#""""""""""""""""""""""""""""""""""""""""""""""""""""""
connection_string <- "mongodb+srv://mira-user:2qr3fxQ6m4v8QK25@mira-db.dpvok4h.mongodb.net/?retryWrites=true&w=majority&appName=mira-db"

################## Secret Key
key <- "Anti-D-2024"
################# define viewer role
role <- 2

############## Fetch reports datas
data <- fetch_mongodb(connection_string = connection_string, db="test", collection = "reports")

############## Fetch login datas
login_data <- fetch_mongodb(connection_string = connection_string, collection = "users", db="test")

################## prepocessing of data ####################
data$dateState <- as.Date(substr(data$dateState, 1, 10), format = "%Y-%m-%d")
#data$valueDate <- format(data$valueDate, "%d.%m.%Y")

data$createdAt <- as.Date(substr(data$createdAt, 1, 10), format = "%Y-%m-%d")
#data$createdAt <- format(data$createdAt, "%d.%m.%Y")

data$updatedAt <- as.Date(substr(data$updatedAt, 1, 10), format = "%Y-%m-%d")
#data$updatedAt <- format(data$updatedAt, "%d.%m.%Y")


startDate = c()
endDate = c()
for (i in 1:nrow(data)){
  if (!is.null(data$dateRangeState[[i]][1])){
    startDate = c(startDate, strsplit(unlist(data$dateRangeState), ",")[[i]][1])
    endDate =  c(endDate, strsplit(unlist(data$dateRangeState), ",")[[i]][2])
  } else {
    startDate = c(startDate, NA)
    endDate =  c(endDate, NA)
  }

}

data$startDate <- startDate
data$startDate <- as.Date(substr(data$startDate, 1, 10), format = "%Y-%m-%d")
#data$startDate <- format(data$startDate, "%d.%m.%Y")

data$endDate <- endDate
data$endDate <- as.Date(substr(data$endDate, 1, 10), format = "%Y-%m-%d")
#data$endDate <- format(data$endDate, "%d.%m.%Y")



#"""""""""""""""""""""""""""""""""""""""""""""
data$identity <- gsub("Eine andere Person",
                      "Andere Person", data$identity)
data$identity <- gsub("Ich melde in Vertretung für die betroffene Person",
                      "In Vertretung der Betroffenen", data$identity)
data$identity <- gsub("Eine Organisation/Institution",
                      "Organisation/Institution", data$identity)


#"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
data$happenedOnline <- ifelse(data$happenedOnline == "false", "Das wirkliche Leben" , "Es ist online passiert")


#""""""""""""""""""""""""""""""""""""""""""""""""""""""
data$haveYouTakenMeasures <- ifelse(data$haveYouTakenMeasures == "Ja",
                               data$haveYouTakenMeasuresYes,
                               "Hat nichts gemacht")


data$haveYouTakenMeasures <- gsub("Sie hat den Fall bei der Polizei angezeigt",
                             "Polizei angezeigt", data$haveYouTakenMeasures)
data$haveYouTakenMeasures <- gsub("Sie hat eine Beratungsstelle aufgesucht",
                             "Beratungsstelle aufgesucht", data$haveYouTakenMeasures)
data$haveYouTakenMeasures<- gsub("Sie hat den Fall bei einer anderen Meldestelle gemeldet, und zwar:",
                            "Meldestelle gemeldet", data$haveYouTakenMeasures)
data$haveYouTakenMeasures <- gsub("Anderes, und zwar",
                             "Anderes", data$haveYouTakenMeasures)



#"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
Allemagne <- sf::st_read(paste(path_data, "DEU_adm_shp/DEU_adm3.shp", sep=""), quiet = TRUE) %>%
  rename(Province = NAME_2, location = NAME_3)


#""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
data$newFormOfRacismYes <- ifelse(data$isItAnotherFormOfDiscrimination == "Ja",
                                    data$newFormOfRacismYes, data$isItAnotherFormOfDiscrimination)
#
data$newFormOfRacismYes <- gsub("Nein",
                          "keine Form", data$newFormOfRacismYes)
data$newFormOfRacismYes <- gsub("Andere Formen, und zwar:",
                                "Andere", data$newFormOfRacismYes)

#""""""""""""""""""""""""""""""""""""""""""""""""""""""""
data$typeOfRacism <- gsub("Eine andere Art von Rassismus und zwar",
                          "Andere Art von Rassismus", data$typeOfRacism)


#""""""""""""""""""""""""""""""""""""""""""""""""""""""""
data$areaLocation <- gsub("Anderes und zwar:",
                          "Anderes", data$areaLocation)


#""""""""""""""""""""""""""""""""""""""""""""""""""""""""
data$racismManifestation <- gsub("Anderes und zwar",
                                 "Anderes", data$racismManifestation)
data$racismManifestation <- gsub("Die Person wurde benachteiligt",
                                 "Benachteiligt worden", data$racismManifestation)
data$racismManifestation <- gsub("Die Person wurde belästigt, gemobbt beleidigt oder schikaniert",
                                 "Belästigt, gemobbt, beleidigt oder schikaniert worden", data$racismManifestation)
data$racismManifestation <- gsub("Die Person habe sexuelle Belästigung erlebt",
                                 "Sexuelle Belästigung erlebt", data$racismManifestation)
data$racismManifestation <- gsub("Die Person wurde in einer Alltagsituation unterschwellig gedemütigt oder erniedrigt",
                                 "Unterschwellig gedemütigt oder erniedrigt worden", data$racismManifestation)
data$racismManifestation <- gsub("Die Person hat digitale Gewalt oder Diskriminierung erlebt",
                                 "Digitale Gewalt oder Diskriminierung erlebt", data$racismManifestation)
data$racismManifestation <- gsub("Die Person hat Gewalt erlebt",
                                 "Gewalt erlebt", data$racismManifestation)


data$racismManifestation <- gsub("Ich wurde benachteiligt",
                                 "Benachteiligt worden", data$racismManifestation)
data$racismManifestation <- gsub("Ich wurde belästigt, gemobbt beleidigt oder schikaniert",
                                 "Belästigt, gemobbt, beleidigt oder schikaniert worden", data$racismManifestation)
data$racismManifestation <- gsub("Ich habe sexuelle Belästigung erlebt",
                                 "Sexuelle Belästigung erlebt", data$racismManifestation)
data$racismManifestation <- gsub("Ich wurde in einer Alltagsituation unterschwellig gedemütigt oder erniedrigt",
                                 "Unterschwellig gedemütigt oder erniedrigt worden", data$racismManifestation)
data$racismManifestation <- gsub("Ich habe digitale Gewalt oder Diskriminierung erlebt",
                                 "Digitale Gewalt oder Diskriminierung erlebt", data$racismManifestation)
data$racismManifestation <- gsub("Ich habe Gewalt erlebt",
                                 "Gewalt erlebt", data$racismManifestation)


#""""""""""""""""""""""""""""""""""""""""""""""""""""""
data$description[is.na(data$description)] <- "aber"




#""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
################## Data to generate map topic ##############
file_save_lda_model <- paste(path_data, "lda_model.rds", sep="")
file_save_prediction <- paste(path_data, "prediction.rds", sep="")

# Vérifier si les fichiers existent
if (!file.exists(file_save_lda_model) || !file.exists(file_save_prediction)) {
  german_stopwords <- stopwords("german")
  # create corpus object
  corpus <- Corpus(VectorSource(data$description))
  # preprocessing chain
  processedCorpus <- tm_map(corpus, content_transformer(tolower))
  processedCorpus <- tm_map(processedCorpus, removeWords, german_stopwords)
  processedCorpus <- tm_map(processedCorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
  processedCorpus <- tm_map(processedCorpus, removeNumbers)
  processedCorpus <- tm_map(processedCorpus, stemDocument, language = "german")
  processedCorpus <- tm_map(processedCorpus, stripWhitespace)

  # compute document term matrix with terms >= minimumFrequency
  minimumFrequency <- 3
  DTM <- DocumentTermMatrix(processedCorpus, control = list(bounds = list(global = c(minimumFrequency,Inf))))

  # Supprimer les termes rares (sparse)
  DTM <- removeSparseTerms(DTM, sparse = 0.99)
  # Supprimer les documents avec des entrées nulles
  DTM <- DTM[row_sums(as.matrix(DTM)) > 0, ]

  set.seed(61)
  K <- 5
  # Si les fichiers n'existent pas, exécuter les étapes de modélisation et de prédiction
  topicModel <- LDA(DTM, K, method="Gibbs", control = list(iter = 500, verbose = 25))
  lda_fit <- topicModel

  # Sauvegarde du modèle LDA au format rds
  saveRDS(lda_fit, file = file_save_lda_model)

  # Prédiction
  prediction <- c()
  for (i in 1:length(data$description)) {
    text_to_categorize <- data$description[i]
    corpus_to_categorize <- Corpus(VectorSource(text_to_categorize))
    text_to_categorize <- tm_map(corpus_to_categorize, content_transformer(tolower))
    text_to_categorize <- tm_map(text_to_categorize, removeWords, german_stopwords)
    text_to_categorize <- tm_map(text_to_categorize, removePunctuation, preserve_intra_word_dashes = TRUE)
    text_to_categorize <- tm_map(text_to_categorize, removeNumbers)
    text_to_categorize <- tm_map(text_to_categorize, stemDocument, language = "german")
    text_to_categorize <- tm_map(text_to_categorize, stripWhitespace)

    # Créer une Document-Term Matrix (DTM) pour le texte
    dtm_to_categorize <- DocumentTermMatrix(corpus_to_categorize)
    dtm_to_categorize <- removeSparseTerms(dtm_to_categorize, sparse = 0.99)
    dtm_to_categorize <- dtm_to_categorize[row_sums(as.matrix(dtm_to_categorize)) > 0, ]

    test.topics <- posterior(lda_fit, dtm_to_categorize)
    index_max_probability <- which.max(test.topics$topics)
    prediction <- c(prediction, index_max_probability)
  }

  # Sauvegarde de la prédiction au format rds
  saveRDS(prediction, file_save_prediction)
} else {
  # Si les fichiers existent, charger les modèles et les prédictions à partir des fichiers rds
  lda_fit <- readRDS(file_save_lda_model)
  prediction <- readRDS(file_save_prediction)
}

data$prediction <- prediction
data$prediction[data$prediction==1] <- "topic 1"
data$prediction[data$prediction==2] <- "topic 2"
data$prediction[data$prediction==3] <- "topic 3"
data$prediction[data$prediction==4] <- "topic 4"
data$prediction[data$prediction==5] <- "topic 5"

# ajout d'une colonne province sur dina avec des valeurs aleatoire des regions du Nordrhein-Westfalen
# Création d'un dataframe des fréquences des sujets par province et jointure avec data1
province_topics <- as.data.frame.matrix(table(data$location, data$prediction))

# Réinitialiser les noms de lignes
province_topics$location <- rownames(province_topics)
rownames(province_topics) <- NULL

province_topics_data1 <- Allemagne %>%
  filter(NAME_1 == "Nordrhein-Westfalen") %>%
  select(NAME_1, Province, location) %>%
  left_join(province_topics, by = "location")


colonnes <- colnames(province_topics_data1)[grepl("^topic \\d+$", colnames(province_topics_data1))]
colonnes <- sprintf("`%s`", colonnes)
colonnes <- paste(colonnes, collapse = ", ")

formule <- as.formula(paste("cbind(", paste(colonnes, collapse = ", "), ") ~ Province"))
province_topics_data2 <- aggregate(formule, data = province_topics_data1, FUN = sum)

Allemagne2 <- sf::st_read(paste(path_data, "DEU_adm_shp/DEU_adm2.shp", sep=""), quiet = TRUE) %>%
  rename(Province = NAME_2) %>%
  filter(NAME_1 == "Nordrhein-Westfalen") %>%
  select(NAME_1, Province) %>%
  left_join(province_topics_data2, by = "Province") %>%
  replace_na(list(`topic 1` = 0, `topic 2` = 0, `topic 3` = 0, `topic 4` = 0, `topic 5` = 0))


latitude <- c()
longitude <- c()
for (i in 1:nrow(Allemagne2)) {
  # Récupérer les coordonnées du centre du polygone
  geometry_sf <- st_sfc(Allemagne2[i,]$geometry)
  centroid <- st_centroid(geometry_sf)
  latitude <- c(latitude, st_coordinates(centroid)[, "Y"])
  longitude <- c(longitude, st_coordinates(centroid)[, "X"])
}
Allemagne2$latitude <- latitude
Allemagne2$longitude <- longitude



################# Data to display Wordcloud #############
generate_wordcloud <- function(characteristics, min_freq = 5, language = "german") {
  # Charger les mots vides pour la langue spécifiée
  stopwords <- tm::stopwords(language)

  # Créer un objet Corpus
  corpus <- Corpus(VectorSource(characteristics))

  # Chaîne de prétraitement
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords)
  corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = TRUE)
  corpus <- tm_map(corpus, removeNumbers)

  # Obtenir le texte de chaque document dans le corpus
  characteristics_text <- sapply(corpus, as.character)

  # Concaténer toutes les chaînes de caractères en une seule
  all_characteristics_text <- paste(characteristics_text, collapse = " ")

  # Séparation du texte en mots
  words <- unlist(strsplit(all_characteristics_text, "\\W+"))

  # Création d'une table de fréquence des mots
  word_freq <- table(words)

  # Convertir la table en data frame
  word_freq_df <- as.data.frame(word_freq)
  names(word_freq_df) <- c("Word", "Frequency")

  # Filtrer les mots avec une fréquence minimale
  word_freq_df <- subset(word_freq_df, Frequency >= min_freq)

  # Affichage du nuage de mots interactif avec wordcloud2
  wordcloud2(word_freq_df)
}




#""""""""""""""""""""""""""" to display select input Compare page
key_var <- c("identity", "gender",
             "happenedOnline", "age", "haveYouTakenMeasures","newFormOfRacismYes",
             "typeOfRacism", "racismManifestation","numberOfEmployees")

options_var <- lapply(seq_along(key_var), function(i) {
  list(key = key_var[i], text = key_var[i])
})

filter_list <-c("Alle",unique(data$identity))
filter_text <- c("Alle",unique(data$identity))
options_filter <- lapply(seq_along(filter_list), function(i) {
  list(key = filter_list[i], text = filter_text[i])
})




########### to export data ###################
df <- data
df$areaLocation <- gsub("c\\(", "", df$areaLocation)
df$areaLocation <- gsub("\\)", "", df$areaLocation)

df$haveYouTakenMeasures <- gsub("c\\(", "", df$haveYouTakenMeasures)
df$haveYouTakenMeasures <- gsub("\\)", "", df$haveYouTakenMeasures)

df$gender <- gsub("c\\(", "", df$gender)
df$gender <- gsub("\\)", "", df$gender)


df <- df %>% dplyr::select(identity, gender, age,dateState,startDate,endDate, location, happenedOnline,
                           racismManifestation, racismManifestationIWasDisadvantaged, racismManifestationIWasDisadvantagedFreeField,
                           haveYouTakenMeasures,  typeOfRacism, newFormOfRacismYes,
                           organizationType, numberOfEmployees
)

