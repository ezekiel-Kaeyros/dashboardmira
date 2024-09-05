box::use(
  tm[tm_map,content_transformer,TermDocumentMatrix,Corpus,VectorSource,inspect,removeNumbers,
     removeWords,removePunctuation,stripWhitespace,stopwords]
)

box::use(
  app/logic/import_data
)

text <- import_data$data$description
text_f <- Corpus(VectorSource(text))
#inspect(text_f)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
text_f <- tm_map(text_f, toSpace, "/")
text_f <- tm_map(text_f, toSpace, "@")
text_f <- tm_map(text_f, toSpace, "\\|")

text_f <- tm_map(text_f, content_transformer(tolower))
text_f <- tm_map(text_f, removeNumbers)
text_f <- tm_map(text_f, removeWords, stopwords("english"))
text_f <- tm_map(text_f, removePunctuation)
text_f <- tm_map(text_f, stripWhitespace)

TDM <- TermDocumentMatrix(text_f)
m <- as.matrix(TDM)
v <- sort(rowSums(m),decreasing=TRUE)
data_wordcloud <- data.frame(word = names(v),freq=v)

