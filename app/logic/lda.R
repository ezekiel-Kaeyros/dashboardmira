box::use(
  topicmodels[ LDA, terms, posterior],
  tidytext,
  LDAvis[serVis,createJSON],
  quanteda[corpus, corpus_reshape, dfm, dfm_trim, convert, stopwords],
  Rmpfr[mpfr],
  servr
)


box::use(
  app/logic/import_data,
)


root <- getwd()
path_data <- paste(root, "/", "app/data/", sep="")
route <- paste(path_data,"/Topic_modelling", sep="")

# Vérifier si le dossier existe déjà
if (!dir.exists(route)) {
  # Si le dossier n'existe pas, exécuter serVis

  sotu_corpus <- corpus(import_data$data$description)
  corp = corpus_reshape(sotu_corpus, to = "sentences")
  #corp = corpus_reshape(data_corpus_inaugural, to = "paragraphs")
  dfm = dfm(corp, remove_punct=T, remove=stopwords("german"))
  dfm = dfm_trim(dfm, min_docfreq = 5)

  dtm = convert(dfm, to = "topicmodels")
  set.seed(1)

  harmonicMean <- function(logLikelihoods, precision = 2000L) {
    llMed <- median(logLikelihoods)
    as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                         prec = precision) + llMed))))
  }

  m = LDA(dtm, method = "Gibbs", k = 5,  control = list(alpha = 0.1))
  m

  terms(m, 5)

  dtm = dtm[slam::row_sums(dtm) > 0, ]
  phi = as.matrix(posterior(m)$terms)
  theta <- as.matrix(posterior(m)$topics)
  vocab <- colnames(phi)
  doc.length = slam::row_sums(dtm)
  term.freq = slam::col_sums(dtm)[match(vocab, colnames(dtm))]

  json = createJSON(phi = phi, theta = theta, vocab = vocab,
                    doc.length = doc.length, term.frequency = term.freq)
  serVis(json, out.dir = route, open.browser = FALSE)
} else {
  # Si le dossier existe déjà, afficher un message ou effectuer une autre action
  print("Le dossier Topic Modeling existe déjà, aucune action supplémentaire nécessaire.")
}







#serVis(json, out.dir = 'Topic_modelling', open.browser = FALSE)
