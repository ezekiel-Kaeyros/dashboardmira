box::use(
  shiny[tagList, img, NS, div],
  shiny.fluent[Text, fluentPage]
)


header <- tagList(
  div(Text(""))
)

#' @export
header_ui <-  header
