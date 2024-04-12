box::use(
  shiny[tagList, div, h1, img, a, observeEvent],
  shiny.fluent[ActionButton.shinyInput, Text,Stack],glue[glue],

)

# home page cards

#' @export
app_overview_card <- function(title, icon, content) {
  div(class = "app_overview_card",
      tagList(
        div(class = "app_overview_card__header",
            h1(class = "app_overview_card__title", title),
            icon
        ),
        h1(class = "app_overview_card__content", content)
      )
  )
}


#' @export
app_metrics_card <- function(content) {
  tagList(
    div(class = "app_metrics__wrapper",
        h1(class = "app_metrics__title", "Recent"),
        div(class = "app_metrics_card",
            tagList(
              div(class = "app_metrics_card__header",
                  h1(class = "title", "PT14505"),
                  img(src = "qualityIcon.svg", class = "icon")
              ),
              div(class = "app_metrics_card__content", content)
            )
        ))
  )
}

#' @export
recent_card <- function(id, url, date) {
  ################ That is the Link not a Button
  a(href = url, class = "recent_app_card",
    tagList(
      div(class = "recent_app_card__wrapper",
          h1(class = "recent_app_card__title", id),
          div(class = "recent_app_card__content",
              h1(class = "description", date)
          )
      ),
      img(src = "./icons/tag.svg", class = "icon")
    )

  )
}


#' @export
card_ui <- function(title, actionBtn ,content) {
  div(class = "card",
    tagList(
      div(class = "card__header",
        h1(class = "title", title),
        actionBtn
      ),
      div(class = "card__content", content)
    )
  )
}

card_server <- function(input, output) {

}

#' @export
card_comapre_ui <- function(title, actionBtn ,content) {
  div(class = "compare_card",
      tagList(
        div(class = "compare_card__header",
            div( class = "title",
              h1(""), #Choose files to compare
              actionBtn
            )
        ),
        div(class = "card__content", content)
      )
  )
}


#' @export
makeCard <- function(title, content, size = 5, style = "") {

  div(
    class = glue("card ms-depth-8 ms-sm{size} ms-xl{size}"),
    style = style,
    Stack(
      tokens = list(childrenGap = 5),
      Text(variant = "large", title, block = TRUE),
      content
    )
  )
}
