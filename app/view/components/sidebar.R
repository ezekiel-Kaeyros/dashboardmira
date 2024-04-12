box::use(
  shiny.fluent[Nav, Stack, fluentPage],
  shiny[NS, div, img]
)

sidebar_nav <- Nav(
  groups = list(
    list(links = list(
                      list(name = "Home", url = "#!/", key = "home", icon = "Home"),
                      list(
                        name = "Quantitative Data",
                        url = "#!/quantitative",
                        key = "quantitative",
                        icon = "BarChart4"
                      ),
                      list(
                        name = "Qualitative Data",
                        url = "#!/qualitative",
                        key = "qualitative",
                        icon = "SearchData"
                      ),
                      list(
                        name = "Compare",
                        url = "#!/compare",
                        key = "compare",
                        icon = "Tiles"
                      )))
  ),
  initialSelectedKey = "home",
  styles = list(
    root = list(
      height = "100%",
      boxSizing = "border-box",
      overflowY = "auto"
    )
  )
)

#' @export
sidebar_ui <-  Stack(
  tokens = list(childrenGap = 70),
  div(class = "sidebar__logo", img(src = "logo.svg", class = "logo")),
  div(class = "sidebar__navigation",
    sidebar_nav
  )
)
