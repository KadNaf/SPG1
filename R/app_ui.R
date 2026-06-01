#' Application UI
#' @export
app_ui <- function() {
  shiny::fluidPage(
    shiny::uiOutput("app_content")
  )
}