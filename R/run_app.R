# =========================================================
# 5. RUN APP
# =========================================================

run_app <- function(...) {

  options(shiny.maxRequestSize = 500 * 1024^2)

  shiny::shinyApp(
    ui = app_ui,
    server = app_server,
    ...
  )
}