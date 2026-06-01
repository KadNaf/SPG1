# =========================================================
# 3. NOUVEAU app_ui GLOBAL
# =========================================================

app_ui <- function() {

  shiny::fluidPage(

    shiny::uiOutput("app_launcher_ui")
  )
}