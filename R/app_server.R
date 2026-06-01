#' Application Server
#' @export
app_server <- function(input, output, session) {
  rv <- shiny::reactiveValues(
    launched = FALSE,  # Nouveau: pour suivre si on a cliqué sur Launch
    raw = NULL,
    data = NULL,
    formatted_data = NULL
  )
  
  # Affiche soit la landing, soit l'app principale
  output$app_content <- shiny::renderUI({
    if (!rv$launched) {
      landing_ui()
    } else {
      main_app_ui()
    }
  })
  
  # Quand on clique sur Launch
  shiny::observeEvent(input$launch_app, {
    rv$launched <- TRUE
  }, ignoreInit = TRUE)
  
  # Initialise les modules seulement après le launch
  shiny::observe({
    if (rv$launched) {
      server_import_data("import", rv)
      server_allele_frequencies("allele", rv)
      server_general_stats("general_stats", rv)
      server_LD("ld", rv)
      server_null_alleles("null_alleles", rv)
      server_isolation_by_distance("ibd", rv)
    }
  })
}