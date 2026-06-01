# =========================================================
# 4. SERVER
# =========================================================

app_server <- function(input, output, session) {

  rv <- shiny::reactiveValues(
    launched       = FALSE,
    raw            = NULL,
    data           = NULL,
    formatted_data = NULL
  )

  # -------------------------------------------------------
  # LANCEMENT APP
  # -------------------------------------------------------

  shiny::observeEvent(input$launch_app, {

    rv$launched <- TRUE
  })

  # -------------------------------------------------------
  # UI DYNAMIQUE
  # -------------------------------------------------------

  output$app_launcher_ui <- shiny::renderUI({

    if (!rv$launched) {

      landing_ui()

    } else {

      main_app_ui()
    }
  })

  # -------------------------------------------------------
  # MODULES
  # -------------------------------------------------------

  server_import_data("import", rv)
  server_allele_frequencies("allele", rv)
  server_general_stats("general_stats", rv)
  server_LD("ld", rv)
  server_null_alleles("null_alleles", rv)
  server_isolation_by_distance("ibd", rv)
}