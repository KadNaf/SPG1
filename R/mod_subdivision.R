# mod_subdivision_ui.R

mod_subdivision_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$head(gs_head()),

    module_banner("sitemap", "Population Subdivision · FST · G-test",""),

    fluidRow(
      box(
        width = 12,
        title = div(style = "background-color: #FFFFFF; padding: 10px; color: #333a43; font-weight: 600;",
                    "FST: CI & p-value parameters"),
        solidHeader = TRUE, status = "primary",
        fluidRow(
          column(3,
            h4(icon("sliders"), "Parameters"),
            numericInput(ns("n_perm_fst"),     "Number of Permutations:",        value = 10000, min = 100,  max = 20000, step = 100),
            numericInput(ns("n_boot_fst"),     "Number of Bootstrap Replicates:", value = 10000, min = 100,  max = 20000, step = 100),
            numericInput(ns("conf_level_fst"), "Confidence Level:",               value = 0.95, min = 0.80, max = 0.99,  step = 0.01),
            uiOutput(ns("ui_fst_out_status")),
            downloadButton(ns("run_FST_Analysis"), " Run",
                         icon  = icon("rocket"),
                         class = "btn-action-primary btn-block",
                         style = "font-weight: bold;")
          ),
          column(9,
            h4(icon("chart-line"), "FST Analysis Summary",
               style = "font-weight: 600; color: #2c3e50; margin-bottom: 15px;"),
            fluidRow(
              column(3,
                valueBoxOutput(ns("global_fst_box"),       width = NULL),
                valueBoxOutput(ns("fst_ci_width_box"),     width = NULL)
              ),
              column(3,
                valueBoxOutput(ns("global_fst_pvalue_box"), width = NULL),
                valueBoxOutput(ns("fst_power_box"),         width = NULL)
              ),
              column(3,
                valueBoxOutput(ns("significant_loci_fst_box"), width = NULL),
                valueBoxOutput(ns("fst_convergence_box"),      width = NULL)
              ),
              column(3,
                valueBoxOutput(ns("analysis_time_fst_box"), width = NULL),
                valueBoxOutput(ns("fst_quality_box"),       width = NULL)
              )
            ),
            fluidRow(
              column(12,
                h5("Analysis Progress", style = "margin-top: 15px; font-weight: 600;"),
                shinyWidgets::progressBar(id = ns("fst_progress"), value = 0,
                                          title = "Overall Progress")
              )
            ),
            fluidRow(
              column(4,
                valueBoxOutput(ns("fst_locus_boot_box"), width = NULL)
              ),
              column(8,
                tags$p(style = "color:#666; font-size:12px; margin-top: 25px;",
                  icon("info-circle"),
                  " Overall FST with bootstrap CI obtained by resampling ", tags$b("loci"),
                  " (with replacement) instead of subsamples. See the dedicated tab below for the full table (FST, FIT, FIS)."
                )
              )
            )
          )
        )
      )
    ),

    h2("G-based Permutation Test \u2014 Subdivision", class = "section-title"),

    fluidRow(
      box(
        width = 12,
        title = div(style = "background-color: #FFFFFF; padding: 10px; color: #333a43; font-weight: 600;",
                    icon("flask"), "G-test: parameters"),
        solidHeader = TRUE, status = "primary",
        fluidRow(
          column(3,
            h4(icon("sliders"), "Parameters"),
            numericInput(ns("n_perm_g"),     "Number of Permutations:",
                         value = 10000, min = 1000, max = 50000, step = 1000),
            numericInput(ns("conf_level_g"), "Confidence Level:",
                         value = 0.95, min = 0.80, max = 0.99,  step = 0.01),
            uiOutput(ns("ui_gtest_out_status")),
            downloadButton(ns("run_G_test"), " Run",
                         icon  = icon("rocket"),
                         class = "btn-action-primary btn-block",
                         style = "font-weight: bold;")
          ),
          column(9,
            h4(icon("chart-area"), "G-test Summary",
               style = "font-weight: 600; color: #2c3e50; margin-bottom: 15px;"),
            fluidRow(
              column(3,
                valueBoxOutput(ns("g_global_obs_box"),    width = NULL),
                valueBoxOutput(ns("g_power_box"),         width = NULL)
              ),
              column(3,
                valueBoxOutput(ns("g_global_pvalue_box"), width = NULL),
                valueBoxOutput(ns("g_mean_pvalue_box"),   width = NULL)
              ),
              column(3,
                valueBoxOutput(ns("g_signif_loci_box"),   width = NULL),
                valueBoxOutput(ns("g_time_box"),          width = NULL)
              ),
              column(3,
                valueBoxOutput(ns("g_n_perm_box"),        width = NULL)
              )
            ),
            fluidRow(
              column(12,
                h5("Analysis Progress", style = "margin-top: 15px; font-weight: 600;"),
                shinyWidgets::progressBar(id = ns("g_progress"), value = 0,
                                          title = "Overall Progress")
              )
            )
          )
        )
      )
    )
  )
}