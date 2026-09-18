# mod_genetic_diversities.R

mod_genetic_diversities_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$head(gs_head()),

    module_banner("chart-line", "Genetic Diversities · HS · HT",""),
    
    # tags$div(class = "spg-method-note", style = "border-left-color:#78B7C5;",
    #   HTML(paste0(
    #     "<b>HS</b> (within-population gene diversity) and <b>HT</b> (total gene diversity) ",
    #     "from Weir &amp; Cockerham (1984), reported per locus and as multilocus estimates. ",
    #     "<br><br>",
    #     "<b>Confidence intervals are computed by three resampling schemes:</b>",
    #     "<ul style='margin:4px 0 0 16px;'>",
    #     "<li><b>Individuals</b> (HS per locus and per population): individuals resampled with replacement within each population.</li>",
    #     "<li><b>Populations</b> (HS and HT per locus): populations resampled with replacement.</li>",
    #     "<li><b>Loci</b> (overall HS and HT only): loci resampled with replacement.</li>",
    #     "</ul>"
    #   ))
    # ),

    fluidRow(
      box(
        width = 12,
        title = div(style = "background-color: #FFFFFF; padding: 10px; color: #333a43; font-weight: 600;",
                    # icon("chart-line"),
                    "Genetic Diversity Analysis parameters"),
        solidHeader = TRUE, status = "primary",
        fluidRow(
          column(3,
            h4(icon("sliders"), "Parameters"),
            numericInput(ns("n_perm_fst_div"),    "Number of Permutations:",        value = 5000, min = 100, max = 20000, step = 100),
            numericInput(ns("n_boot_fst_div"),    "Number of Bootstrap Replicates:", value = 5000, min = 100, max = 20000, step = 100),
            downloadButton(ns("run_FST_Analysis_div"), " Run",
                         icon = icon("rocket"),
                         class = "btn-action-primary btn-block", style = "font-weight: bold;"),
            tags$small(
              style = "color: #666; margin-top: 6px; display: block;",
              icon("info-circle"),
              "Also populates FST/FIT results in the Population subdivision tab."
            )
          ),
          column(9,
            h4(icon("chart-line"), "Diversity Analysis Summary",
               style = "font-weight: 600; color: #2c3e50; margin-bottom: 15px;"),
            fluidRow(
              column(3, valueBoxOutput(ns("global_fst_div_box"),    width = NULL)),
              column(3, valueBoxOutput(ns("global_hs_box"),         width = NULL)),
              column(3, valueBoxOutput(ns("global_ht_box"),         width = NULL)),
              column(3, valueBoxOutput(ns("analysis_time_div_box"), width = NULL))
            ),
            fluidRow(
              column(12,
                h5("Analysis Progress", style = "margin-top: 15px; font-weight: 600;"),
                shinyWidgets::progressBar(id = ns("fst_progress_div"), value = 0,
                                          title = "Overall Progress")
              )
            )
          )
        )
      )
    )
  )
}
