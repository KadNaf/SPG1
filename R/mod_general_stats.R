# mod_general_stats.R

mod_general_stats_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    useWaiter(),
    tags$head(gs_head()),

    module_banner("table", "General Statistics",""),

    fluidRow(
      box(
        width = 6,
        title = div(style = "background-color: #FFFFFF; padding: 10px; color: #333a43; font-weight: 600;",
                    icon("chart-bar"), "Statistics selection"),
        solidHeader = TRUE, status = "primary",
        h4("Select Genetic Indices"),
        h5("Diversity Measures:"),
        checkboxInput(ns("ho_checkbox"),  "Ho (Observed Heterozygosity)", TRUE),
        checkboxInput(ns("hs_checkbox"),  "Hs (Expected Heterozygosity within populations)", TRUE),
        checkboxInput(ns("ht_checkbox"),  "Ht (Total expected heterozygosity)", TRUE),
        h5("F-statistics (Weir & Cockerham):"),
        checkboxInput(ns("fit_wc_checkbox"), "FIT (Weir & Cockerham estimator)", TRUE),
        checkboxInput(ns("fis_wc_checkbox"), "FIS (Weir & Cockerham estimator)", TRUE),
        checkboxInput(ns("fst_wc_checkbox"), "FST (Weir & Cockerham estimator)", TRUE),
        h5("Advanced Statistics:"),
        checkboxInput(ns("fst_max_checkbox"),  "Fst-max (Maximum differentiation, Meirmans)", FALSE),
        checkboxInput(ns("fst_prim_checkbox"), "Fst' (Meirmans) (Empirical standardisation)", FALSE),
        checkboxInput(ns("GST_checkbox"),      "GST (Nei's genetic differentiation)", FALSE),
        checkboxInput(ns("GST_sec_checkbox"),  "GST'' (Hedrick's correction)", FALSE),
        h5("Detail population (used when saving results):"),
        selectInput(ns("selected_pop_overall"), "Select Population:", choices = NULL),
        tags$hr(),
        downloadButton(ns("run_basic_stats"),
                     label = "Run",
                     icon = icon("rocket"),
                     class = "btn-action-primary btn-block")
      )
    ),

    # tags$p(HTML(paste0(
    #   "For each allele at each locus, WC84 variance components ",
    #   "(a = between-pop, b = between-indiv, c = within-indiv) are computed ",
    #   "and the three F-statistics derived: ",
    #   "<b>FIS</b> = b/(b+c), <b>FST</b> = a/(a+b+c), <b>FIT</b> = (a+b)/(a+b+c). ",
    #   "<br>High <b>FIS</b> for a specific allele may indicate amplification dropout. ",
    #   "Outlier <b>FST</b> may signal selection or local adaptation."
    # )), style = "font-size: 16px; line-height: 1.5; color: #2c3e50;"),

    fluidRow(
      box(
        width = 12,
        title = div(style = "background-color: #FFFFFF; padding: 10px; color: #333a43; font-weight: 600;",
                    icon("dna"), "F-statistics per allele (Weir & Cockerham)"),
        solidHeader = TRUE, status = "primary",
        fluidRow(
          column(12,
            downloadButton(ns("compute_allele_fstats"),
              label = "Run",
              icon = icon("rocket"),
              class = "btn-action-primary btn-block", style = "font-weight: bold;")
          )
        ),
        style = "padding: 10px;"
      )
    )
  )
}
