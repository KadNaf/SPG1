# mod_local_panmixia.R

mod_local_panmixia_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$head(gs_head()),

    module_banner("flask", "Local Panmixia · FIS",""),
    
    # tags$div(class = "spg-method-note", style = "border-left-color:#9986A5;",
    #   HTML(paste0(
    #     "Local panmixia means that each sub-population is at ",
    #     "Hardy-Weinberg equilibrium (HWE) \u2014 individuals mate randomly ",
    #     "<em>within</em> their population. ",
    #     "<br><br>",
    #     "<b>H<sub>0</sub>:</b> FIS = 0 within each population (no departure from HWE). &nbsp;",
    #     "<b>Bootstrap:</b> individuals resampled with replacement within populations; percentile CI. &nbsp;",
    #     "<b>Permutation:</b> alleles reshuffled within each population; two-sided |FIS| test."
    #   ))
    # ),

    fluidRow(
      box(
        width = 12,
        title = div(style = "background-color: #FFFFFF; padding: 10px; color: #333a43; font-weight: 600;",
                    # icon("rocket"),
                    "FIS: CI & p-value parameters"),
        solidHeader = TRUE, status = "primary",
        fluidRow(
          column(3,
            h4(icon("sliders"), "Parameters"),
            numericInput(ns("n_perm"),    "Number of Permutations:",        value = 5000, min = 100, max = 20000),
            numericInput(ns("n_boot"),    "Number of Bootstrap Replicates:", value = 5000, min = 100, max = 20000),
            numericInput(ns("conf_level"),"Confidence Level:",               value = 0.95, min = 0.80, max = 0.99, step = 0.01),
            selectInput(ns("analysis_level"), "Analysis Level:",
                        choices = c("By Locus", "By Population"), selected = "By Locus"),
            uiOutput(ns("ui_fis_out_status")),
            downloadButton(ns("Run_FIS_Analysis"), " Run",
                         icon = icon("rocket"),
                         class = "btn-action-primary btn-block", style = "font-weight: bold;")
          ),
          column(9,
            h4(icon("chart-line"), "Analysis Summary",
               style = "font-weight: 600; color: #2c3e50; margin-bottom: 15px;"),
            fluidRow(
              column(3, valueBoxOutput(ns("global_fis_box"),        width = NULL)),
              column(3, valueBoxOutput(ns("global_pvalue_box"),     width = NULL)),
              column(3, valueBoxOutput(ns("significant_loci_box"),  width = NULL)),
              column(3, valueBoxOutput(ns("analysis_time_box"),     width = NULL))
            ),
            fluidRow(
              column(12,
                h5("Analysis Progress", style = "margin-top: 15px; font-weight: 600;"),
                shinyWidgets::progressBar(id = ns("fis_progress"), value = 0,
                                          title = "Overall Progress")
              )
            )
          )
        )
      )
    ),

    h2("FIS · By Locus \u00d7 Population", class = "section-title"),
    # tags$p(HTML(paste0(
    #   "WC84 FIS and permutation p-values for every locus \u00d7 population combination. ",
    #   "Permutation only (no bootstrap CI). Run independently of the main analysis above."
    # )), style = "font-size: 16px; line-height: 1.5; color: #2c3e50;"),

    fluidRow(
      box(
        width = 12,
        title = div(style = "background-color: #FFFFFF; padding: 10px; color: #333a43; font-weight: 600;",
                    icon("table"),
                    "FIS (WC84) per locus \u00d7 population"),
        solidHeader = TRUE, status = "primary",
        fluidRow(
          column(3,
            h4(icon("sliders"), "Parameters"),
            numericInput(ns("fis_lp_n_perm"), "Number of Permutations:",
                         value = 5000, min = 100, max = 20000)
          ),
          column(9,
            br(),
            uiOutput(ns("ui_fislp_out_status")),
            downloadButton(ns("run_fis_locus_pop"), " Run",
                         icon = icon("rocket"),
                         class = "btn-action-primary btn-block", style = "font-weight: bold;")
          )
        ),
        style = "padding: 10px;"
      )
    )
  )
}