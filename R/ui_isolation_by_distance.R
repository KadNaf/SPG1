# ui_isolation_by_distance.R
# Tab 1 — Geographic Distances (Haversine, km) from GPS, half-matrix + long format
# Tab 2 — Mantel Test: uploaded Matrix 1 (square or rectangular) vs internal
#         Dgeo / ln(Dgeo), joint row/column permutation (Mantel-valid on
#         rectangular / incomplete matrices).

isolation_by_distance_UI <- function(id) {
  ns <- NS(id)

  fluidPage(
    tags$head(gs_head()),

    module_banner(
      "map-marker-alt",
      "Isolation by Distance",
      "Geographic distances (Haversine, km) \u00b7 Mantel test \u00b7 rectangular matrices",
      "#2CBF9F"
    ),

    tags$div(
      style = paste(
        "display:flex; align-items:center; gap:12px;",
        "background:#FFF8E1; border:2px solid #E1AF00;",
        "border-radius:6px; padding:10px 16px; margin-bottom:16px;"
      ),
      tags$span(style = "font-size:1.8em; line-height:1;", "\U0001f6a7"),
      tags$div(
        tags$strong(style = "color:#7B5800;", "Module under construction"),
        tags$span(style = "color:#7B5800; margin-left:8px; font-size:0.9em;",
          "Results are functional but the module is still being validated. Use with caution.")
      )
    ),

    tabsetPanel(
      id = ns("main_tabs"), type = "tabs",

      # ════════════════════════════════════════════════════════════════════
      # TAB 1 — Geographic distances
      # ════════════════════════════════════════════════════════════════════
      tabPanel(
        title = tagList(icon("globe"), " Geographic Distances"),
        value = "tab_geo",

        tags$div(
          class = "spg-method-note", style = "border-left-color:#2CBF9F; margin-top:14px;",
          HTML(paste0(
            "Pairwise geographic distance (Haversine formula, km) between population ",
            "centroids, computed from individual GPS coordinates set at import. ",
            "95% confidence intervals are obtained by <b>bootstrapping individuals within ",
            "each population</b> (resample with replacement, recompute the centroid, ",
            "recompute the distance) \u2014 the CI reflects within-population GPS dispersion; ",
            "it collapses to ~0 if every individual in a population shares the same ",
            "coordinate.<br><br>",
            "<b>Requires latitude/longitude columns set during data import.</b>"
          ))
        ),

        fluidRow(
          box(
            width = 3,
            title = div(style = "background:#FFFFFF; padding:10px; color:#333a43; font-weight:600;",
                        icon("sliders-h"), " Parameters"),
            solidHeader = TRUE, status = "primary",
            numericInput(ns("n_boot_geo"), "Bootstrap replicates (individuals):",
                         value = 500, min = 100, max = 10000, step = 100),
            sliderInput(ns("conf_level_geo"), "Confidence level (%):",
                        min = 80, max = 99, value = 95, step = 1),
            tags$hr(),
            actionButton(
              ns("run_geo"), "Compute Geographic Distances",
              icon  = icon("rocket"),
              class = "btn-action-primary btn-block",
              style = "font-weight:bold;"
            )
          ),
          box(
            width = 9,
            title = div(style = "background:#FFFFFF; padding:10px; color:#333a43; font-weight:600;",
                        icon("chart-bar"), " Summary"),
            solidHeader = TRUE, status = "primary",
            fluidRow(
              column(3, valueBoxOutput(ns("box_npops_geo"),  width = NULL)),
              column(3, valueBoxOutput(ns("box_npairs_geo"), width = NULL)),
              column(3, valueBoxOutput(ns("box_avg_dgeo"),   width = NULL)),
              column(3, valueBoxOutput(ns("box_max_dgeo"),   width = NULL))
            )
          )
        ),

        fluidRow(
          box(
            width = 5,
            title = div(style = "background:#FFFFFF; padding:10px; color:#333a43; font-weight:600;",
                        icon("th"), " Half-matrix \u2014 D", tags$sub("geo"), " (km)"),
            solidHeader = FALSE,
            uiOutput(ns("ui_geo_matrix"))
          ),
          box(
            width = 7,
            title = div(style = "background:#FFFFFF; padding:10px; color:#333a43; font-weight:600;",
                        icon("table"), " Long format \u2014 pairwise table"),
            solidHeader = FALSE,
            DT::DTOutput(ns("dt_geo_long")),
            tags$br(),
            downloadButton(ns("dl_geo_csv"), "Download CSV", class = "btn-action-secondary btn-sm"),
            tags$span("\u00a0\u00a0"),
            downloadButton(ns("dl_geo_txt"), "Download TXT", class = "btn-action-secondary btn-sm")
          )
        )
      ),

      # ════════════════════════════════════════════════════════════════════
      # TAB 2 — Mantel test
      # ════════════════════════════════════════════════════════════════════
      tabPanel(
        title = tagList(icon("project-diagram"), " Mantel Test"),
        value = "tab_mantel",

        tags$div(
          class = "spg-method-note", style = "border-left-color:#2CBF9F; margin-top:14px;",
          HTML(paste0(
            "Mantel permutation test between an uploaded ", tags$b("Matrix\u00a01"),
            " and the geographic distance (", tags$b("Matrix\u00a02"), " = D", tags$sub("geo"),
            " or ln(D", tags$sub("geo"), ")) computed in Tab\u00a01 \u2014 nothing needs to be ",
            "uploaded for Matrix\u00a02, it always comes from this app's GPS data. ",
            "Permutation is by <b>joint row/column relabelling</b> of Matrix\u00a02 ",
            "(the classic Mantel procedure), which stays valid when either matrix is ",
            "<b>rectangular</b> (incomplete \u2014 only some pairs available)."
          ))
        ),

        fluidRow(
          box(
            width = 4,
            title = div(style = "background:#FFFFFF; padding:10px; color:#333a43; font-weight:600;",
                        icon("file-upload"), " Matrix 1 (upload)"),
            solidHeader = TRUE, status = "primary",

            fileInput(ns("mat1_file"), "File:", accept = c(".csv", ".txt", ".tsv")),

            radioButtons(ns("mat1_sep"), "Separator:",
              choices = c("Comma" = ",", "Tab" = "\t", "Semicolon" = ";"),
              selected = ",", inline = TRUE),

            radioButtons(ns("mat1_format"), "File layout:",
              choices = c(
                "Square matrix (Pop x Pop, header row + row labels)" = "square",
                "Long / rectangular (Pop1, Pop2, Value columns)"          = "long"
              ),
              selected = "square"),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'long'", ns("mat1_format")),
              uiOutput(ns("ui_mat1_cols"))
            ),

            tags$p(style = "color:#777; font-size:11px;",
              "Square format: first column = row labels, header = column labels ",
              "(same population names as Matrix\u00a02). Missing cells / asymmetric ",
              "(rectangular) matrices are supported \u2014 leave them blank or NA.")
          ),

          box(
            width = 8,
            title = div(style = "background:#FFFFFF; padding:10px; color:#333a43; font-weight:600;",
                        icon("sliders-h"), " Matrix 2 (internal) & Mantel parameters"),
            solidHeader = TRUE, status = "primary",

            fluidRow(
              column(4,
                radioButtons(ns("mat2_choice"), "Matrix 2:",
                  choices = c(
                    "Dgeo (km)"    = "raw",
                    "ln(Dgeo)"     = "ln"
                  ),
                  selected = "ln"),
                tags$p(style = "color:#777; font-size:11px;",
                  "Computed from Tab\u00a01 \u2014 run it first.")
              ),
              column(4,
                radioButtons(ns("mantel_stat"), "Statistic:",
                  choices = c("Pearson r" = "r", "Regression slope b" = "b"),
                  selected = "r"),
                numericInput(ns("n_perm_mantel"), "Permutations:",
                             value = 9999, min = 99, max = 99999, step = 1000)
              ),
              column(4,
                tags$div(style = "margin-top:25px;",
                  actionButton(
                    ns("run_mantel"), "Run Mantel Test",
                    icon  = icon("random"),
                    class = "btn-action-primary btn-block",
                    style = "font-weight:bold;"
                  )
                )
              )
            ),

            tags$hr(),

            fluidRow(
              column(3, valueBoxOutput(ns("box_m_stat"),  width = NULL)),
              column(3, valueBoxOutput(ns("box_m_pval"),  width = NULL)),
              column(3, valueBoxOutput(ns("box_m_n"),     width = NULL)),
              column(3, valueBoxOutput(ns("box_m_r2"),    width = NULL))
            )
          )
        ),

        fluidRow(
          box(
            width = 7,
            title = div(style = "background:#FFFFFF; padding:10px; color:#333a43; font-weight:600;",
                        icon("chart-line"), " Scatter plot"),
            solidHeader = FALSE,
            plotly::plotlyOutput(ns("mantel_scatter"), height = "420px")
          ),
          box(
            width = 5,
            title = div(style = "background:#FFFFFF; padding:10px; color:#333a43; font-weight:600;",
                        icon("file-alt"), " Full results"),
            solidHeader = FALSE,
            verbatimTextOutput(ns("mantel_full_results")),
            tags$br(),
            downloadButton(ns("dl_mantel_results"), "Download results (.txt)",
                           class = "btn-action-secondary btn-sm")
          )
        )
      )
    )
  )
}