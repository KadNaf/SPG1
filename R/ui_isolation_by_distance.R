# ui_genetic_distances.R
# Tab: Pairwise Genetic Distances & Mantel Test
#
# Computes FST (WC84), FST-ENA (FreeNA), DCSE, DCSE-INA between all
# population pairs, with 95% CI by bootstrap over loci (FSTAT convention).
# Mantel test on rectangular column-format matrices via label permutation.

genetic_distances_UI <- function(id) {
  ns <- NS(id)

  fluidPage(
    tags$head(gs_head()),

    module_banner(
      "dna",
      "Pairwise Genetic Distances & Mantel Test",
      paste0(
        "FST (WC84) \u00b7 FST-ENA (FreeNA) \u00b7 DCSE \u00b7 DCSE-INA",
        " \u00b7 Locus bootstrap CI \u00b7 Mantel test \u2014 rectangular matrices"
      ),
      "#5B4EA7"
    ),

    tags$div(
      class = "spg-method-note", style = "border-left-color:#5B4EA7;",
      HTML(paste0(
        "Computes pairwise genetic distances between all population pairs: ",
        "<b>F<sub>ST</sub></b> (Weir &amp; Cockerham 1984), ",
        "<b>F<sub>ST</sub>-ENA</b> (FreeNA null-allele correction; Chapuis &amp; Estoup 2007), ",
        "<b>D<sub>CSE</sub></b> (Cavalli-Sforza &amp; Edwards 1967 chord distance) and ",
        "<b>D<sub>CSE</sub>-INA</b> (chord distance on ENA-corrected frequencies). ",
        "95% confidence intervals are obtained by <b>bootstrap over loci</b> (FSTAT 2.9.4 convention). ",
        "Geographic distances (Haversine, km) are added automatically when GPS columns are present. ",
        "<br><br>",
        "The <b>Mantel test</b> (Tab 2) accepts either the table computed above or an external ",
        "column-format file (same layout as RT and FSTAT\u00a02.9.4): ",
        "Pop1, Pop2, distance\u00a01, distance\u00a02, \u2026 ",
        "Permutation uses <b>label randomisation</b>, which correctly handles ",
        "<b>rectangular (incomplete) matrices</b>. ",
        "Statistics available: Pearson\u00a0<i>r</i> or regression slope\u00a0<i>b</i> ",
        "(Rousset\u00a01997 convention). ",
        "<br><br>",
        "<b>References:</b> Weir &amp; Cockerham 1984 <em>Evolution</em>. | ",
        "Cavalli-Sforza &amp; Edwards 1967 <em>Am\u00a0J\u00a0Hum\u00a0Genet</em>. | ",
        "Chapuis &amp; Estoup 2007 <em>Mol\u00a0Biol\u00a0Evol</em>. | ",
        "Rousset 1997 <em>Genetics</em>. | ",
        "S\u00e9r\u00e9 <em>et\u00a0al.</em>\u00a02017 <em>Heredity</em>."
      ))
    ),

    tabBox(
      width = 12, id = ns("main_tabs"),

      # ── Tab 1: Pairwise genetic distances ──────────────────────────────────
      tabPanel(
        title = tagList(icon("calculator"), " Pairwise Genetic Distances"),
        value = "tab_gd",

        fluidRow(

          # ── Parameters ──────────────────────────────────────────────────
          box(
            width = 3,
            title = div(
              style = "background:#FFFFFF; padding:10px; color:#333a43; font-weight:600;",
              icon("sliders-h"), " Parameters"
            ),
            solidHeader = TRUE, status = "primary",

            textInput(ns("null_code"), "Null allele code:", value = "999999",
                      placeholder = "e.g. 999999, 0, NA"),

            numericInput(ns("n_boot_loci"), "Bootstrap replicates (loci):",
                         value = 1000, min = 100, max = 20000, step = 100),

            sliderInput(ns("conf_level"), "Confidence level (%):",
                        min = 80, max = 99, value = 95, step = 1),

            tags$hr(),
            checkboxInput(ns("use_gps"),
                          "Add geographic distances (requires GPS columns)",
                          value = TRUE),

            tags$hr(),
            tags$label(class = "control-label",
                       "Restrict to populations (leave empty = all):"),
            uiOutput(ns("pop_selector_ui")),

            tags$hr(),
            actionButton(
              ns("run_gd"), "Compute Distances",
              icon  = icon("calculator"),
              class = "btn-action-primary btn-block",
              style = "font-weight:bold;"
            )
          ),

          # ── Results ─────────────────────────────────────────────────────
          box(
            width = 9,
            title = div(
              style = "background:#FFFFFF; padding:10px; color:#333a43; font-weight:600;",
              icon("table"), " Pairwise distance table"
            ),
            solidHeader = TRUE, status = "primary",

            fluidRow(
              column(3, valueBoxOutput(ns("box_npops"),  width = NULL)),
              column(3, valueBoxOutput(ns("box_npairs"), width = NULL)),
              column(3, valueBoxOutput(ns("box_nloci"),  width = NULL)),
              column(3, valueBoxOutput(ns("box_nboot"),  width = NULL))
            ),

            tags$hr(),

            DT::DTOutput(ns("gd_table")),

            tags$br(),
            fluidRow(
              column(8,
                downloadButton(ns("dl_gd_csv"), "Download CSV",
                               class = "btn-action-secondary btn-sm"),
                tags$span("\u00a0\u00a0"),
                downloadButton(ns("dl_gd_txt"), "Download TSV",
                               class = "btn-action-secondary btn-sm")
              ),
              column(4, style = "text-align:right;",
                actionButton(ns("send_to_mantel"),
                             tagList(icon("arrow-right"), " Send to Mantel Test"),
                             class = "btn btn-warning btn-sm",
                             style = "font-weight:600;")
              )
            )
          )
        )
      ),

      # ── Tab 2: Mantel test ────────────────────────────────────────────────
      tabPanel(
        title = tagList(icon("project-diagram"), " Mantel Test"),
        value = "tab_mantel",

        fluidRow(

          # ── Parameters ──────────────────────────────────────────────────
          box(
            width = 4,
            title = div(
              style = "background:#FFFFFF; padding:10px; color:#333a43; font-weight:600;",
              icon("sliders-h"), " Mantel parameters"
            ),
            solidHeader = TRUE, status = "primary",

            radioButtons(
              ns("mantel_source"), "Data source:",
              choices = c(
                "Use computed pairwise table above" = "computed",
                "Upload external column-format file" = "upload"
              ),
              selected = "computed"
            ),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'upload'", ns("mantel_source")),
              fileInput(
                ns("mantel_file"),
                "File (CSV, TSV or TXT):",
                accept = c(".csv", ".txt", ".tsv"),
                placeholder = "Pop1, Pop2, dist1, dist2, \u2026"
              ),
              radioButtons(
                ns("mantel_sep"), "Separator:",
                choices  = c("Tab" = "\t", "Comma" = ",", "Semicolon" = ";"),
                selected = "\t", inline = TRUE
              ),
              checkboxInput(ns("mantel_header"), "File has header row", value = TRUE)
            ),

            tags$hr(),

            tags$div(
              style = "font-size:12px; color:#555; margin-bottom:6px;",
              "Column assignments (auto-populated after data loads):"
            ),
            uiOutput(ns("mantel_pop1_ui")),
            uiOutput(ns("mantel_pop2_ui")),
            uiOutput(ns("mantel_x_ui")),
            uiOutput(ns("mantel_y_ui")),

            tags$hr(),

            radioButtons(
              ns("mantel_stat"), "Test statistic:",
              choices = c(
                "Pearson r (correlation)"      = "r",
                "Regression slope b (Rousset)" = "b"
              ),
              selected = "r"
            ),

            numericInput(ns("n_perm_mantel"), "Permutations:",
                         value = 9999, min = 99, max = 99999, step = 1000),

            tags$hr(),

            actionButton(
              ns("run_mantel"), "Run Mantel Test",
              icon  = icon("random"),
              class = "btn-action-primary btn-block",
              style = "font-weight:bold;"
            )
          ),

          # ── Results ─────────────────────────────────────────────────────
          box(
            width = 8,
            title = div(
              style = "background:#FFFFFF; padding:10px; color:#333a43; font-weight:600;",
              icon("chart-bar"), " Mantel results"
            ),
            solidHeader = FALSE,

            fluidRow(
              column(3, valueBoxOutput(ns("box_m_stat"),  width = NULL)),
              column(3, valueBoxOutput(ns("box_m_pval"),  width = NULL)),
              column(3, valueBoxOutput(ns("box_m_n"),     width = NULL)),
              column(3, valueBoxOutput(ns("box_m_r2"),    width = NULL))
            ),

            tags$hr(),

            fluidRow(
              column(7,
                tags$h5("Permutation distribution",
                        style = "font-weight:600; color:#2c3e50; margin-bottom:4px;"),
                plotly::plotlyOutput(ns("mantel_hist"), height = "220px"),
                tags$br(),
                tags$h5("Scatter plot",
                        style = "font-weight:600; color:#2c3e50; margin-bottom:4px;"),
                plotly::plotlyOutput(ns("mantel_scatter"), height = "220px")
              ),
              column(5,
                tags$div(
                  style = "font-size:12px; line-height:1.8; margin-top:6px;",
                  tags$p(tags$strong("How to read")),
                  tags$p(
                    "The observed statistic (", tags$em("r"), " or ", tags$em("b"),
                    ") is compared to the distribution produced by randomly ",
                    "permuting population labels across all pairs."
                  ),
                  tags$p(
                    tags$strong("p-value"), "\u00a0= proportion of permuted statistics \u2265 ",
                    "observed (one-sided; positive association). ",
                    "One-sided p for negative association = 1\u2212p."
                  ),
                  tags$p(
                    tags$strong("Label permutation"), " is used (not row-shuffle), ",
                    "so missing pairs in rectangular matrices are handled correctly."
                  ),
                  tags$p(
                    tags$strong("Rectangular matrices"), " arise when only specific ",
                    "pairs are tested (e.g., contemporaneous pairs only, or ",
                    "excluding certain combinations)."
                  ),
                  tags$hr(),
                  tags$p(
                    style = "color:#777; font-size:11px;",
                    "Manly 2018. RT. | Goudet 2002. FSTAT\u00a02.9.4. | ",
                    "Rousset 1997. Genetics."
                  )
                )
              )
            ),

            tags$hr(),

            tags$h5("Data loaded into Mantel test",
                    style = "font-weight:600; color:#2c3e50; margin-bottom:4px;"),
            DT::DTOutput(ns("mantel_preview_table")),
            tags$br(),
            downloadButton(ns("dl_mantel_data"), "Download Mantel input data",
                           class = "btn-action-secondary btn-sm")
          )
        )
      )
    )
  )
}