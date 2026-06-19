# ui_genetic_distances.R
# Tab: Pairwise Genetic Distances & Mantel Test
#
# Computes FST (WC84), FST-ENA (FreeNA), DCSE, DCSE-INA between all
# population pairs, with 95% CI by bootstrap over loci (FSTAT convention).
# Mantel test on rectangular column-format matrices via label permutation.

isolation_by_distance_UI <- function(id) {

  ns <- NS(id)

  custom_css <- tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500;600&family=IBM+Plex+Sans:wght@300;400;500;600&display=swap');

    .mt-module * { font-family: 'IBM Plex Sans', sans-serif; }

    .mt-header {
      background: linear-gradient(135deg, #0f172a 0%, #1e293b 55%, #4c1d95 100%);
      border-radius: 10px; padding: 1.2rem 1.6rem; margin-bottom: 1rem;
      position: relative; overflow: hidden;
    }
    .mt-header::before {
      content: ''; position: absolute; inset: 0;
      background: repeating-linear-gradient(
        -45deg, transparent, transparent 28px,
        rgba(255,255,255,.018) 28px, rgba(255,255,255,.018) 29px);
    }
    .mt-header-title { font-size:1.05rem; font-weight:600; color:#f1f5f9; letter-spacing:.01em; margin-bottom:.2rem; }
    .mt-header-sub   { font-size:.75rem; color:#94a3b8; font-family:'IBM Plex Mono',monospace; }
    .mt-badges { display:flex; gap:6px; margin-top:.5rem; flex-wrap:wrap; }
    .mt-badge  { display:inline-block; border-radius:20px; padding:2px 10px; font-size:.67rem; font-family:'IBM Plex Mono',monospace; }
    .mt-badge-purple { background:rgba(167,139,250,.15); border:1px solid rgba(167,139,250,.3); color:#a78bfa; }
    .mt-badge-teal   { background:rgba(20,184,166,.15);  border:1px solid rgba(20,184,166,.3);  color:#2dd4bf; }
    .mt-badge-amber  { background:rgba(251,191,36,.12);  border:1px solid rgba(251,191,36,.3);  color:#fbbf24; }

    .mt-vbox-row { display:flex; gap:9px; margin-bottom:1rem; flex-wrap:wrap; }
    .mt-vbox { flex:1; min-width:130px; background:#fff; border:1px solid #e2e8f0; border-radius:9px; padding:.6rem .85rem; display:flex; align-items:center; gap:9px; }
    .mt-vbox-icon  { width:30px; height:30px; border-radius:7px; display:flex; align-items:center; justify-content:center; font-size:12px; flex-shrink:0; }
    .mt-vbox-label { font-size:10px; color:#94a3b8; text-transform:uppercase; letter-spacing:.06em; margin-bottom:1px; }
    .mt-vbox-val   { font-size:18px; font-weight:600; color:#0f172a; line-height:1.1; font-family:'IBM Plex Mono',monospace; }

    .mt-panel { background:#fff; border:1px solid #e2e8f0; border-radius:9px; margin-bottom:.85rem; overflow:hidden; }
    .mt-panel-head { background:#f8fafc; border-bottom:1px solid #e2e8f0; padding:.55rem .9rem; }
    .mt-panel-title { font-size:12px; font-weight:600; color:#1e293b; display:flex; align-items:center; gap:6px; flex-wrap:wrap; }
    .mt-panel-body { padding:.85rem; }

    .mt-info { background:#eff6ff; border:1px solid #bfdbfe; border-radius:7px; padding:.45rem .8rem; font-size:11.5px; color:#1d4ed8; margin-bottom:.85rem; line-height:1.65; }
    .mt-warn { background:#fffbeb; border:1px solid #fcd34d; border-radius:7px; padding:.45rem .8rem; font-size:11.5px; color:#92400e; margin-bottom:.85rem; line-height:1.65; }

    .mt-btn-run {
      background:linear-gradient(135deg,#7c3aed,#4c1d95) !important;
      border:none !important; color:#fff !important; border-radius:7px !important;
      font-weight:600 !important; font-size:13px !important; padding:7px 22px !important;
      box-shadow:0 2px 8px rgba(124,58,237,.3) !important;
    }
    .mt-btn-run:hover { opacity:.9; }

    .mt-result-box {
      background:#faf5ff; border:1px solid #d8b4fe; border-radius:8px;
      padding:.65rem 1rem; font-size:11.5px; color:#3b0764;
      font-family:'IBM Plex Mono',monospace; line-height:1.9;
      margin-top:.5rem;
    }
    .mt-result-box strong { color:#6d28d9; }

    .mt-module .dataTables_wrapper { font-size:12px; }
    .mt-module table.dataTable thead th {
      background:#f8fafc !important; color:#475569 !important;
      font-family:'IBM Plex Mono',monospace !important;
      font-size:10.5px !important; font-weight:600 !important;
    }
    .mt-module table.dataTable tbody td {
      font-family:'IBM Plex Mono',monospace !important;
      font-size:11px !important; color:#1e293b !important;
    }
  "))

  tags$div(class = "mt-module", custom_css,

    tags$div(class = "mt-header",
      tags$div(class = "mt-header-title",
        icon("project-diagram"), " Mantel Test \u2014 Rectangular Matrices"),
      tags$div(class = "mt-header-sub",
        "Label-permutation Mantel test \u00b7 Manly (2018) RT \u00b7 Fstat 2.9.4 convention \u00b7 ",
        "Rousset (1997) regression statistic"),
      tags$div(class = "mt-badges",
        tags$span(class = "mt-badge mt-badge-purple", "Label permutation"),
        tags$span(class = "mt-badge mt-badge-teal",   "Rectangular-matrix safe"),
        tags$span(class = "mt-badge mt-badge-amber",  "Reuses existing distance table \u2014 no recomputation")
      )
    ),

    tags$div(class = "mt-info",
      icon("info-circle"), " ",
      "This module performs ", tags$strong("only"), " the Mantel permutation test. ",
      "Genetic distances (F", tags$sub("ST"), ", F", tags$sub("ST"), "-ENA, D", tags$sub("CSE"),
      ", D", tags$sub("CSE"), "-INA) and their bootstrap CIs are taken as-is from the ",
      tags$strong("Null Allele Estimation"), " module \u2014 nothing is recalculated here."
    ),

    tags$div(class = "mt-vbox-row",
      tags$div(class = "mt-vbox",
        tags$div(class="mt-vbox-icon", style="background:#e0f2fe;color:#0369a1;", icon("table")),
        tags$div(tags$div(class="mt-vbox-label","Pairs available"),
                 tags$div(class="mt-vbox-val", uiOutput(ns("vb_npairs_avail"))))),
      tags$div(class = "mt-vbox",
        tags$div(class="mt-vbox-icon", style="background:#dcfce7;color:#166534;", icon("link")),
        tags$div(tags$div(class="mt-vbox-label","Pairs used"),
                 tags$div(class="mt-vbox-val", uiOutput(ns("vb_npairs_used"))))),
      tags$div(class = "mt-vbox",
        tags$div(class="mt-vbox-icon", style="background:#f3e8ff;color:#7e22ce;", icon("chart-line")),
        tags$div(tags$div(class="mt-vbox-label","Statistic"),
                 tags$div(class="mt-vbox-val", uiOutput(ns("vb_stat"))))),
      tags$div(class = "mt-vbox",
        tags$div(class="mt-vbox-icon", style="background:#fef9c3;color:#854d0e;", icon("check-circle")),
        tags$div(tags$div(class="mt-vbox-label","p-value (one-sided)"),
                 tags$div(class="mt-vbox-val", uiOutput(ns("vb_pval"))))),
      tags$div(class = "mt-vbox",
        tags$div(class="mt-vbox-icon", style="background:#ccfbf1;color:#0d9488;", icon("percentage")),
        tags$div(tags$div(class="mt-vbox-label","R\u00b2"),
                 tags$div(class="mt-vbox-val", uiOutput(ns("vb_r2")))))
    ),

    fluidRow(

      # ── Data source ────────────────────────────────────────────────────
      column(4,
        tags$div(class = "mt-panel",
          tags$div(class = "mt-panel-head",
            tags$div(class = "mt-panel-title", icon("database"), " (1) Data source")),
          tags$div(class = "mt-panel-body",

            radioButtons(ns("mt_source"), NULL,
              choices = c(
                "Pairwise table from Null Alleles module" = "computed",
                "Upload external column file"             = "upload"
              ),
              selected = "computed"
            ),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'upload'", ns("mt_source")),
              fileInput(ns("mt_file"), "File (Pop1, Pop2, dist1, dist2, ...):",
                        accept = c(".csv",".txt",".tsv")),
              radioButtons(ns("mt_sep"), "Separator:",
                choices = c("Tab"="\t","Comma"=",","Semicolon"=";"),
                selected="\t", inline=TRUE),
              checkboxInput(ns("mt_header"), "File has header row", value = TRUE)
            ),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'computed'", ns("mt_source")),
              tags$hr(),
              checkboxInput(ns("mt_use_extra"),
                "Add an extra distance file (e.g. geographic, ecological, temporal)",
                value = FALSE),
              conditionalPanel(
                condition = sprintf("input['%s'] == true", ns("mt_use_extra")),
                fileInput(ns("mt_extra_file"),
                  "Extra file (first 2 cols = Pop1, Pop2 IDs):",
                  accept = c(".csv",".txt",".tsv")),
                radioButtons(ns("mt_extra_sep"), "Separator:",
                  choices = c("Tab"="\t","Comma"=",","Semicolon"=";"),
                  selected="\t", inline=TRUE),
                checkboxInput(ns("mt_extra_header"), "File has header row", value = TRUE),
                tags$p(style="color:#777;font-size:11px;",
                  "Merged on normalised (Pop1, Pop2) pair \u2014 order-independent.")
              )
            ),

            tags$hr(),
            tags$div(style="font-size:12px; color:#555; margin-bottom:6px;",
                     "Column assignment:"),
            uiOutput(ns("col_pop1_ui")),
            uiOutput(ns("col_pop2_ui")),
            uiOutput(ns("col_x_ui")),
            uiOutput(ns("col_y_ui"))
          )
        )
      ),

      # ── Parameters + results ──────────────────────────────────────────
      column(8,
        tags$div(class = "mt-panel",
          tags$div(class = "mt-panel-head",
            tags$div(class = "mt-panel-title", icon("sliders-h"), " (2) Mantel parameters")),
          tags$div(class = "mt-panel-body",
            fluidRow(
              column(4,
                radioButtons(ns("mt_stat"), "Test statistic:",
                  choices = c("Pearson r (correlation)"      = "r",
                              "Regression slope b (Rousset)" = "b"),
                  selected = "r"),
                checkboxInput(ns("mt_log_x"), "ln(transform) X", value = FALSE),
                checkboxInput(ns("mt_log_y"), "ln(transform) Y", value = FALSE)
              ),
              column(4,
                numericInput(ns("mt_n_perm"), "Permutations:",
                             value = 9999, min = 99, max = 200000, step = 1000),
                textInput(ns("mt_exclude"),
                  "Exclude pairs (optional, 'ID1-ID2', comma-separated):",
                  value = "")
              ),
              column(4,
                tags$div(style="margin-top:6px;font-size:11px;color:#64748b;",
                  icon("info-circle"), " Excluded pairs demonstrate rectangular-matrix ",
                  "support: other pairs sharing the same sub-samples are kept.")
              )
            ),
            actionButton(ns("run_mantel"),
              tagList(icon("random"), tags$strong("  Run Mantel Test")),
              class = "mt-btn-run btn"),
            uiOutput(ns("ui_mantel_status"))
          )
        ),

        tags$div(class = "mt-panel",
          tags$div(class = "mt-panel-head",
            tags$div(class = "mt-panel-title", icon("chart-area"), " Results")),
          tags$div(class = "mt-panel-body",
            fluidRow(
              column(6,
                tags$h5("Permutation distribution",
                        style="font-weight:600;color:#2c3e50;margin-bottom:4px;"),
                plotly::plotlyOutput(ns("mt_hist"), height = "260px")),
              column(6,
                tags$h5("Scatter plot",
                        style="font-weight:600;color:#2c3e50;margin-bottom:4px;"),
                plotly::plotlyOutput(ns("mt_scatter"), height = "260px"))
            ),
            uiOutput(ns("ui_mantel_summary"))
          )
        )
      )
    ),

    tags$div(class = "mt-panel",
      tags$div(class = "mt-panel-head",
        tags$div(class = "mt-panel-title", icon("table"), " Data loaded into the test")),
      tags$div(class = "mt-panel-body",
        DT::DTOutput(ns("dt_preview")),
        tags$br(),
        downloadButton(ns("dl_mantel_data"), "Download data used", class="btn btn-default btn-sm"),
        tags$br(), tags$br(),
        tags$h5("Pairs used in the last Mantel run",
                style="font-weight:600;color:#2c3e50;margin-bottom:4px;"),
        DT::DTOutput(ns("dt_pairs_used")),
        downloadButton(ns("dl_pairs_used"), "Download pairs used", class="btn btn-default btn-sm")
      )
    )
  )
}