# module/ui_fst_ena.R
# FST-ENA (null allele correction for Fst) and DCSE-INA (corrected Cavalli-Sforza & Edwards distance)
# ENA and INA algorithms — Chapuis & Estoup (2007) / FreeNA

fst_ena_UI <- function(id) {
  ns <- NS(id)

  custom_css <- tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500;600&family=IBM+Plex+Sans:wght@300;400;500;600&display=swap');

    .fe-module * { font-family: 'IBM Plex Sans', sans-serif; }
    .fe-module .mono { font-family: 'IBM Plex Mono', monospace; }

    /* ── Header ─────────────────────────────────────────── */
    .fe-header {
      background: linear-gradient(135deg, #0f172a 0%, #1e293b 55%, #064e3b 100%);
      border-radius: 10px; padding: 1.2rem 1.6rem; margin-bottom: 1rem;
      position: relative; overflow: hidden;
    }
    .fe-header::before {
      content: ''; position: absolute; inset: 0;
      background: repeating-linear-gradient(
        -45deg, transparent, transparent 28px,
        rgba(255,255,255,.018) 28px, rgba(255,255,255,.018) 29px);
    }
    .fe-header-title {
      font-size: 1.1rem; font-weight: 600; color: #f1f5f9;
      letter-spacing: .01em; margin-bottom: .2rem;
    }
    .fe-header-sub {
      font-size: .76rem; color: #94a3b8;
      font-family: 'IBM Plex Mono', monospace;
    }
    .fe-badges { display: flex; gap: 6px; margin-top: .5rem; flex-wrap: wrap; }
    .fe-badge {
      display: inline-block; border-radius: 20px;
      padding: 2px 10px; font-size: .68rem;
      font-family: 'IBM Plex Mono', monospace;
    }
    .fe-badge-teal  { background:rgba(20,184,166,.15); border:1px solid rgba(20,184,166,.3); color:#2dd4bf; }
    .fe-badge-green { background:rgba(74,222,128,.12); border:1px solid rgba(74,222,128,.3); color:#4ade80; }
    .fe-badge-amber { background:rgba(251,191,36,.12); border:1px solid rgba(251,191,36,.3); color:#fbbf24; }
    .fe-badge-blue  { background:rgba(56,189,248,.15); border:1px solid rgba(56,189,248,.3); color:#38bdf8; }

    /* ── Value boxes ─────────────────────────────────── */
    .fe-vbox-row { display: flex; gap: 9px; margin-bottom: 1rem; flex-wrap: wrap; }
    .fe-vbox {
      flex: 1; min-width: 130px; background: #fff;
      border: 1px solid #e2e8f0; border-radius: 9px;
      padding: .65rem .9rem; display: flex; align-items: center; gap: 9px;
    }
    .fe-vbox-icon {
      width: 32px; height: 32px; border-radius: 7px;
      display: flex; align-items: center; justify-content: center;
      font-size: 13px; flex-shrink: 0;
    }
    .fe-vbox-label {
      font-size: 10px; color: #94a3b8; text-transform: uppercase;
      letter-spacing: .06em; margin-bottom: 1px;
    }
    .fe-vbox-val {
      font-size: 19px; font-weight: 600; color: #0f172a; line-height: 1.1;
      font-family: 'IBM Plex Mono', monospace;
    }

    /* ── Buttons ─────────────────────────────────────── */
    .fe-btn {
      background: linear-gradient(135deg, #0d9488, #064e3b) !important;
      border: none !important; color: #fff !important;
      border-radius: 7px !important; font-weight: 600 !important;
      font-size: 13px !important; padding: 7px 20px !important;
      box-shadow: 0 2px 8px rgba(13,148,136,.3) !important;
      transition: transform .15s, box-shadow .15s;
    }
    .fe-btn:hover {
      transform: translateY(-1px);
      box-shadow: 0 4px 14px rgba(13,148,136,.45) !important;
    }

    /* ── Panels ──────────────────────────────────────── */
    .fe-panel {
      background: #fff; border: 1px solid #e2e8f0;
      border-radius: 9px; margin-bottom: .9rem; overflow: hidden;
    }
    .fe-panel-head {
      background: #f8fafc; border-bottom: 1px solid #e2e8f0;
      padding: .6rem .95rem; display: flex; align-items: center; flex-wrap: wrap;
    }
    .fe-panel-title {
      font-size: 12.5px; font-weight: 600; color: #1e293b;
      display: flex; align-items: center; gap: 6px; flex-wrap: wrap;
    }
    .fe-panel-body { padding: .9rem; }

    /* ── Info / warn / formula strips ───────────────── */
    .fe-info {
      background: #f0fdfa; border: 1px solid #99f6e4; border-radius: 7px;
      padding: .5rem .85rem; font-size: 11.5px; color: #134e4a;
      display: flex; align-items: flex-start; gap: 7px;
      margin-bottom: .9rem; line-height: 1.75;
    }
    .fe-formula {
      background: #fafaf9; border: 1px solid #d6d3d1; border-radius: 7px;
      padding: .55rem .85rem; font-size: 11px; color: #292524;
      font-family: 'IBM Plex Mono', monospace;
      margin-bottom: .9rem; line-height: 1.8;
    }
    .fe-warn {
      background: #fffbeb; border: 1px solid #fcd34d; border-radius: 7px;
      padding: .5rem .85rem; font-size: 11.5px; color: #92400e;
      display: flex; align-items: flex-start; gap: 7px;
      margin-bottom: .9rem; line-height: 1.7;
    }

    /* ── Comparison grid for corrected vs uncorrected ── */
    .fe-compare-grid { display: flex; gap: 10px; margin-bottom: .9rem; flex-wrap: wrap; }
    .fe-compare-card {
      flex: 1; min-width: 200px; border-radius: 9px;
      border: 1px solid #e2e8f0; overflow: hidden;
    }
    .fe-compare-head {
      padding: .5rem .8rem; font-size: 11.5px; font-weight: 700;
      color: #fff; display: flex; align-items: center; gap: 6px;
    }
    .fe-compare-head-uncorr { background: #475569; }
    .fe-compare-head-corr   { background: #0d9488; }
    .fe-compare-body { padding: .6rem .8rem; background: #fff; font-size: 11px; color: #334155; line-height: 1.7; }

    /* ── Matrix table ────────────────────────────────── */
    .fe-matrix-wrap { overflow-x: auto; }
    .fe-matrix {
      border-collapse: collapse; font-size: 11.5px;
      font-family: 'IBM Plex Mono', monospace; width: 100%;
    }
    .fe-matrix th {
      background: #f8fafc; color: #475569; font-weight: 600;
      padding: 4px 10px; border: 1px solid #e2e8f0;
      font-size: 11px; white-space: nowrap;
    }
    .fe-matrix td {
      padding: 4px 10px; border: 1px solid #e2e8f0;
      color: #1e293b; text-align: right; white-space: nowrap;
    }
    .fe-matrix tr:nth-child(even) td { background: #f8fafc; }
    .fe-matrix .diag { background: #f1f5f9 !important; color: #94a3b8; }
    .fe-matrix .pop-label { font-weight: 700; color: #0f172a; text-align: left; }

    /* ── Export row ──────────────────────────────────── */
    .fe-export {
      display: flex; align-items: center; gap: 6px;
      padding-top: .55rem; border-top: 1px solid #f1f5f9; margin-top: .55rem;
    }
    .fe-export-lbl { font-size: 11px; color: #94a3b8; }

    /* ── DT tweaks ───────────────────────────────────── */
    .fe-module .dataTables_wrapper { font-size: 12px; }
    .fe-module table.dataTable thead th {
      background: #f8fafc !important; color: #475569 !important;
      font-family: 'IBM Plex Mono', monospace !important;
      font-size: 11px !important; font-weight: 600 !important;
      letter-spacing: .03em !important;
    }
    .fe-module table.dataTable tbody td {
      font-family: 'IBM Plex Mono', monospace !important;
      font-size: 11.5px !important; color: #1e293b !important;
    }
    .fe-module .nav-tabs > li > a {
      font-size: 12px; font-weight: 500; color: #475569;
      border-radius: 6px 6px 0 0; padding: 5px 14px;
    }
    .fe-module .nav-tabs > li.active > a { color: #0f172a; font-weight: 600; }
  "))

  xbtn <- function(csv_id, txt_id)
    tags$div(class = "fe-export",
      tags$span(class = "fe-export-lbl", "Export :"),
      downloadButton(ns(csv_id), "CSV",
        class = "btn btn-default btn-xs",
        style = "padding:2px 10px; font-size:11px;"),
      downloadButton(ns(txt_id), "TXT",
        class = "btn btn-default btn-xs",
        style = "padding:2px 10px; font-size:11px;"))

  tags$div(class = "fe-module",
    custom_css,

    # ── Header ────────────────────────────────────────────────
    tags$div(class = "fe-header",
      tags$div(class = "fe-header-title",
        icon("project-diagram"),
        " FST-ENA \u00b7 DCSE-INA distance — Null allele correction"),
      tags$div(class = "fe-header-sub",
        "ENA and INA algorithms \u00b7 Chapuis & Estoup (2007) \u00b7 FreeNA \u00b7 Weir (1996) \u00b7 Cavalli-Sforza & Edwards (1967)"),
      tags$div(class = "fe-badges",
        tags$span(class = "fe-badge fe-badge-teal",
          "ENA \u2014 FST corrected for null alleles"),
        tags$span(class = "fe-badge fe-badge-green",
          "INA \u2014 Corrected DCSE distance"),
        tags$span(class = "fe-badge fe-badge-amber",
          "Raw FST \u2014 Weir (1996)"),
        tags$span(class = "fe-badge fe-badge-blue",
          "Raw DCSE \u2014 Cavalli-Sforza & Edwards (1967)")
      )
    ),

    # ── Value boxes ───────────────────────────────────────────
    tags$div(class = "fe-vbox-row",
      tags$div(class = "fe-vbox",
        tags$div(class = "fe-vbox-icon",
          style = "background:#ccfbf1; color:#0d9488;", icon("dna")),
        tags$div(tags$div(class = "fe-vbox-label", "Loci"),
                 tags$div(class = "fe-vbox-val", uiOutput(ns("vb_loci"))))
      ),
      tags$div(class = "fe-vbox",
        tags$div(class = "fe-vbox-icon",
          style = "background:#dcfce7; color:#166534;", icon("map-marker-alt")),
        tags$div(tags$div(class = "fe-vbox-label", "Populations"),
                 tags$div(class = "fe-vbox-val", uiOutput(ns("vb_pops"))))
      ),
      tags$div(class = "fe-vbox",
        tags$div(class = "fe-vbox-icon",
          style = "background:#fef9c3; color:#854d0e;", icon("chart-bar")),
        tags$div(tags$div(class = "fe-vbox-label", "Raw FST"),
                 tags$div(class = "fe-vbox-val", uiOutput(ns("vb_fst_raw"))))
      ),
      tags$div(class = "fe-vbox",
        tags$div(class = "fe-vbox-icon",
          style = "background:#ccfbf1; color:#0d9488;", icon("chart-bar")),
        tags$div(tags$div(class = "fe-vbox-label", "FST-ENA"),
                 tags$div(class = "fe-vbox-val", uiOutput(ns("vb_fst_ena"))))
      ),
      tags$div(class = "fe-vbox",
        tags$div(class = "fe-vbox-icon",
          style = "background:#e0f2fe; color:#0369a1;", icon("ruler")),
        tags$div(tags$div(class = "fe-vbox-label", "DCSE-INA (mean)"),
                 tags$div(class = "fe-vbox-val", uiOutput(ns("vb_dc_ina"))))
      )
    ),

    # ── Main tabs ─────────────────────────────────────────────
    tabsetPanel(
      id = ns("fe_tabs"), type = "tabs",

      # ════════════════════════════════════════════════════════ #
      # TAB 1 — Global FST (multilocus)                         #
      # ════════════════════════════════════════════════════════ #
      tabPanel(
        title = tagList(icon("globe"), " Global FST (multilocus)"),
        value = "tab_fst_global",
        br(),

        tags$div(class = "fe-info",
          icon("info-circle"),
          tags$div(
            tags$strong("Multilocus global FST"), " — Weir (1996) / Genepop method.",
            tags$br(),
            tags$strong("Raw FST"), " : calculated on observed allele frequencies (null alleles excluded from denominator).",
            tags$br(),
            tags$strong("FST-ENA"), " : calculated on allele frequencies corrected by the EM algorithm ",
            tags$em("(Excluding Null Alleles)"), "."
          )
        ),

        tags$div(class = "fe-formula",
          tags$strong("Weir (1996) formula :"),
          tags$br(),
          "FST = S1 / S3   where   S1 = Σ_loci [ s²P × nc ]   and   S3 = Σ_loci [ (s²P + s²I + s²G) × nc ]",
          tags$br(),
          "nc = (N_tot − N_tot² / N_tot) / (r − 1)   ;   r = number of effective populations",
          tags$br(),
          tags$strong("ENA : corrected frequencies = corrdgenefreq[locus, pop, allele]   (from EM-FreeNA)")
        ),

        tags$div(class = "fe-panel",
          tags$div(class = "fe-panel-head",
            tags$div(class = "fe-panel-title",
              icon("sliders-h"), " Parameters")),
          tags$div(class = "fe-panel-body",
            fluidRow(
              column(4,
                tags$div(style = "margin-top:25px;",
                  actionButton(ns("run_fst_global"),
                    label = tagList(icon("play"), tags$strong(" Calculate")),
                    class = "fe-btn btn")))
            )
          )
        ),

        tags$div(class = "fe-compare-grid",
          tags$div(class = "fe-compare-card",
            tags$div(class = "fe-compare-head fe-compare-head-uncorr",
              icon("table"), " Raw FST — Weir (1996)"),
            tags$div(class = "fe-compare-body",
              "Observed allele frequencies, null alleles excluded from denominator.",
              tags$br(), "May be biased by the presence of null alleles."
            )
          ),
          tags$div(class = "fe-compare-card",
            tags$div(class = "fe-compare-head fe-compare-head-corr",
              icon("check-circle"), " FST-ENA — Chapuis & Estoup (2007)"),
            tags$div(class = "fe-compare-body",
              "Frequencies corrected by the EM algorithm. Null alleles are reintegrated,",
              tags$br(), "the bias due to null homozygotes is corrected."
            )
          )
        ),

        tags$div(class = "fe-panel",
          tags$div(class = "fe-panel-head",
            tags$div(class = "fe-panel-title",
              icon("list"), " Global FST multilocus — per locus")),
          tags$div(class = "fe-panel-body",
            DT::DTOutput(ns("dt_fst_global")),
            xbtn("dl_fst_global_csv", "dl_fst_global_txt")
          )
        )
      ),

      # ════════════════════════════════════════════════════════ #
      # TAB 2 — Pairwise FST                                    #
      # ════════════════════════════════════════════════════════ #
      tabPanel(
        title = tagList(icon("exchange-alt"), " Pairwise FST"),
        value = "tab_fst_pair",
        br(),

        tags$div(class = "fe-info",
          icon("info-circle"),
          tags$div(
            tags$strong("Pairwise FST"), " — Weir (1996) for each population pair.",
            tags$br(),
            "The lower triangle is displayed: raw FST (without correction) and FST-ENA (with ENA correction).",
            tags$br(),
            tags$strong("NA"), " : calculation not applicable (insufficient sample size for the pair)."
          )
        ),

        tags$div(class = "fe-panel",
          tags$div(class = "fe-panel-head",
            tags$div(class = "fe-panel-title",
              icon("sliders-h"), " Parameters")),
          tags$div(class = "fe-panel-body",
            fluidRow(
              column(4,
                radioButtons(ns("fst_pair_type"), "Display:",
                  choices = c(
                    "Raw FST (without correction)" = "raw",
                    "FST-ENA (corrected)"           = "ena",
                    "Both side by side"              = "both"
                  ), selected = "both", inline = FALSE)),
              column(3,
                tags$div(style = "margin-top:25px;",
                  actionButton(ns("run_fst_pair"),
                    label = tagList(icon("play"), tags$strong(" Calculate")),
                    class = "fe-btn btn")))
            )
          )
        ),

        tags$div(class = "fe-panel",
          tags$div(class = "fe-panel-head",
            tags$div(class = "fe-panel-title",
              icon("th"), " Pairwise FST matrix — lower triangle")),
          tags$div(class = "fe-panel-body",
            uiOutput(ns("ui_fst_pair_matrix")),
            xbtn("dl_fst_pair_csv", "dl_fst_pair_txt")
          )
        ),

        tags$div(class = "fe-panel",
          tags$div(class = "fe-panel-head",
            tags$div(class = "fe-panel-title",
              icon("list"), " Pairwise FST — long table format")),
          tags$div(class = "fe-panel-body",
            DT::DTOutput(ns("dt_fst_pair")),
            xbtn("dl_fst_pair_long_csv", "dl_fst_pair_long_txt")
          )
        )
      ),

      # ════════════════════════════════════════════════════════ #
      # TAB 3 — Pairwise DCSE distance                          #
      # ════════════════════════════════════════════════════════ #
      tabPanel(
        title = tagList(icon("ruler-combined"), " Pairwise DCSE distance"),
        value = "tab_dc",
        br(),

        tags$div(class = "fe-info",
          icon("info-circle"),
          tags$div(
            tags$strong("Cavalli-Sforza & Edwards (1967) genetic distance"),
            " — DCSE pairwise.",
            tags$br(),
            tags$strong("Raw DCSE"), " : calculated on observed frequencies (null alleles excluded).",
            tags$br(),
            tags$strong("DCSE-INA"), " : calculated by including the null allele in the corrected frequencies ",
            tags$em("(Including Null Alleles)"), "."
          )
        ),

        tags$div(class = "fe-formula",
          tags$strong("Cavalli-Sforza & Edwards (1967) formula :"),
          tags$br(),
          "DCSE(i,j) = (2/π) × √[ 2 × (1 − Σ_k √(p_ik × p_jk)) ]",
          tags$br(),
          "Mean distance over loci: mean(DCSE_locus) for valid loci (CSprod ≤ 1)",
          tags$br(),
          tags$strong("INA :"), " corrected frequencies + null allele added as an additional state (freq = rd[locus, pop])"
        ),

        tags$div(class = "fe-panel",
          tags$div(class = "fe-panel-head",
            tags$div(class = "fe-panel-title",
              icon("sliders-h"), " Parameters")),
          tags$div(class = "fe-panel-body",
            fluidRow(
              column(4,
                radioButtons(ns("dc_type"), "Display:",
                  choices = c(
                    "Raw DCSE (without correction)" = "raw",
                    "DCSE-INA (corrected)"          = "ina",
                    "Both side by side"             = "both"
                  ), selected = "both", inline = FALSE)),
              column(3,
                tags$div(style = "margin-top:25px;",
                  actionButton(ns("run_dc"),
                    label = tagList(icon("play"), tags$strong(" Calculate")),
                    class = "fe-btn btn")))
            )
          )
        ),

        tags$div(class = "fe-panel",
          tags$div(class = "fe-panel-head",
            tags$div(class = "fe-panel-title",
              icon("th"), " Pairwise DCSE matrix — lower triangle")),
          tags$div(class = "fe-panel-body",
            uiOutput(ns("ui_dc_matrix")),
            xbtn("dl_dc_csv", "dl_dc_txt")
          )
        ),

        tags$div(class = "fe-panel",
          tags$div(class = "fe-panel-head",
            tags$div(class = "fe-panel-title",
              icon("list"), " Pairwise DCSE — long table format")),
          tags$div(class = "fe-panel-body",
            DT::DTOutput(ns("dt_dc")),
            xbtn("dl_dc_long_csv", "dl_dc_long_txt")
          )
        )
      ),

      # ════════════════════════════════════════════════════════ #
      # TAB 4 — FST per locus × pair                            #
      # ════════════════════════════════════════════════════════ #
      tabPanel(
        title = tagList(icon("table"), " FST per locus × pair"),
        value = "tab_fst_locus",
        br(),

        tags$div(class = "fe-info",
          icon("info-circle"),
          tags$div(
            tags$strong("Per-locus FST"), " for each population pair.",
            tags$br(),
            "Allows identification of outlier loci and comparison",
            " of raw and ENA-corrected estimates locus by locus."
          )
        ),

        tags$div(class = "fe-panel",
          tags$div(class = "fe-panel-head",
            tags$div(class = "fe-panel-title",
              icon("sliders-h"), " Filters")),
          tags$div(class = "fe-panel-body",
            fluidRow(
              column(3,
                selectInput(ns("fl_locus"), "Locus:",
                  choices = c("All loci" = "all"), selected = "all")),
              column(3,
                selectInput(ns("fl_pop1"), "Population 1:",
                  choices = c("All pairs" = "all"), selected = "all")),
              column(3,
                selectInput(ns("fl_pop2"), "Population 2:",
                  choices = c("All pairs" = "all"), selected = "all")),
              column(3,
                tags$div(style = "margin-top:25px;",
                  actionButton(ns("run_fst_locus"),
                    label = tagList(icon("play"), tags$strong(" Calculate")),
                    class = "fe-btn btn")))
            )
          )
        ),

        tags$div(class = "fe-panel",
          tags$div(class = "fe-panel-head",
            tags$div(class = "fe-panel-title",
              icon("list"), " FST per locus × pair (raw and ENA)")),
          tags$div(class = "fe-panel-body",
            DT::DTOutput(ns("dt_fst_locus")),
            xbtn("dl_fst_locus_csv", "dl_fst_locus_txt")
          )
        )
      )
    )
  )
}