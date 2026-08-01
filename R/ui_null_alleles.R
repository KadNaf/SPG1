# module/ui_null_alleles.R
# Null allele frequency estimation (EM), FST-ENA, DCSE-INA
# Simplified UI per supervisor feedback:
#   - Radio buttons for missing genotype coding, default = 000000
#   - Single bootstrap panel: n replicates + CI level choice
#   - 4 automatic output files
#
# References:
#   Dempster, Laird & Rubin (1977)  — EM algorithm
#   Chapuis & Estoup (2007)         — FreeNA: ENA and INA corrections
#   Weir (1996)                     — FST following Genepop method
#   Cavalli-Sforza & Edwards (1967) — Chord genetic distance (DCSE)

null_alleles_UI <- function(id) {
  ns <- NS(id)

  custom_css <- tags$style(HTML("
    /* Palette aligned with the rest of ShinyPopGen (see app_ui.R):
       #333a43 navy (primary/buttons), #8ea1b9 grey-blue (secondary),
       #6B64EF purple (accent/links), #B40F20 red (warnings/FST),
       #3B9AB2 teal (diversities), #EBCC2A amber (LD).
       No external font import: uses the app's own font stack so the module
       renders identically offline / inside the Docker image. */

    .na-module { font-family: 'Helvetica Neue', 'Segoe UI', Arial, sans-serif; }
    .na-module .na-mono {
      font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
    }

    /* ── Value boxes ─────────────────────────────────────────────────── */
    .na-vbox-row { display:flex; gap:9px; margin-bottom:1rem; flex-wrap:wrap; }
    .na-vbox { flex:1; min-width:110px; background:#fff; border:1px solid #e2e8f0; border-radius:6px; padding:.6rem .85rem; display:flex; align-items:center; gap:9px; box-shadow:0 1px 3px rgba(0,0,0,0.04); }
    .na-vbox-icon  { width:30px; height:30px; border-radius:6px; display:flex; align-items:center; justify-content:center; font-size:12px; flex-shrink:0; }
    .na-vbox-label { font-size:10px; color:#8ea1b9; text-transform:uppercase; letter-spacing:.06em; margin-bottom:1px; }
    .na-vbox-val   { font-size:18px; font-weight:600; color:#333a43; line-height:1.1; font-family:ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; }

    /* ── Panels ──────────────────────────────────────────────────────── */
    .na-panel { background:#fff; border:1px solid #e2e8f0; border-radius:6px; margin-bottom:.85rem; overflow:hidden; }
    .na-panel-head { background:#FFFFFF; border-bottom:2px solid #f0f0f0; padding:.6rem .9rem; }
    .na-panel-title { font-size:13px; font-weight:600; color:#333a43; display:flex; align-items:center; gap:6px; flex-wrap:wrap; }
    .na-panel-body { padding:.85rem; }

    /* ── Info strips (same visual family as .spg-method-note) ────────── */
    .na-info { background:#f8f9fb; border:1px solid #d8dbe6; border-left:4px solid #6B64EF; border-radius:4px; padding:.5rem .9rem; font-size:11.5px; color:#2c3e50; margin-bottom:.85rem; line-height:1.65; }
    .na-warn { background:#fdf6ec; border:1px solid #f0d9a8; border-left:4px solid #EBCC2A; border-radius:4px; padding:.5rem .9rem; font-size:11.5px; color:#6b4c1e; margin-bottom:.85rem; line-height:1.65; }

    /* ── Locus coding grid — radio buttons ───────────────────────────── */
    .na-locus-grid { display:flex; flex-wrap:wrap; gap:8px; margin-top:.5rem; }
    .na-locus-item {
      background:#f8fafc; border:1px solid #e2e8f0; border-radius:6px;
      padding:.45rem .7rem; min-width:160px; flex:1;
    }
    .na-locus-item .control-label { display:none; } /* hide redundant label */
    .na-locus-name {
      font-size:11px; font-weight:700; color:#333a43;
      font-family:ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; margin-bottom:3px;
    }
    .na-locus-item .radio { margin:2px 0; }
    .na-locus-item .radio label { font-size:11px; color:#475569; }

    /* ── Bootstrap result ────────────────────────────────────────────── */
    .na-boot-result {
      background:#f8f9fb; border:1px solid #d8dbe6; border-left:4px solid #6B64EF; border-radius:4px;
      padding:.65rem 1rem; font-size:11.5px; color:#2c3e50;
      font-family:ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; line-height:1.9;
      margin-top:.75rem;
    }
    .na-boot-result strong { color:#6B64EF; }

    /* ── Matrix table ────────────────────────────────────────────────── */
    .na-matrix-wrap { overflow-x:auto; margin-top:.5rem; }
    .na-matrix { border-collapse:collapse; font-size:11px; font-family:ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; width:100%; }
    .na-matrix th { background:#f8fafc; color:#475569; font-weight:600; padding:4px 9px; border:1px solid #e2e8f0; font-size:10.5px; white-space:nowrap; }
    .na-matrix td { padding:4px 9px; border:1px solid #e2e8f0; color:#333a43; text-align:right; white-space:nowrap; font-size:11px; }
    .na-matrix tr:nth-child(even) td { background:#f8fafc; }
    .na-matrix .diag  { background:#f1f5f9 !important; color:#94a3b8; text-align:center; }
    .na-matrix .upper { color:#cbd5e1; text-align:center; }
    .na-matrix .lbl   { font-weight:700; color:#333a43; text-align:left; white-space:nowrap; }

    /* ── Download row ────────────────────────────────────────────────── */
    .na-dl-row { display:flex; gap:6px; flex-wrap:wrap; margin-top:.5rem; }
    .na-dl-row .btn { font-size:11px; padding:3px 12px; }

    /* ── DT tweaks ───────────────────────────────────────────────────── */
    .na-module .dataTables_wrapper { font-size:12px; }
    .na-module table.dataTable thead th {
      background:#f8fafc !important; color:#475569 !important;
      font-family:ui-monospace, SFMono-Regular, Menlo, Consolas, monospace !important;
      font-size:10.5px !important; font-weight:600 !important;
    }
    .na-module table.dataTable tbody td {
      font-family:ui-monospace, SFMono-Regular, Menlo, Consolas, monospace !important;
      font-size:11px !important; color:#333a43 !important;
    }
    .na-module .nav-tabs > li > a { font-size:12px; font-weight:500; color:#475569; padding:5px 13px; }
    .na-module .nav-tabs > li.active > a { color:#333a43; font-weight:600; }
  "))

  # ── Shared download row ──────────────────────────────────────────────────
  dlrow <- function(...) tags$div(class="na-dl-row", ...)

  tags$div(class="na-module", custom_css,

    # ── Header (shared component — same as every other module) ─────────────
    module_banner("atom", "Null Allele Estimation \u2014 FST-ENA \u00b7 DCSE-INA",
      "EM algorithm (Dempster, Laird & Rubin 1977) \u00b7 FreeNA (Chapuis & Estoup 2007) \u00b7 Bootstrap CI over loci & sub-samples",
      "#8D8680"),
    tags$div(class = "spg-method-note", style = "border-left-color:#8D8680;",
      HTML(paste0(
        "Null allele frequencies (<b>EM algorithm</b>) are estimated per locus \u00d7 population, ",
        "then used to correct two summary statistics for their presence: ",
        "<b>FST-ENA</b> (Excluding Null Alleles \u2014 Weir 1996 / Genepop method, corrected following Chapuis & Estoup 2007) ",
        "and <b>DCSE-INA</b> (Cavalli-Sforza & Edwards 1967 chord distance, with the null allele added as an extra allelic state). ",
        "<br>Confidence intervals come from two independent bootstraps: <b>over loci</b> (fast, vectorised) ",
        "and <b>over sub-samples</b> (resampling individuals within each population, re-running the EM algorithm each time)."
      ))
    ),

    # ── Value boxes ─────────────────────────────────────────────────────────
    tags$div(class="na-vbox-row",
      tags$div(class="na-vbox",
        tags$div(class="na-vbox-icon",style="background:rgba(107,100,239,0.12);color:#6B64EF;",icon("dna")),
        tags$div(tags$div(class="na-vbox-label","Loci"),
                 tags$div(class="na-vbox-val",uiOutput(ns("vb_loci"))))),
      tags$div(class="na-vbox",
        tags$div(class="na-vbox-icon",style="background:rgba(142,161,185,0.18);color:#333a43;",icon("map-marker-alt")),
        tags$div(tags$div(class="na-vbox-label","Populations"),
                 tags$div(class="na-vbox-val",uiOutput(ns("vb_pops"))))),
      tags$div(class="na-vbox",
        tags$div(class="na-vbox-icon",style="background:rgba(107,100,239,0.12);color:#6B64EF;",icon("users")),
        tags$div(tags$div(class="na-vbox-label","Individuals"),
                 tags$div(class="na-vbox-val",uiOutput(ns("vb_n"))))),
      tags$div(class="na-vbox",
        tags$div(class="na-vbox-icon",style="background:rgba(235,204,42,0.18);color:#8a7413;",icon("percentage")),
        tags$div(tags$div(class="na-vbox-label","Avg p_nulls"),
                 tags$div(class="na-vbox-val",uiOutput(ns("vb_avg_null"))))),
      tags$div(class="na-vbox",
        tags$div(class="na-vbox-icon",style="background:rgba(180,15,32,0.10);color:#B40F20;",icon("exclamation-triangle")),
        tags$div(tags$div(class="na-vbox-label","Max p_nulls"),
                 tags$div(class="na-vbox-val",uiOutput(ns("vb_max_null"))))),
      tags$div(class="na-vbox",
        tags$div(class="na-vbox-icon",style="background:rgba(59,154,178,0.15);color:#3B9AB2;",icon("chart-bar")),
        tags$div(tags$div(class="na-vbox-label","Global FST-ENA"),
                 tags$div(class="na-vbox-val",uiOutput(ns("vb_fst_ena")))))
    ),

    # ════════════════════════════════════════════════════════════════════════
    # SETUP PANEL — 3 user choices only
    # ════════════════════════════════════════════════════════════════════════
    tags$div(class="na-panel",
      tags$div(class="na-panel-head",
        tags$div(class="na-panel-title",
          icon("sliders-h"), " Setup — 3 parameters to configure")),
      tags$div(class="na-panel-body",

        # ── (1) Missing genotype coding per locus (your choice, suggested) ──
        tags$div(class="na-warn",
          icon("exclamation-triangle"), " ",
          tags$strong("(1) Missing genotype coding per locus — your choice"),
          tags$br(),
          tags$span(style="font-size:11px;",
            "For each locus, choose how its blanks should enter the EM algorithm: ",
            tags$strong("000000"), " — treat blanks as uninformative absent / PCR failure, or ",
            tags$strong("999999"), " — treat blanks as informative null homozygotes.",
            tags$br(),
            "A suggestion is pre-selected for each locus based on what your data actually contains ",
            "(does it carry a literal 999999-style genotype anywhere for that locus?), but the final ",
            "choice is always yours — pick the other option if you know better for a given locus.",
            tags$br(),
            "Tick ", tags$strong("\"Flag for recoding sensitivity check\""),
            " to mark a locus for your own reference (exported as the ", tags$code("Recode"),
            " column) — this does not change the computation."
          )
        ),
        uiOutput(ns("locus_coding_ui")),

        tags$hr(style="margin:1rem 0;"),

        # ── (2) Bootstrap parameters ────────────────────────────────────────
        tags$strong("(2) Bootstrap parameters", style="font-size:12px; color:#1e293b;"),
        tags$br(), tags$br(),
        fluidRow(
          column(4,
            numericInput(ns("nboot"),
              label = "Number of replicates (bootstrap over loci):",
              value = 5000, min = 100, max = 99999, step = 1000)),
          column(4,
            numericInput(ns("nboot_subs"),
              label = "Number of replicates (bootstrap over sub-samples):",
              value = 500, min = 50, max = 20000, step = 50)),
          column(4,
            selectInput(ns("ci_level"),
              label = "Confidence interval level:",
              choices = c(
                "99.99% (alpha = 0.0001)" = "0.0001",
                "99.9%  (alpha = 0.001)"  = "0.001",
                "99%    (alpha = 0.01)"   = "0.01",
                "95%    (alpha = 0.05)"   = "0.05",
                "90%    (alpha = 0.10)"   = "0.10"
              ),
              selected = "0.05"))
        ),
        tags$div(class="na-info", style="margin-top:.5rem;",
          icon("info-circle"), " ",
          tags$strong("Bootstrap over loci"), " is vectorised — 5\u202f000 reps run in a few seconds, regardless of value.",
          tags$br(),
          tags$strong("Bootstrap over sub-samples"), " re-runs the EM algorithm for every (replicate \u00d7 locus \u00d7 population) ",
          "combination, so it has its own, smaller replicate count by default. Each EM run inside this bootstrap is also capped ",
          "at ", tags$strong("100 iterations"), " (instead of 5\u202f000) since a bootstrap replicate doesn't need the same precision ",
          "as the main point estimate \u2014 this keeps the wall-clock time reasonable without changing the statistical method.",
          tags$br(),
          tags$strong("Note: "), "the sub-samples bootstrap CI can occasionally sit slightly above the observed FST ",
          "(i.e. the point estimate falls just under its own lower bound). This is a known property of resampling ",
          "individuals \u2014 duplicated individuals in a replicate raise its apparent structure \u2014 not a computation error."
        ),

        tags$hr(style="margin:1rem 0;"),

        # ── (3) Run computation ─────────────────────────────────────────────
        tags$strong("(3) Run all computations + generate output files",
                    style="font-size:12px; color:#1e293b;"),
        tags$br(), tags$br(),
        fluidRow(
          column(4,
            actionButton(ns("run_all"),
              label = tagList(icon("play"), tags$strong("  Compute + Bootstrap + Export")),
              class = "btn-action-primary btn",
              width = "100%"))
        ),
        br(),
        uiOutput(ns("ui_run_status"))
      )
    ),

    # ════════════════════════════════════════════════════════════════════════
    # OUTPUT FILES PANEL
    # ════════════════════════════════════════════════════════════════════════
    tags$div(class="na-panel",
      tags$div(class="na-panel-head",
        tags$div(class="na-panel-title",
          icon("file-download"), " Output files — automatically generated after computation")),
      tags$div(class="na-panel-body",
        tags$div(class="na-info",
          icon("info-circle"), " ",
          "All four files are generated automatically when you click Compute above.",
          " Each file includes the method, references, locus coding, and bootstrap parameters."
        ),
        fluidRow(
          # File 1
          column(3,
            tags$div(class="na-panel", style="border-color:#c9c6f7;",
              tags$div(class="na-panel-head", style="background:#f4f3fe;",
                tags$div(class="na-panel-title", style="color:#6B64EF;",
                  icon("file-alt"), " File 1 — Null allele frequencies")),
              tags$div(class="na-panel-body", style="font-size:11px;color:#475569;",
                "p_nulls per locus \u00d7 population",
                tags$br(), "Global weighted mean per locus",
                tags$br(), "Locus coding reminder",
                tags$br(), br(),
                uiOutput(ns("ui_dl_file1"))
              )
            )
          ),
          # File 2
          column(3,
            tags$div(class="na-panel", style="border-color:#a9d3dc;",
              tags$div(class="na-panel-head", style="background:#eef6f8;",
                tags$div(class="na-panel-title", style="color:#3B9AB2;",
                  icon("chart-bar"), " File 2 — Global FST & FST-ENA")),
              tags$div(class="na-panel-body", style="font-size:11px;color:#475569;",
                "Per locus + multilocus FST / FST-ENA",
                tags$br(), "CI from bootstrap over loci",
                tags$br(), "CI from bootstrap over sub-samples",
                tags$br(), "Both CIs for global values",
                uiOutput(ns("ui_dl_file2"))
              )
            )
          ),
          # File 3
          column(3,
            tags$div(class="na-panel", style="border-color:#cfcdd6;",
              tags$div(class="na-panel-head", style="background:#f5f5f7;",
                tags$div(class="na-panel-title", style="color:#333a43;",
                  icon("table"), " File 3 — Pairwise long format")),
              tags$div(class="na-panel-body", style="font-size:11px;color:#475569;",
                "FST, FST-ENA, DCSE, DCSE-INA",
                tags$br(), "Per pair of sub-samples",
                tags$br(), "All loci combined",
                tags$br(), "CI from bootstrap over loci",
                uiOutput(ns("ui_dl_file3"))
              )
            )
          ),
          # File 4
          column(3,
            tags$div(class="na-panel", style="border-color:#ecdf9d;",
              tags$div(class="na-panel-head", style="background:#fdfaef;",
                tags$div(class="na-panel-title", style="color:#8a7413;",
                  icon("th"), " File 4 — Per-locus half-matrices")),
              tags$div(class="na-panel-body", style="font-size:11px;color:#475569;",
                "FST, FST-ENA, DCSE, DCSE-INA",
                tags$br(), "Half-matrix per locus",
                tags$br(), "Per pair of sub-samples",
                tags$br(), br(),
                uiOutput(ns("ui_dl_file4"))
              )
            )
          )
        )
      )
    ),

    # ════════════════════════════════════════════════════════════════════════
    # RESULTS TABS — for visual inspection
    # ════════════════════════════════════════════════════════════════════════
    tabsetPanel(id = ns("na_tabs"), type = "tabs",

      # ── TAB 1: Null allele frequencies ────────────────────────────────── #
      tabPanel(title = tagList(icon("dna"), " Null allele frequencies"),
               value = "tab_na", br(),
        tags$div(class="na-info",
          icon("info-circle"), " ",
          "Reproduces FreeNA's own null-allele-frequency report: the EM algorithm ",
          "(Dempster, Laird & Rubin 1977) estimated per locus \u00d7 population below, ",
          "and the N-weighted per-locus summary (Av(p_nulls), Av(N_exp_blanks), ",
          "f(expBlanks), one-sided binomial test p-value, and chosen blank coding) further down."
        ),
        tags$div(class="na-panel",
          tags$div(class="na-panel-head",
            tags$div(class="na-panel-title",
              icon("list"), " p_nulls per locus \u00d7 population (EM algorithm)")),
          tags$div(class="na-panel-body",
            DT::DTOutput(ns("dt_t1")))),
        tags$br(),
        tags$div(class="na-panel",
          tags$div(class="na-panel-head",
            tags$div(class="na-panel-title",
              icon("globe"), " Per-locus summary (N-weighted mean, FreeNA report format)")),
          tags$div(class="na-panel-body",
            DT::DTOutput(ns("dt_t2"))))
      ),

      # ── TAB 2: FST & FST-ENA ──────────────────────────────────────────── #
      tabPanel(title = tagList(icon("chart-bar"), " FST / FST-ENA"),
               value = "tab_fst", br(),

        tags$div(class="na-info",
          icon("info-circle"), " ",
          tags$strong("Global multilocus FST"), " \u2014 Weir (1996) / Genepop method. ",
          tags$strong("FST-ENA"), ": EM-corrected frequencies, Excluding Null Alleles \u2014 Chapuis & Estoup (2007).",
          tags$br(),
          "Bootstrap CI over loci (resample loci with replacement) and over sub-samples ",
          "(resample individuals within each population with replacement)."
        ),

        tags$div(class="na-panel",
          tags$div(class="na-panel-head",
            tags$div(class="na-panel-title",
              icon("list"), " Per-locus FST and FST-ENA")),
          tags$div(class="na-panel-body",
            DT::DTOutput(ns("dt_fst_global")))),
        br(),

        tags$div(class="na-panel",
          tags$div(class="na-panel-head",
            tags$div(class="na-panel-title",
              icon("random"), " Bootstrap CI \u2014 Global FST and FST-ENA")),
          tags$div(class="na-panel-body",
            uiOutput(ns("ui_boot_global_fst")))),
        br(),

        tags$div(class="na-panel",
          tags$div(class="na-panel-head",
            tags$div(class="na-panel-title",
              icon("exchange-alt"), " Pairwise FST and FST-ENA \u2014 lower triangle matrix")),
          tags$div(class="na-panel-body",
            fluidRow(
              column(5,
                radioButtons(ns("fst_pair_display"), "Display:",
                  choices = c(
                    "Raw FST (uncorrected)" = "raw",
                    "FST-ENA (corrected)"   = "ena",
                    "Both side by side"     = "both"),
                  selected = "both", inline = TRUE))),
            uiOutput(ns("ui_fst_pair_matrix")))),
        br(),

        tags$div(class="na-panel",
          tags$div(class="na-panel-head",
            tags$div(class="na-panel-title",
              icon("random"), " Bootstrap CI \u2014 Pairwise FST-ENA (over loci)")),
          tags$div(class="na-panel-body",
            uiOutput(ns("ui_boot_pair_fst"))))
      ),

      # ── TAB 3: DCSE / DCSE-INA ────────────────────────────────────────── #
      tabPanel(title = tagList(icon("ruler-combined"), " DCSE / DCSE-INA"),
               value = "tab_dc", br(),

        tags$div(class="na-info",
          icon("info-circle"), " ",
          tags$strong("Cavalli-Sforza & Edwards (1967) chord distance."),
          " DCSE-INA includes the null allele as an extra state \u2014 Chapuis & Estoup (2007).",
          tags$br(),
          "DCSE(i,j) = (2/\u03c0)\u00d7\u221a[2\u00d7(1\u2212\u03a3\u221a(p_ik\u00d7p_jk))]  ",
          "INA: corrdgenefreq + null allele appended (freq = rd[locus, pop])."
        ),

        tags$div(class="na-panel",
          tags$div(class="na-panel-head",
            tags$div(class="na-panel-title",
              icon("th"), " Pairwise DCSE and DCSE-INA \u2014 lower triangle matrix")),
          tags$div(class="na-panel-body",
            fluidRow(
              column(5,
                radioButtons(ns("dc_display"), "Display:",
                  choices = c(
                    "Raw DCSE (uncorrected)" = "raw",
                    "DCSE-INA (corrected)"   = "ina",
                    "Both side by side"      = "both"),
                  selected = "both", inline = TRUE))),
            uiOutput(ns("ui_dc_matrix")))),
        br(),

        tags$div(class="na-panel",
          tags$div(class="na-panel-head",
            tags$div(class="na-panel-title",
              icon("random"), " Bootstrap CI \u2014 Pairwise DCSE-INA (over loci)")),
          tags$div(class="na-panel-body",
            uiOutput(ns("ui_boot_pair_dc"))))
      ),

      # ── TAB 4: Per-locus x pair ───────────────────────────────────────── #
      tabPanel(title = tagList(icon("table"), " Per-locus \u00d7 pair"),
               value = "tab_locus_pair", br(),

        tags$div(class="na-info",
          icon("info-circle"), " ",
          "FST, FST-ENA, DCSE and DCSE-INA for each locus \u00d7 pair of populations.",
          " Useful for detecting outlier loci."
        ),

        fluidRow(
          column(3, selectInput(ns("fl_locus"), "Locus:",
            choices = c("All loci" = "all"), selected = "all")),
          column(3, selectInput(ns("fl_pop1"), "Population 1:",
            choices = c("All pairs" = "all"), selected = "all")),
          column(3, selectInput(ns("fl_pop2"), "Population 2:",
            choices = c("All pairs" = "all"), selected = "all"))
        ),

        tags$div(class="na-panel",
          tags$div(class="na-panel-head",
            tags$div(class="na-panel-title",
              icon("list"), " FST and FST-ENA per locus \u00d7 pair")),
          tags$div(class="na-panel-body",
            DT::DTOutput(ns("dt_fst_locus")))),
        br(),

        tags$div(class="na-panel",
          tags$div(class="na-panel-head",
            tags$div(class="na-panel-title",
              icon("list"), " DCSE and DCSE-INA per locus \u00d7 pair")),
          tags$div(class="na-panel-body",
            DT::DTOutput(ns("dt_dc_locus"))))
      )

    ) # end tabsetPanel
  )   # end tags$div.na-module
}
