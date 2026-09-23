# mod_subdivision_ui.R

mod_subdivision_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$head(gs_head()),

    module_banner("sitemap", "Population Subdivision · FST · G-test",""),
    
    # tags$div(class = "spg-method-note", style = "border-left-color:#B40F20;",
    #   HTML(paste0(
    #     "Population subdivision: allele-frequency differences among populations (FST > 0). ",
    #     "HS and HT are also computed here \u2014 see the <b>Diversities</b> tab for locus bootstrap. ",
    #     "<br><br>",
    #     "<b>H<sub>0</sub> (FST):</b> FST = 0 (no differentiation). &nbsp;",
    #     "<b>Bootstrap:</b> population-block resampling; percentile CI. &nbsp;",
    #     "<b>Permutation (FST):</b> genotypes randomly reassigned among populations; one-sided test.",
    #     "<br>",
    #     "<b>H<sub>0</sub> (G-test):</b> allele frequencies homogeneous across populations. &nbsp;",
    #     "<b>Permutation (G):</b> complete multilocus genotypes (whole individuals) reassigned at random ",
    #     "among populations \u2014 the valid scheme when Hardy-Weinberg is <em>not</em> assumed within samples ",
    #     "(Goudet et al. 1996). ",
    #     "Two one-sided p-values are reported: p<sub>\u2265</sub> = (b + 1)/(m + 1) with ",
    #     "b = #{G<sub>perm</sub> &ge; G<sub>obs</sub>}, and p<sub>&gt;</sub> with b = #{G<sub>perm</sub> &gt; G<sub>obs</sub>}.",
    #     "<br><b>Only individuals with a complete multilocus genotype</b> (not missing at ALL loci ",
    #     "simultaneously) are used, for every locus and for the permutation, so the combined test across ",
    #     "loci stays valid (Goudet et al. 1996 \u00a77.1, note 1). ",
    #     "N<sub>geno</sub> can therefore be lower than the number of individuals with non-missing data at a single locus considered in isolation. ",
    #     "<br><b>Two bootstrap schemes are available</b> for FST/HS/HT: resampling <b>subsamples</b> (populations, as whole blocks \u2014 ",
    #     "the default table below) and resampling <b>loci</b> (with replacement across the locus set \u2014 see the ",
    #     "dedicated table further down)."
    #   ))
    # ),

    # ==========================================================#
    # SECTION 1 — FST : Bootstrap CI + permutation
    # ==========================================================#
    fluidRow(
      box(
        width = 12,
        title = div(style = "background-color: #FFFFFF; padding: 10px; color: #333a43; font-weight: 600;",
                    # icon("sitemap"),
                    "FST: CI & p-value parameters"),
        solidHeader = TRUE, status = "primary",
        fluidRow(
          column(3,
            h4(icon("sliders"), "Parameters"),
            numericInput(ns("n_perm_fst"),     "Number of Permutations:",        value = 5000, min = 100,  max = 20000, step = 100),
            numericInput(ns("n_boot_fst"),     "Number of Bootstrap Replicates:", value = 5000, min = 100,  max = 20000, step = 100),
            numericInput(ns("conf_level_fst"), "Confidence Level:",               value = 0.95, min = 0.80, max = 0.99,  step = 0.01),
            downloadButton(ns("run_FST_Analysis"), " Run",
                         icon  = icon("rocket"),
                         class = "btn-action-primary btn-block",
                         style = "font-weight: bold;"),
            tags$small(
              style = "color: #666; margin-top: 6px; display: block;",
              icon("info-circle"),
              "Also computes HS, HT and locus bootstrap (see Genetic diversities tab)."
            )
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

    # ==========================================================#
    # SECTION 2 — G-test : permutation test de subdivision
    # ==========================================================#
    h2("G-based Permutation Test \u2014 Subdivision", class = "section-title"),
    # tags$p(HTML(paste0(
    #   "G statistic (log-likelihood ratio) per locus, built on the alleles \u00d7 populations contingency ",
    #   "table (same formula as in the LD test; Sokal & Rohlf 1981). ",
    #   "Global test: G<sub>global</sub> = &Sigma; G<sub>locus</sub> (additive property). ",
    #   "<br>Permutation: <b>complete multilocus genotypes</b> (whole individuals) reassigned at random ",
    #   "among populations \u2014 the valid scheme when Hardy-Weinberg is not assumed within samples ",
    #   "(Goudet et al. 1996 \u00a77.1), equivalent to \u201cNOT assuming random mating within samples\u201d. ",
    #   "Two one-sided p-values per locus: ",
    #   "p<sub>\u2265</sub> = (b + 1)/(m + 1) with b = #{G<sub>perm</sub> &ge; G<sub>obs</sub>}, ",
    #   "and p<sub>&gt;</sub> with b = #{G<sub>perm</sub> &gt; G<sub>obs</sub>}.",
    #   "<br>This test permutes whole individuals <b>among subsamples</b> \u2014 it is not a bootstrap and does not ",
    #   "resample loci; the loci-resampling bootstrap is reported in the FST section above."
    # )), style = "font-size: 16px; line-height: 1.5; color: #2c3e50;"),

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
            downloadButton(ns("run_G_test"), " Run",
                         icon  = icon("rocket"),
                         class = "btn-action-primary btn-block",
                         style = "font-weight: bold;")
            # tags$small(
            #   style = "color: #666; margin-top: 6px; display: block;",
            #   icon("info-circle"),
            #   "10,000 permutations recommended for a stable p-value. Minimum 1,000. ",
            #   "Global G = sum of the per-locus G values. Permutation is over subsamples ",
            #   "(whole individuals reassigned among populations), not over loci."
            # )
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
