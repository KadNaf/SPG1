# ui_allele_frequencies.R

ui_allele_frequencies <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    tags$head(gs_head()),
    
    module_banner("table", "Allele Frequencies",""),
    
    # tags$div(class = "spg-method-note", style = "border-left-color:#78B7C5;",
    #   HTML(paste0(
    #     "<b>Allele frequency analysis</b> following the Fstat format. ",
    #     "Displays allele frequencies for each marker across populations, ",
    #     "with sample sizes (N genotyped, N missing) and diversity indices ",
    #     "per locus-population combination.",
    #     "<br><br>",
    #     "<b>Diversity indices reported:</b>",
    #     "<ul style='margin:4px 0 0 16px;'>",
    #     "<li><b>Na</b>: Number of alleles</li>",
    #     "<li><b>Ne</b>: Effective number of alleles</li>",
    #     "<li><b>He</b>: Expected heterozygosity (gene diversity)</li>",
    #     "<li><b>Ho</b>: Observed heterozygosity</li>",
    #     "<li><b>Fis</b>: Inbreeding coefficient (per locus-population)</li>",
    #     "</ul>",
    #     "Allele frequencies include zeros for missing alleles."
    #   ))
    # ),
    
    fluidRow(
      box(
        width = 12,
        title = div(style = "background-color: #FFFFFF; padding: 10px; color: #333a43; font-weight: 600;",
                    icon("chart-pie"),
                    "Allele Frequency Analysis Parameters"),
        solidHeader = TRUE, status = "primary",
        fluidRow(
          column(4,
            h4(icon("filter"), "Selection"),
            selectInput(ns("fstat_population"), "Population:",
              choices = c("All populations" = "all"), multiple = FALSE),
            selectizeInput(ns("fstat_marker"), "Marker:",
              choices = NULL, multiple = FALSE,
              options = list(placeholder = "Select a marker")),
            br(),
            downloadButton(ns("update_fstat"),
              label = "Run",
              icon = icon("rocket"),
              class = "btn-action-primary btn-block", style = "font-weight: bold;")
          )
        )
      )
    )
  )
}
