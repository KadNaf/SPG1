#' Landing Page UI
#' @export
landing_ui <- function() {
  logos <- spg_logo_uris()
  
  shiny::tags$head(
    shiny::tags$style(shiny::HTML("
      body {
        margin: 0;
        background: linear-gradient(135deg, #1a237e 0%, #0d1642 100%);
        min-height: 100vh;
        font-family: 'Helvetica Neue', Arial, sans-serif;
        color: #ffffff;
      }
      .landing-container {
        max-width: 1200px;
        margin: 0 auto;
        padding: 60px 40px;
        min-height: 100vh;
        display: flex;
        flex-direction: column;
        justify-content: space-between;
      }
      .landing-title {
        font-size: 2.8rem;
        color: #64ffda;
        margin: 0 0 15px 0;
      }
      .landing-subtitle {
        font-size: 1.3rem;
        color: #8892b0;
        margin: 0;
      }
      .landing-credits {
        margin-top: 30px;
        font-size: 0.95rem;
        color: #8892b0;
        line-height: 1.8;
      }
      .landing-credits strong { color: #ccd6f6; }
      .landing-center {
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        flex: 1;
        gap: 40px;
      }
      .intertryp-logo {
        background: white;
        padding: 15px 25px;
        border-radius: 8px;
      }
      .intertryp-logo img { height: 80px; }
      .landing-buttons {
        display: flex;
        gap: 20px;
      }
      .btn-launch {
        background: #64ffda;
        color: #0d1642;
        border: none;
        padding: 16px 40px;
        font-size: 1.1rem;
        font-weight: 700;
        border-radius: 4px;
        cursor: pointer;
        transition: all 0.3s;
      }
      .btn-launch:hover {
        background: #ffffff;
        transform: translateY(-2px);
      }
      .btn-doc {
        background: transparent;
        color: #64ffda;
        border: 2px solid #64ffda;
        padding: 16px 40px;
        font-size: 1.1rem;
        font-weight: 700;
        border-radius: 4px;
        cursor: pointer;
        text-decoration: none;
        display: inline-flex;
        align-items: center;
        gap: 10px;
      }
      .btn-doc:hover {
        background: rgba(100, 255, 218, 0.1);
      }
      .landing-footer {
        display: flex;
        justify-content: center;
        gap: 40px;
        padding-top: 40px;
        border-top: 1px solid rgba(100, 255, 218, 0.2);
      }
      .landing-footer img { height: 60px; }
    "))
  )
  
  shiny::div(
    class = "landing-container",
    shiny::div(
      class = "landing-header",
      shiny::tags$h1(class = "landing-title", "ShinyPopGen V1 (SPG)"),
      shiny::tags$p(
        class = "landing-subtitle",
        "A Versatile, user-friendly and multi-OS application to analyse population genetic data"
      ),
      shiny::div(
        class = "landing-credits",
        shiny::tags$p(shiny::HTML("<strong>Programming:</strong> Vincent Manzanilla and Naffiou Kaderi")),
        shiny::tags$p(shiny::HTML("<strong>Conception:</strong> Thierry de Meeûs")),
        shiny::tags$p("Intertryp, Univ Montpellier, Cirad, IRD, Montpellier, France")
      )
    ),
    shiny::div(
      class = "landing-center",
      shiny::div(
        class = "intertryp-logo",
        if (!is.null(logos$intertryp))
          shiny::tags$img(src = logos$intertryp, alt = "INTERTRYP")
      ),
      shiny::div(
        class = "landing-buttons",
        shiny::actionButton("launch_app", "Launch", class = "btn-launch", 
                           icon = shiny::icon("rocket")),
        shiny::tags$a(href = "#", class = "btn-doc", 
                     shiny::icon("file-pdf"), "Documentation")
      )
    ),
    shiny::div(
      class = "landing-footer",
      if (!is.null(logos$ird)) 
        shiny::tags$img(src = logos$ird, alt = "IRD"),
      if (!is.null(logos$umontpellier)) 
        shiny::tags$img(src = logos$umontpellier, alt = "UM"),
      if (!is.null(logos$cirad)) 
        shiny::tags$img(src = logos$cirad, alt = "CIRAD")
    )
  )
}