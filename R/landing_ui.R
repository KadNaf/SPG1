# =========================================================
# 1. PAGE DE GARDE (landing page)
# =========================================================

landing_ui <- function() {

  shiny::tagList(

    shiny::tags$head(
      shiny::tags$style(shiny::HTML("

        html, body {
          margin:0;
          padding:0;
          width:100%;
          height:100%;
          overflow:hidden;
          font-family: Arial, Helvetica, sans-serif;
          background:#001F6B;
        }

        .spg-cover {
          width:100vw;
          height:100vh;
          background:#001F6B;
          position:relative;
          color:white;
          overflow:hidden;
        }

        .spg-title {
          position:absolute;
          top:20px;
          left:10px;
          color:#11C5FF;
        }

        .spg-title h1 {
          margin:0;
          font-size:52px;
          font-weight:300;
        }

        .spg-title h2 {
          margin-top:20px;
          font-size:28px;
          font-weight:300;
          line-height:1.4;
          width:900px;
        }

        .spg-authors {
          position:absolute;
          left:10px;
          top:260px;
          color:#00E676;
          font-size:22px;
          line-height:1.6;
        }

        .spg-dna {
          position:absolute;
          top:20px;
          right:40px;
          width:170px;
          height:170px;
          border-radius:50%;
          background:#6C63FF;
          display:flex;
          align-items:center;
          justify-content:center;
        }

        .spg-dna img {
          width:110px;
        }

        .spg-doc {
          position:absolute;
          top:210px;
          right:40px;
          width:230px;
          height:110px;
          background:#12F0D0;
          border-radius:20px;
          text-align:center;
          color:black;
          font-size:24px;
          padding-top:10px;
          cursor:pointer;
        }

        .spg-doc img {
          width:50px;
          margin-top:5px;
        }

        .spg-center-logo {
          position:absolute;
          top:360px;
          left:50%;
          transform:translateX(-50%);
        }

        .spg-center-logo img {
          width:260px;
        }

        .spg-bottom {
          position:absolute;
          bottom:80px;
          width:100%;
          display:flex;
          justify-content:space-around;
          align-items:center;
        }

        .spg-bottom img {
          height:90px;
        }

        .launch-btn {
          position:absolute;
          left:20px;
          bottom:20px;
          background:#12F0D0 !important;
          color:black !important;
          border:none !important;
          border-radius:10px !important;
          font-size:24px !important;
          padding:10px 30px !important;
          font-weight:bold !important;
        }

      "))
    ),

    shiny::div(
      class = "spg-cover",

      # TITRE
      shiny::div(
        class = "spg-title",

        shiny::tags$h1("ShinyPopGen V1 (SPG)"),

        shiny::tags$h2(
          "A Versatile, user-friendly and multi-OS application",
          shiny::tags$br(),
          "to analyse population genetic data"
        )
      ),

      # AUTEURS
      shiny::div(
        class = "spg-authors",

        shiny::div("Programming: Vincent Manzanilla and Naffiou Kaderi"),
        shiny::div("Conception: Thierry de Meeûs"),
        shiny::div("Intertryp, Univ Montpellier, Cirad, IRD, Montpellier, France")
      ),

      # ADN
      shiny::div(
        class = "spg-dna",

        shiny::tags$img(
          src = "spg_www/shinypopgen_logo.svg"
        )
      ),

      # DOCUMENTATION
      shiny::div(
        class = "spg-doc",

        "Documentation",

        shiny::tags$br(),

        shiny::tags$img(
          src = "spg_www/pdf_icon.png"
        )
      ),

      # LOGO CENTRE
      shiny::div(
        class = "spg-center-logo",

        shiny::tags$img(
          src = "spg_www/intertryp.png"
        )
      ),

      # LOGOS BAS
      shiny::div(
        class = "spg-bottom",

        shiny::tags$img(src = "spg_www/ird.png"),
        shiny::tags$img(src = "spg_www/univ_montpellier.png"),
        shiny::tags$img(src = "spg_www/cirad.png")
      ),

      # BOUTON LAUNCH
      shiny::actionButton(
        inputId = "launch_app",
        label = "Launch",
        icon = shiny::icon("rocket"),
        class = "launch-btn"
      )
    )
  )
}