#' Splash Screen UI
#'
#' Landing page shown before the main application.
#' Matches the screenshot: dark blue background, title, authors,
#' institution logos, Documentation button and Launch button.
#'
#' @return A \code{shiny::fluidPage} object.
#' @export
splash_ui <- function() {

  # Resource paths (same as app_ui so logos resolve whether called first or not)
  shiny::addResourcePath(
    "spg_www",
    system.file("app/www", package = "shinypopgen")
  )

  logos <- spg_logo_uris()   # bundled base64 helpers already in the package

  shiny::fluidPage(
    title = "ShinyPopGen",

    # ── Head: reset + dark-blue full-viewport background ─────────────────────
    shiny::tags$head(
      shiny::tags$title("ShinyPopGen"),
      shiny::tags$meta(charset = "UTF-8"),
      shiny::tags$meta(name = "viewport",
                       content = "width=device-width, initial-scale=1"),
      shiny::tags$style(shiny::HTML("

        /* ── Reset & full-viewport dark-blue background ── */
        html, body {
          margin: 0; padding: 0;
          width: 100%; height: 100%;
          background-color: #0d1b3e;
          font-family: 'Helvetica Neue', 'Segoe UI', Arial, sans-serif;
          color: #FFFFFF;
          overflow: hidden;           /* single viewport, no scroll */
        }
        .container-fluid { padding: 0 !important; }

        /* ── Outer wrapper: full viewport, flex column ── */
        .spg-splash {
          display: flex;
          flex-direction: column;
          justify-content: space-between;
          align-items: stretch;
          width: 100vw;
          height: 100vh;
          padding: 36px 48px 28px 48px;
          box-sizing: border-box;
          background: linear-gradient(160deg, #0a1530 0%, #0d1b3e 55%, #112255 100%);
        }

        /* ── Top section: title block + icon ── */
        .spg-splash-top {
          display: flex;
          justify-content: space-between;
          align-items: flex-start;
        }
        .spg-splash-title-block { flex: 1; }

        .spg-splash-title {
          font-size: 2.4rem;
          font-weight: 700;
          color: #4fc3f7;
          margin: 0 0 18px 0;
          line-height: 1.15;
        }
        .spg-splash-subtitle {
          font-size: 1.25rem;
          font-weight: 500;
          color: #4fc3f7;
          margin: 0 0 28px 0;
          line-height: 1.5;
          max-width: 540px;
        }
        .spg-splash-authors {
          font-size: 0.95rem;
          color: #7ec8e3;
          line-height: 2.0;
          margin: 0;
        }

        /* ── Top-right: app icon + Documentation button ── */
        .spg-splash-right {
          display: flex;
          flex-direction: column;
          align-items: center;
          gap: 14px;
          flex-shrink: 0;
          margin-left: 32px;
        }
        .spg-splash-appicon {
          width: 120px;
          height: 120px;
          border-radius: 50%;
          background: rgba(79, 195, 247, 0.15);
          border: 2px solid rgba(79, 195, 247, 0.35);
          display: flex;
          align-items: center;
          justify-content: center;
          font-size: 3.2rem;
          color: #4fc3f7;
        }
        .spg-splash-doc-btn {
          background: #00e5c8 !important;
          color: #0d1b3e !important;
          border: none !important;
          border-radius: 6px !important;
          font-weight: 700 !important;
          font-size: 0.95rem !important;
          padding: 8px 18px !important;
          cursor: pointer !important;
          text-decoration: none !important;
          display: flex;
          align-items: center;
          gap: 6px;
          transition: background 0.18s, transform 0.12s;
          white-space: nowrap;
        }
        .spg-splash-doc-btn:hover {
          background: #FFFFFF !important;
          transform: translateY(-1px);
          color: #0d1b3e !important;
          text-decoration: none !important;
        }
        /* PDF icon below doc button */
        .spg-splash-pdf-icon {
          font-size: 2.4rem;
          color: rgba(255,255,255,0.20);
        }

        /* ── Middle: partner logos ── */
        .spg-splash-logos {
          display: flex;
          align-items: center;
          justify-content: center;
          flex-wrap: wrap;
          gap: 32px;
          padding: 0 16px;
        }
        .spg-splash-logos img {
          max-height: 60px;
          max-width: 180px;
          object-fit: contain;
          filter: brightness(1.0);
        }
        /* Text fallback when logo image unavailable */
        .spg-logo-text {
          font-size: 1.4rem;
          font-weight: 800;
          letter-spacing: 1px;
          color: #FFFFFF;
        }

        /* ── Bottom: Launch button ── */
        .spg-splash-bottom {
          display: flex;
          align-items: center;
          justify-content: flex-start;
        }
        .spg-launch-btn {
          background: #00e5c8 !important;
          color: #0d1b3e !important;
          border: none !important;
          border-radius: 6px !important;
          font-weight: 700 !important;
          font-size: 1.1rem !important;
          padding: 12px 36px !important;
          cursor: pointer !important;
          display: flex;
          align-items: center;
          gap: 10px;
          transition: background 0.18s, transform 0.12s, box-shadow 0.18s;
          box-shadow: 0 4px 18px rgba(0, 229, 200, 0.25);
        }
        .spg-launch-btn:hover {
          background: #FFFFFF !important;
          transform: translateY(-2px);
          box-shadow: 0 8px 28px rgba(0, 229, 200, 0.35);
          color: #0d1b3e !important;
        }
        .spg-launch-btn .fa { font-size: 1.1rem; }

      "))
    ),

    # ── Splash layout ─────────────────────────────────────────────────────────
    shiny::div(
      class = "spg-splash",

      # ── TOP: title block (left) + icon/doc (right) ─────────────────────────
      shiny::div(
        class = "spg-splash-top",

        # Left: title, subtitle, authors
        shiny::div(
          class = "spg-splash-title-block",
          shiny::tags$h1(class = "spg-splash-title",
            "ShinyPopGen V1 (SPG)"
          ),
          shiny::tags$p(class = "spg-splash-subtitle",
            "A Versatile, user-friendly and multi-OS application",
            shiny::tags$br(),
            "to analyse population genetic data"
          ),
          shiny::tags$p(class = "spg-splash-authors",
            shiny::HTML(paste0(
              "Programming: Vincent Manzanilla and Naffiou Kaderi<br>",
              "Conception: Thierry de Mee\u00fbs<br>",
              "Intertryp, Univ Montpellier, Cirad, IRD, Montpellier, France"
            ))
          )
        ),

        # Right: round icon + Documentation button + PDF icon
        shiny::div(
          class = "spg-splash-right",

          # Round icon (uses package logo if available, else DNA icon)
          shiny::div(
            class = "spg-splash-appicon",
            if (!is.null(logos$intertryp))
              shiny::tags$img(src = logos$intertryp, height = "80px",
                              alt = "ShinyPopGen",
                              style = "border-radius:50%; object-fit:cover;")
            else
              shiny::icon("dna")
          ),

          # Documentation button (links to package vignette / PDF if bundled)
          shiny::tags$a(
            class  = "spg-splash-doc-btn",
            href   = "https://forge.ird.fr/intertryp/shiny_pop_gen",
            target = "_blank",
            shiny::icon("file-pdf"), "Documentation"
          ),

          # Subtle PDF icon below button
          shiny::div(
            class = "spg-splash-pdf-icon",
            shiny::icon("file-pdf")
          )
        )
      ),

      # ── MIDDLE: partner logos ──────────────────────────────────────────────
      shiny::div(
        class = "spg-splash-logos",

        # Intertryp
        shiny::tags$a(
          href = "https://umr-intertryp.cirad.fr/en", target = "_blank",
          style = "text-decoration:none;",
          if (!is.null(logos$intertryp))
            shiny::tags$img(src = logos$intertryp, alt = "Intertryp")
          else
            shiny::tags$span(class = "spg-logo-text", "INTERTRYP")
        ),

        # IRD
        shiny::tags$a(
          href = "https://www.ird.fr/en", target = "_blank",
          style = "text-decoration:none;",
          if (!is.null(logos$ird))
            shiny::tags$img(src = logos$ird, alt = "IRD")
          else
            shiny::tags$span(class = "spg-logo-text", "IRD")
        ),

        # Université de Montpellier (use bundled asset if present, else text)
        if (!is.null(logos$um))
          shiny::tags$a(
            href = "https://www.umontpellier.fr/en/", target = "_blank",
            style = "text-decoration:none;",
            shiny::tags$img(src = logos$um, alt = "Université de Montpellier")
          )
        else
          shiny::tags$a(
            href = "https://www.umontpellier.fr/en/", target = "_blank",
            style = "text-decoration:none;",
            shiny::tags$span(class = "spg-logo-text",
              style = "color:#E63946;",
              "UNIVERSIT\u00c9 DE MONTPELLIER")
          ),

        # CIRAD
        shiny::tags$a(
          href = "https://www.cirad.fr/en", target = "_blank",
          style = "text-decoration:none;",
          if (!is.null(logos$cirad))
            shiny::tags$img(src = logos$cirad, alt = "CIRAD")
          else
            shiny::tags$span(class = "spg-logo-text", "CIRAD")
        )
      ),

      # ── BOTTOM: Launch button ──────────────────────────────────────────────
      shiny::div(
        class = "spg-splash-bottom",
        shiny::actionButton(
          inputId = "btn_launch",
          label   = shiny::tagList(shiny::icon("rocket"), " Launch"),
          class   = "spg-launch-btn"
        )
      )
    )
  )
}
