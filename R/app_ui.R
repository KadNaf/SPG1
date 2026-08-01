#' Application UI
#'
#' @return A \code{bslib::page_navbar} object.
#' @export
app_ui <- function() {

  # -- Resource paths --------------------------------------------------------
  shiny::addResourcePath(
    "spg_www",
    system.file("app/www", package = "shinypopgen")
  )
  shiny::addResourcePath(
    "sdb",
    find.package("shinydashboard")
  )

  # -- Footer logos (bundled in package, served as base64 data URIs) ---------
  logos <- spg_logo_uris()

  # -- Example data for Welcome tab ------------------------------------------

  data_one_col <- data.frame(
    Population = c("Boulouparis", "Boulouparis", "Gadji", "PortLaguerre", "Sarramea"),
    B12 = c("192/194", "200/200", "0/0", "145/145", "0/0"),
    C07 = c("145/192", "179/179", "92/100", "92/92", "92/92"),
    stringsAsFactors = FALSE
  )

  data_two_col <- data.frame(
    Population = c("Boulouparis", "Gadji", "PortLaguerre", "Sarramea"),
    B12   = c(192, 200,   0, 145),
    B12_1 = c(194, 200,   0, 145),
    C07   = c(145, 179,  92,  92),
    C07_1 = c(192, 179,  92, 100),
    stringsAsFactors = FALSE
  )

  # -- Theme -----------------------------------------------------------------

  # Wes Anderson palettes — full reference:
  #  Royal1:        #FAEFD1                          (cream page bg)
  #  IsleofDogs2:   #EAD3BF #AA9486 #B6854D #39312F #1C1718
  #  IsleofDogs1:   #9986A5 #79402E #CCBA72 #D9D0D3 #8D8680
  #  AsteroidCity1: #0A9F9D #CEB175 #E54E21 #6C8645 #C18748 (navbar)
  #  FantasticFox1: #DD8D29 #E2D200 #46ACC8 #E58601 #B40F20 (box accent, download btn)
  #  Zissou1:       #3B9AB2 #78B7C5 #EBCC2A #E1AF00 #F21A00 (significance gradient)
  #  Moonrise2:     #798E87 #C27D38 #CCC591 #29211F
  #  GrandBudapest2: #C6CDF7 (dark-mode links)
  spg_theme <- bslib::bs_theme(
    version   = 5,
    bg        = "#f5f7fa",   # light grey page background
    fg        = "#333a43",   # dark grey
    primary   = "#333a43",   # dark grey primary
    secondary = "#8ea1b9",   # medium grey-blue
    "navbar-bg"            = "#333a43",  # dark grey navbar
    "navbar-color"         = "#FFFFFF",
    "navbar-hover-color"   = "#8ea1b9",
    "navbar-active-color"  = "#FFFFFF",
    "navbar-brand-color"   = "#FFFFFF",
    "nav-link-font-size"   = "0.85rem",
    "nav-link-font-weight" = "500"
  )

  # -- Custom CSS (light + dark mode) ----------------------------------------

  custom_css <- shiny::HTML("

    /* ===== BODY & LAYOUT ===== */
    body {
      font-family: 'Helvetica Neue', 'Segoe UI', Arial, sans-serif;
      font-size: 15px;
      color: #333a43;
      background-color: #f5f7fa;
    }
    p, label, .form-label, .shiny-input-container label,
    .selectize-input, .form-control, .form-select,
    .dataTables_wrapper, .dt-container {
      font-size: 14px;
      color: #333a43;
    }

    .bslib-page-navbar > .tab-content {
      padding: 20px;
      min-height: calc(100vh - 56px);
    }

    /* ===== WES ANDERSON COLOR MAP =====
       IsleofDogs2: #FAEFD1(Royal1-bg) #EAD3BF #AA9486 #B6854D #39312F #1C1718
       IsleofDogs1: #9986A5  #79402E  #CCBA72  #D9D0D3  #8D8680
       Moonrise2:   #798E87  #C27D38  #CCC591  #29211F
       Royal2:      #9A8822  #F5CDB4  #F8AFA8  #FDDDA0  #74A089
       GrandBudapest2: #C6CDF7 (dark-mode link only)
    ===== */

    /* ===== BOX — white cards on light-grey page ===== */
    .box {
      border-radius: 4px !important;
      border-top:   none !important;
      border-left:  none !important;
      border-right: none !important;
      border-bottom: 2px solid #333a43 !important;
      box-shadow: 0 1px 6px rgba(57,49,47,0.10) !important;
      margin-bottom: 18px;
      background: #FFFFFF;                          /* white card */
    }
    .box-header {
      background: #FFFFFF !important;
      border-bottom: 1px solid #E8E0D6 !important;
      padding: 0 !important;                        /* title div provides its own padding */
      border-radius: 4px 4px 0 0 !important;
    }
    /* Fallback: plain title without inner div — restore padding */
    .box-header .box-title {
      padding: 10px 16px;
      display: block;
    }
    .box-title {
      font-size: 15px !important;
      font-weight: 600 !important;
      color: #333a43 !important;
    }
    .box-body { padding: 14px 16px !important; }

    /* h-tags */
    .box h2 {
      font-size: 17px; font-weight: 700;
      color: #333a43;
      border-bottom: 1px solid #8ea1b9;
      padding-bottom: 8px; margin-top: 16px;
    }
    .box h3 { font-size: 15px; font-weight: 600; color: #333a43; }
    .box h4 { font-size: 14px; font-weight: 600; color: #333a43; }

    a         { color: #8ea1b9; }
    a:hover   { color: #333a43; }

    /* ===== SECTION TITLE (module h2 with class=section-title) ===== */
    .section-title {
      color: #333a43 !important;
      border-bottom: 2px solid #8ea1b9 !important;
    }

    /* ===== NAVBAR — dark grey #333a43 ===== */
    .navbar {
      padding-top: 0; padding-bottom: 0; min-height: 52px;
      background-color: #333a43 !important;
      border-bottom: 1px solid #1e242b;
    }
    .navbar-brand { padding: 6px 12px 6px 0; }
    .navbar .nav-link {
      padding-top: 14px !important; padding-bottom: 14px !important;
      color: #FFFFFF !important;
      display: inline-flex !important; align-items: center; gap: 7px;
      transition: color 0.15s ease, border-bottom-color 0.15s ease;
    }
    .navbar .nav-link:hover  { color: #8ea1b9 !important; }
    .navbar .nav-link.active {
      color: #FFFFFF !important;
      font-weight: 600;
      border-bottom: 2px solid #8ea1b9;
    }
    .nav-intertryp { opacity: 0.75; font-size: 0.85rem; }
    .nav-intertryp:hover { opacity: 1; }
    .bslib-input-dark-mode {
      font-size: 1rem; padding: 14px 10px; cursor: pointer;
      color: #FFFFFF !important;
    }

    /* ===== valueBox significance gradient =====
       maroon = B40F20 dark red  (FantasticFox1 — p < 0.001)
       orange = E1AF00 amber     (Zissou1       — p < 0.05)
       aqua   = 3B9AB2 teal      (Zissou1       — not significant)
    ===== */
    .bg-maroon, .small-box.bg-maroon { background-color: #B40F20 !important; }
    .bg-maroon > .inner, .bg-maroon > .icon { background-color: #B40F20 !important; }
    .bg-orange, .small-box.bg-orange { background-color: #E1AF00 !important; }
    .bg-orange > .inner, .bg-orange > .icon { background-color: #E1AF00 !important; }
    .bg-aqua,   .small-box.bg-aqua   { background-color: #3B9AB2 !important; }
    .bg-aqua   > .inner, .bg-aqua   > .icon { background-color: #3B9AB2 !important; }

    /* ===== DARK MODE =====
       bg: IsleofDogs2 #1C1718, cards: #39312F, navbar: #29211F(Moonrise2)
       text: #EAD3BF, titles: #CCBA72(IsleofDogs1), links: #C6CDF7(GrandBudapest2)
    ===== */
    [data-bs-theme='dark'] body,
    [data-bs-theme='dark'] {
      background-color: #1C1718;                    /* IsleofDogs2 near-black */
      color: #EAD3BF;                               /* IsleofDogs2 parchment */
    }
    [data-bs-theme='dark'] .bslib-page-navbar > .tab-content {
      background-color: #1C1718;
    }
    /* Light mode page bg */
    .bslib-page-navbar > .tab-content {
      background-color: #f5f7fa;
    }
    [data-bs-theme='dark'] .navbar {
      background-color: #1e242b !important;
      border-bottom-color: #333a43 !important;
    }
    [data-bs-theme='dark'] .navbar .nav-link       { color: #8ea1b9 !important; }
    [data-bs-theme='dark'] .navbar .nav-link:hover { color: #FFFFFF !important; }
    [data-bs-theme='dark'] .navbar .nav-link.active {
      color: #FFFFFF !important;
      border-bottom-color: #8ea1b9;
    }
    [data-bs-theme='dark'] .bslib-input-dark-mode  { color: #8ea1b9 !important; }

    [data-bs-theme='dark'] .box {
      background: #39312F !important;               /* IsleofDogs2 dark brown */
      color: #EAD3BF !important;
      border-bottom-color: #8D8680 !important;      /* IsleofDogs1 taupe */
      box-shadow: 0 1px 5px rgba(0,0,0,0.50) !important;
    }
    [data-bs-theme='dark'] .box-header {
      background: #39312F !important;               /* dark mode keeps dark brown */
      border-bottom-color: #8D8680 !important;
    }
    [data-bs-theme='dark'] .box-title { color: #CCBA72 !important; }
    [data-bs-theme='dark'] .box p,
    [data-bs-theme='dark'] .box li,
    [data-bs-theme='dark'] .box label { color: #D9D0D3; }
    [data-bs-theme='dark'] .box h2 {
      color: #CCBA72; border-bottom-color: #8D8680;
    }
    [data-bs-theme='dark'] .box h3 { color: #CCBA72; }
    [data-bs-theme='dark'] .box h4 { color: #EAD3BF; }
    [data-bs-theme='dark'] a       { color: #C6CDF7; }  /* GrandBudapest2 lavender */
    [data-bs-theme='dark'] a:hover { color: #CCBA72; }  /* IsleofDogs1 gold */

    [data-bs-theme='dark'] .form-control,
    [data-bs-theme='dark'] .form-select {
      background: #29211F; color: #EAD3BF; border-color: #8D8680;
    }
    [data-bs-theme='dark'] .selectize-input,
    [data-bs-theme='dark'] .selectize-dropdown {
      background: #29211F !important; color: #EAD3BF !important;
      border-color: #8D8680 !important;
    }
    [data-bs-theme='dark'] .well {
      background: #39312F; border-color: #8D8680; color: #D9D0D3;
    }
    [data-bs-theme='dark'] table.dataTable { color: #EAD3BF; }
    [data-bs-theme='dark'] .dataTables_wrapper { color: #D9D0D3; }

    /* ===== BUTTONS ===== */
    /* Action/compute — dark grey, white text */
    .btn-action-primary, .btn-action-primary.btn-primary {
      background-color: #333a43 !important; color: #FFFFFF !important;
      border: none !important; font-weight: 600; border-radius: 3px;
      transition: transform 0.12s ease, box-shadow 0.12s ease, background-color 0.12s ease;
      box-shadow: 0 1px 2px rgba(0,0,0,0.15);
    }
    .btn-action-primary:hover { background-color: #1e242b !important; transform: translateY(-1px); box-shadow: 0 4px 10px rgba(0,0,0,0.18); }
    .btn-action-primary:active { transform: translateY(0); box-shadow: 0 1px 2px rgba(0,0,0,0.15); }
    .btn-action-primary:focus-visible { outline: 2px solid #6B64EF; outline-offset: 2px; }
    .btn-action-primary:disabled, .btn-action-primary.disabled {
      background-color: #8ea1b9 !important; opacity: 0.75; transform: none; box-shadow: none; cursor: progress;
    }

    /* All download buttons — dark grey */
    .btn-download-primary, .btn-download-secondary, .btn-download-info,
    .btn-primary, .btn-block.btn-primary,
    .btn-outline-primary {
      background-color: #333a43 !important; color: #FFFFFF !important;
      border: none !important; border-radius: 3px;
    }
    .btn-download-primary:hover, .btn-download-secondary:hover,
    .btn-download-info:hover, .btn-primary:hover, .btn-outline-primary:hover {
      background-color: #1e242b !important;
    }

    /* ===== NAV TABS & PILLS (inside modules) ===== */
    /* Active tab: dark grey bg, white text */
    .nav-tabs .nav-link.active,
    .nav-pills .nav-link.active {
      background-color: #333a43 !important;
      color: #FFFFFF !important;
      border-color: #333a43 !important;
    }
    /* Inactive tab: dark grey text, transparent bg */
    .nav-tabs .nav-link,
    .nav-pills .nav-link {
      color: #333a43 !important;
      padding: 7px 16px !important;
      transition: background-color 0.15s ease, color 0.15s ease, border-color 0.15s ease;
    }
    .nav-tabs .nav-link:hover,
    .nav-pills .nav-link:hover {
      color: #FFFFFF !important;
      background-color: #8ea1b9 !important;
      border-color: #8ea1b9 !important;
    }

    /* ===== WELCOME PAGE ===== */
    .spg-hero {
      background: linear-gradient(145deg, #1a2035 0%, #26306B 55%, #333a43 100%);
      padding: 48px 48px 40px;
      display: flex;
      align-items: center;
      justify-content: space-between;
      margin: -20px -20px 32px -20px;
      border-bottom: 3px solid #6B64EF;
    }
    .spg-hero-text {
      flex: 1;
      text-align: left;
      padding-right: 32px;
    }
    .spg-hero-logo {
      flex-shrink: 0;
      display: flex;
      align-items: center;
    }
    .spg-hero h1 {
      color: #FFFFFF !important;
      font-size: 2.8rem !important;
      font-weight: 800 !important;
      letter-spacing: -0.5px;
      margin: 0 0 8px 0 !important;
      border: none !important;
    }
    .spg-hero .spg-tagline {
      color: #A9F0D8;
      font-size: 1.1rem;
      margin: 0 0 28px 0;
      letter-spacing: 0.04em;
    }
    .spg-cta {
      display: inline-block;
      background: #A9F0D8 !important;
      color: #1a2035 !important;
      font-weight: 700 !important;
      font-size: 1rem !important;
      padding: 11px 30px !important;
      border-radius: 4px !important;
      border: none !important;
      cursor: pointer;
      text-decoration: none !important;
      transition: background 0.18s, transform 0.12s;
    }
    .spg-cta:hover {
      background: #FFFFFF !important;
      transform: translateY(-1px);
    }
    .spg-module-grid {
      display: grid;
      grid-template-columns: repeat(3, 1fr);
      gap: 14px;
      margin: 0 0 24px 0;
    }
    @media (max-width: 768px) {
      .spg-module-grid { grid-template-columns: 1fr 1fr; }
    }
    .spg-module-card {
      background: #FFFFFF;
      border-radius: 5px;
      padding: 18px 16px 14px;
      border-bottom: 3px solid #8ea1b9;
      box-shadow: 0 1px 5px rgba(51,58,67,0.08);
      transition: box-shadow 0.18s, transform 0.12s;
    }
    .spg-module-card:hover {
      box-shadow: 0 4px 14px rgba(51,58,67,0.14);
      transform: translateY(-2px);
    }
    .spg-module-card .card-icon {
      font-size: 1.5rem;
      margin-bottom: 8px;
      color: #6B64EF;
    }
    .spg-module-card h5 {
      font-size: 0.88rem !important;
      font-weight: 700 !important;
      color: #333a43 !important;
      margin: 0 0 5px 0 !important;
    }
    .spg-module-card p {
      font-size: 0.80rem !important;
      color: #666 !important;
      margin: 0 !important;
      line-height: 1.4 !important;
    }
    .spg-steps {
      counter-reset: step-counter;
      list-style: none;
      padding: 0;
      margin: 0;
    }
    .spg-steps li {
      counter-increment: step-counter;
      display: flex;
      align-items: flex-start;
      gap: 14px;
      margin-bottom: 12px;
      font-size: 14px;
    }
    .spg-steps li::before {
      content: counter(step-counter);
      min-width: 26px; height: 26px;
      background: #333a43;
      color: #FFFFFF;
      border-radius: 50%;
      display: flex; align-items: center; justify-content: center;
      font-size: 0.8rem; font-weight: 700;
    }
    /* ===== HELP PAGE ===== */
    /* Smooth in-page scroll when jumping to a #help-... anchor, scoped to
       the tab-content column so it doesn't affect the sidebar/navbar. */
    body.spg-sidebar-mode.bslib-page-navbar > .container-fluid > .tab-content {
      scroll-behavior: smooth;
    }
    .spg-help-nav {
      display: flex; flex-wrap: wrap; gap: 8px;
      margin-bottom: 24px; padding-bottom: 16px;
      border-bottom: 1px solid #e2e6ec;
    }
    .spg-help-nav a {
      display: inline-flex; align-items: center; gap: 6px;
      background: #f5f7fa; color: #333a43 !important;
      border: 1px solid #e2e6ec; border-radius: 20px;
      padding: 5px 14px; font-size: 12.5px; font-weight: 500;
      text-decoration: none !important;
      transition: background-color 0.15s ease, color 0.15s ease, border-color 0.15s ease, transform 0.12s ease;
    }
    .spg-help-nav a:hover {
      background: #333a43; color: #FFFFFF !important;
      border-color: #333a43; transform: translateY(-1px);
    }
    .spg-help-section {
      margin-bottom: 28px;
    }
    .spg-help-section h3 {
      font-size: 1rem !important;
      font-weight: 700 !important;
      color: #333a43 !important;
      border-bottom: 2px solid #8ea1b9 !important;
      padding-bottom: 6px !important;
      margin-bottom: 14px !important;
    }
    /* scroll-margin so a jumped-to section isn't hidden under the sticky
       module banner at the top of the tab-content column */
    .spg-help-section { scroll-margin-top: 16px; }
    /* FAQ accordion (details/summary — no JS dependency needed) */
    .spg-faq-item {
      border: 1px solid #e2e6ec; border-radius: 6px;
      margin-bottom: 8px; overflow: hidden; background: #fff;
    }
    .spg-faq-item summary {
      padding: 10px 14px; font-size: 13.5px; font-weight: 600;
      color: #333a43; cursor: pointer; list-style: none;
      display: flex; align-items: center; gap: 8px;
    }
    .spg-faq-item summary::-webkit-details-marker { display: none; }
    .spg-faq-item summary::before {
      content: ">"; color: #6B64EF; font-size: 12px; font-weight: 700;
      display: inline-block;
      transition: transform 0.15s ease;
    }
    .spg-faq-item[open] summary::before { transform: rotate(90deg); }
    .spg-faq-item summary:hover { background: #f5f7fa; }
    .spg-faq-body {
      padding: 2px 14px 14px 36px; font-size: 13px; color: #444b55; line-height: 1.7;
    }
    .spg-faq-body code {
      background: #f0f0f4; padding: 1px 5px; border-radius: 3px; font-size: 12px;
    }
    /* Module guide table */
    .spg-modguide-table { width: 100%; border-collapse: collapse; font-size: 12.5px; }
    .spg-modguide-table th {
      background: #333a43; color: #FFFFFF; text-align: left;
      padding: 8px 10px; font-weight: 600;
    }
    .spg-modguide-table td {
      padding: 8px 10px; border-bottom: 1px solid #e2e6ec; vertical-align: top;
    }
    .spg-modguide-table tr:nth-child(even) td { background: #f8f9fc; }
    .spg-modguide-table tr:hover td { background: #f0f0fb; }
    /* Glossary */
    .spg-glossary dt {
      font-weight: 700; color: #333a43; font-size: 13px;
      margin-top: 10px; font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
    }
    .spg-glossary dd {
      margin-left: 0; font-size: 13px; color: #444b55; line-height: 1.6; margin-bottom: 4px;
    }
    .spg-format-note {
      background: #f5f7fa;
      border-left: 4px solid #6B64EF;
      border-radius: 3px;
      padding: 10px 14px;
      font-size: 13px;
      margin-bottom: 14px;
    }
    .spg-tip {
      background: #f5f7fa;
      border-left: 4px solid #2CBF9F;
      border-radius: 3px;
      padding: 10px 14px;
      font-size: 13px;
      margin-bottom: 10px;
    }

    /* Module banner (replaces h2 section-title in each tab) */
    .spg-module-banner {
      background: linear-gradient(135deg, #1a2035 0%, #26306B 60%, #333a43 100%);
      padding: 22px 40px 18px;
      display: flex;
      align-items: center;
      gap: 22px;
      margin: -20px -20px 28px -20px;
      border-bottom: 4px solid #6B64EF;
    }
    .spg-banner-icon {
      font-size: 2.8rem;
      color: rgba(255,255,255,0.18);
      flex-shrink: 0;
    }
    .spg-banner-title {
      color: #FFFFFF !important;
      font-size: 1.75rem !important;
      font-weight: 800 !important;
      margin: 0 0 5px 0 !important;
      border: none !important;
      letter-spacing: -0.2px;
    }
    .spg-banner-subtitle {
      color: #A9F0D8;
      font-size: 0.95rem;
      margin: 0 0 10px 0;
    }
    /* Method note: styled description below banner */
    .spg-method-note {
      background: #f8f9fb;
      border-left: 4px solid #6B64EF;
      border-radius: 4px;
      padding: 14px 20px;
      margin-bottom: 20px;
      font-size: 15px;
      line-height: 1.65;
      color: #2c3e50;
    }

    /* Secondary action */
    .btn-action-secondary {
      background-color: #8ea1b9 !important; color: #FFFFFF !important;
      border: none !important; border-radius: 3px;
      transition: transform 0.12s ease, box-shadow 0.12s ease, background-color 0.12s ease;
    }
    .btn-action-secondary:hover { background-color: #333a43 !important; transform: translateY(-1px); box-shadow: 0 4px 10px rgba(0,0,0,0.18); }
    .btn-action-secondary:active { transform: translateY(0); box-shadow: none; }
    .btn-action-secondary:disabled, .btn-action-secondary.disabled {
      opacity: 0.65; transform: none; box-shadow: none; cursor: progress;
    }

    /* ── Default: navbar hidden (welcome is entry page; sidebar shows it) ── */
    .bslib-page-navbar > nav.navbar { display: none !important; }

    /* ── Left sidebar mode: embedded flex layout ─────────────────────────── */
    body.spg-sidebar-mode.bslib-page-navbar {
      display: flex !important;
      flex-direction: row !important;
      align-items: stretch !important;
      height: 100vh !important;
      gap: 0 !important;
      padding: 0 !important;
      margin: 0 !important;
    }
    body.spg-sidebar-mode.bslib-page-navbar > nav.navbar {
      display: flex !important;
      flex: 0 0 210px !important;
      width: 210px !important;
      height: 100vh !important;
      position: sticky !important;
      top: 0 !important;
      align-self: flex-start !important;
      flex-direction: column !important;
      align-items: stretch !important;
      overflow-y: auto !important;
      overflow-x: hidden !important;
      scrollbar-width: none !important;       /* Firefox: hide sidebar scrollbar */
      z-index: 100 !important;
      background: linear-gradient(180deg, #1a2035 0%, #26306B 100%) !important;
      border-right: 1px solid rgba(107,100,239,0.25) !important;
      box-shadow: 3px 0 16px rgba(0,0,0,0.18) !important;
      padding: 0 !important;
      margin: 0 !important;
    }
    /* Chrome/Safari: hide sidebar scrollbar */
    body.spg-sidebar-mode.bslib-page-navbar > nav.navbar::-webkit-scrollbar {
      display: none !important;
    }
    /* Hide brand - ShinyPopGen label is now inside the banner */
    body.spg-sidebar-mode.bslib-page-navbar > nav.navbar .navbar-brand { display: none !important; }
    /* Collapse wrapper - full-width column */
    body.spg-sidebar-mode.bslib-page-navbar > nav.navbar .navbar-collapse {
      display: flex !important;
      flex-direction: column !important;
      width: 100% !important;
      height: 100% !important;
      padding: 6px 0 !important;
    }
    body.spg-sidebar-mode.bslib-page-navbar > nav.navbar .navbar-nav {
      flex-direction: column !important;
      width: 100% !important;
    }
    body.spg-sidebar-mode.bslib-page-navbar > nav.navbar .nav-link {
      padding: 9px 16px !important;
      text-align: left !important;
      border-radius: 0 4px 4px 0 !important;
      white-space: nowrap !important;
      font-size: 0.88rem !important;
      color: rgba(255,255,255,0.82) !important;
      border-left: 3px solid transparent !important;
      display: flex !important; align-items: center; gap: 9px;
      margin: 1px 6px 1px 0 !important;
      transition: background 0.15s ease, color 0.15s ease, border-left-color 0.15s ease;
    }
    body.spg-sidebar-mode.bslib-page-navbar > nav.navbar .nav-link:hover {
      background: rgba(255,255,255,0.08) !important;
      color: #FFFFFF !important;
      border-left-color: #A9F0D8 !important;
    }
    body.spg-sidebar-mode.bslib-page-navbar > nav.navbar .nav-link.active {
      background: rgba(107,100,239,0.25) !important;
      color: #FFFFFF !important;
      border-left-color: #6B64EF !important;
      font-weight: 600 !important;
    }
    /* Dark mode toggle pushed to bottom of sidebar */
    body.spg-sidebar-mode.bslib-page-navbar > nav.navbar .navbar-nav.ms-auto {
      margin-top: auto !important;
      border-top: 1px solid rgba(255,255,255,0.1) !important;
      padding: 8px 0 !important;
    }
    /* Body locked to exactly viewport height; all scrolling happens inside
       the content column (.container-fluid > .tab-content). */
    body.spg-sidebar-mode {
      overflow: hidden !important;
      height: 100vh !important;
    }
    /* .container-fluid is the actual direct child of body (bslib wraps
       .tab-content inside it). Make it the flex child that takes the
       remaining width and clip its own overflow so the body never grows. */
    body.spg-sidebar-mode.bslib-page-navbar > .container-fluid {
      flex: 1 1 0 !important;
      min-width: 0 !important;
      padding: 0 !important;
      margin: 0 !important;
      height: 100vh !important;
      overflow: hidden !important;
    }
    /* tab-content is the scroll container; height is constrained so
       overflow-y:auto triggers a visible scrollbar when content overflows. */
    body.spg-sidebar-mode.bslib-page-navbar > .container-fluid > .tab-content {
      height: 100vh !important;
      overflow-y: auto !important;
      overflow-x: auto !important;
      padding-top: 0 !important;
    }
    /* Welcome page: thin overlay scrollbar — visible only while scrolling,
       never reserves a permanent gutter on Linux/Windows. */
    body.spg-welcome-mode {
      scrollbar-width: thin !important;
    }
    body.spg-welcome-mode::-webkit-scrollbar { width: 6px; }
    body.spg-welcome-mode::-webkit-scrollbar-track { background: transparent; }
    body.spg-welcome-mode::-webkit-scrollbar-thumb {
      background-color: rgba(51,58,67,0.20);
      border-radius: 3px;
    }
    body.spg-welcome-mode::-webkit-scrollbar-thumb:hover {
      background-color: rgba(51,58,67,0.50);
    }

    /* Tab pane: no top padding so banner is flush */
    body.spg-sidebar-mode.bslib-page-navbar .tab-content > .tab-pane {
      padding-top: 0 !important;
      margin-top: 0 !important;
    }
    /* Banner: sticky at top of its scroll container (.tab-content) */
    body.spg-sidebar-mode .spg-module-banner {
      position: sticky !important;
      top: 0 !important;
      z-index: 100 !important;
      margin-top: 0 !important;
      margin-left: -20px !important;
      margin-right: -20px !important;
    }
    /* Welcome footer logos */
    .spg-footer {
      background: #FFFFFF;
      margin: 32px -20px -20px -20px;
      padding: 24px 40px;
      display: flex;
      align-items: center;
      justify-content: center;
      gap: 48px;
      border-top: 3px solid #6B64EF;
    }
    .spg-footer img { max-height: 56px; }

  ")

  # -- Welcome tab content ---------------------------------------------------

  # Helper: one module feature card (clickable → navigates to tab)
  module_card <- function(icon_name, title, desc, accent = "#8ea1b9", tab_value = NULL) {
    onclick_js <- if (!is.null(tab_value))
      paste0("document.querySelector('[data-value=\"", tab_value, "\"]').click();")
    else NULL
    shiny::div(
      class = "spg-module-card",
      style = paste0(
        "border-bottom-color:", accent, ";",
        if (!is.null(tab_value)) "cursor:pointer;" else ""
      ),
      onclick = onclick_js,
      shiny::div(class = "card-icon", shiny::icon(icon_name)),
      shiny::tags$h5(title),
      shiny::tags$p(desc)
    )
  }

  welcome_content <- shiny::tagList(

    # ── Hero ──────────────────────────────────────────────────────────────────
    shiny::div(
      class = "spg-hero",
      # Left: text + CTA
      shiny::div(
        class = "spg-hero-text",
        shiny::HTML('<svg viewBox="0 0 510 410" height="340" xmlns="http://www.w3.org/2000/svg" aria-label="ShinyPopGen" style="display:block; margin-bottom:20px;">
          <defs>
            <linearGradient id="hero-tg" x1="0" y1="0" x2="1" y2="0">
              <stop offset="0%" stop-color="#8F86FF"/>
              <stop offset="100%" stop-color="#5AA7FF"/>
            </linearGradient>
          </defs>
          <text x="0" y="85" fill="#F4F6FF" font-size="65" font-family="Inter,Segoe UI,Roboto,Helvetica,Arial,sans-serif" font-weight="300" letter-spacing="-1.5">ShinyPopGen V1</text>
          <text x="0" y="175" fill="url(#hero-tg)" font-size="65" font-family="Inter,Segoe UI,Roboto,Helvetica,Arial,sans-serif" font-weight="500" letter-spacing="-2">SPG-V1</text>
          <line x1="0" y1="210" x2="300" y2="210" stroke="#7074D8" stroke-width="2"/>
          <text x="2" y="240" fill="#F4F6FF" font-size="18" font-family="Inter,Segoe UI,Roboto,Helvetica,Arial,sans-serif" font-weight="400">
            <tspan x="2" dy="0">A Versatile, user-friendly and multi-OS application</tspan>
            <tspan x="2" dy="22">to analyse population genetic data</tspan>
          </text>
          <rect x="2"   y="280" width="72" height="12" rx="6" fill="#6F67F5"/>
          <rect x="86"  y="280" width="72" height="12" rx="6" fill="#7F76FF"/>
          <rect x="172" y="280" width="72" height="12" rx="6" fill="#17B08B"/>
          <rect x="258" y="280" width="42" height="12" rx="6" fill="#C45D34"/>
          <rect x="314" y="280" width="72" height="12" rx="6" fill="#9A650E"/>
          <rect x="400" y="280" width="96" height="12" rx="6" fill="#5F56CA"/>
          <text x="2" y="330" fill="#787BD3" font-size="22" font-family="Inter,Segoe UI,Roboto,Helvetica,Arial,sans-serif" font-weight="400" letter-spacing="2.8">IRD \u00b7 UCAD \u00b7 CIRAD \u00b7 INTERTRYP</text>
        </svg>'),
        # Credits section
        shiny::div(
          class = "spg-credits",
          style = "margin: 10px 0 15px 0; padding: 10px 0; border-top: 1px solid rgba(255,255,255,0.08); border-bottom: 1px solid rgba(255,255,255,0.08);",
          shiny::tags$p(
            style = "color: #a0a8c0; font-size: 20px; font-family: Inter,Segoe UI,Roboto,Helvetica,Arial,sans-serif; margin: 0; line-height: 1.7;",
            shiny::tags$strong("Programming:", style = "color: #c8d0e8;"), 
            " Vincent Manzanilla and Naffiou Kadiri",
            shiny::tags$br(),
            shiny::tags$strong("Conception:", style = "color: #c8d0e8;"), 
            " Thierry de Meeûs"
          )
        ),
        shiny::tags$button(
          class   = "spg-cta",
          onclick = "document.querySelector('[data-value=\"import\"]').click();",
          shiny::icon("upload"), " Launch "
        )
      ),
      # Right: big logo
      shiny::div(
        class = "spg-hero-logo",
        shiny::tags$img(
          src   = "spg_www/shinypopgen_logo.svg",
          height = "400px",
          alt   = "ShinyPopGen logo",
          style = "filter: drop-shadow(0 8px 32px rgba(0,0,0,0.55));"
        )
      )
    ),

    # ── Module grid ───────────────────────────────────────────────────────────
    shinydashboard::box(
      width = 12, solidHeader = FALSE,
      title = shiny::div(
        style = "background:#FFFFFF; padding:10px; color:#333a43; font-weight:600;",
        shiny::icon("th"), " Analysis modules"
      ),
      shiny::div(
        class = "spg-module-grid",
        module_card("upload",      "Data Import",          "Import CSV/TXT, auto-detect columns, assign populations and markers, preview map.", "#6B64EF", "import"),
        module_card("chart-pie",   "Allele Frequencies",   "Allele frequency tables and plots per population, missing data overview.", "#2CBF9F", "allele_frequencies"),
        module_card("table",       "General Statistics",   "Na, Ne, Ho, He, sample sizes, F-statistics per allele (WC84).", "#3B9AB2", "general_stats"),
        module_card("flask",       "Local Panmixia",       "Within-population HWE. FIS per locus and population, bootstrap CI, permutation p-value.", "#9986A5", "local_panmixia"),
        module_card("globe",       "Global Panmixia",      "Overall HWE across all populations. Multilocus FIT, bootstrap CI, permutation p-value.", "#E1AF00", "global_panmixia"),
        module_card("sitemap",     "Subdivision",          "Population differentiation. FST (WC84) per locus and overall, bootstrap CI, permutation p-value.", "#B40F20", "subdivision"),
        module_card("chart-line",  "Genetic Diversities",  "HS and HT per locus. Locus bootstrap for multilocus FST, FIT, FIS, HS, HT.", "#78B7C5", "genetic_diversities"),
        module_card("link",        "Linkage Disequilibrium","Pairwise LD tests among all loci with permutation p-values.", "#EBCC2A", "linkage_desequilibrium"),
        module_card("circle-notch","Null Alleles",          "Null allele frequency estimation by locus \u00d7 population using the FreeNA EM algorithm.", "#8D8680", "null_alleles"),
        module_card("map-marker-alt","Isolation by Distance \U0001f6a7","Pairwise FST\u2044(1\u2212FST) vs geographic distance. Mantel test (Rousset 1997).", "#2CBF9F", "isolation_by_distance")
      )
    ),

    # ── Help shortcut card ───────────────────────────────────────────────────
    shiny::div(
      style = paste0(
        "cursor:pointer; display:flex; align-items:center; gap:20px;",
        "background:linear-gradient(135deg,#1a2035 0%,#26306B 60%,#333a43 100%);",
        "border-radius:6px; padding:20px 28px; margin-bottom:24px;",
        "border:1px solid rgba(107,100,239,0.35);",
        "box-shadow:0 4px 18px rgba(0,0,0,0.18);",
        "transition:box-shadow 0.18s, transform 0.12s;"
      ),
      onclick = "var el=document.querySelector('[data-value=\"help\"]'); if(el) el.click();",
      onmouseover = "this.style.boxShadow='0 8px 28px rgba(107,100,239,0.35)'; this.style.transform='translateY(-2px)';",
      onmouseout  = "this.style.boxShadow='0 4px 18px rgba(0,0,0,0.18)'; this.style.transform='';",
      shiny::div(
        style = "flex-shrink:0; width:48px; height:48px; border-radius:50%; background:rgba(107,100,239,0.25); display:flex; align-items:center; justify-content:center; font-size:1.4rem; color:#A9F0D8;",
        shiny::icon("question-circle")
      ),
      shiny::div(
        shiny::tags$p(style = "margin:0; font-size:1rem; font-weight:700; color:#FFFFFF;", "Need help?"),
        shiny::tags$p(style = "margin:0; font-size:0.85rem; color:#A9F0D8;",
          "Data format requirements, encoding options, statistical methods & key references.")
      ),
      shiny::div(
        style = "margin-left:auto; flex-shrink:0; color:rgba(169,240,216,0.7); font-size:1.2rem;",
        shiny::icon("arrow-right")
      )
    ),

    # ── Citation + contact ────────────────────────────────────────────────────
    shiny::fluidRow(
      shinydashboard::box(
        width = 8, solidHeader = FALSE,
        title = shiny::div(
          style = "background:#FFFFFF; padding:10px; color:#333a43; font-weight:600;",
          shiny::icon("book"), " Citation"
        ),
        shiny::tags$blockquote(
          style = "font-size:13px; line-height:1.7; border-left:3px solid #6B64EF; padding-left:14px; color:#555; margin:0;",
          "ShinyPopGen: an interactive Shiny application for population genetics data import, exploration, and descriptive analyses. IRD / CIRAD / INTERTRYP."
        )
      ),
      shinydashboard::box(
        width = 4, solidHeader = FALSE,
        title = shiny::div(
          style = "background:#FFFFFF; padding:10px; color:#333a43; font-weight:600;",
          shiny::icon("envelope"), " Contact"
        ),
        shiny::tags$p(
          style = "font-size:13px; margin:0;",
          "Bugs and suggestions:",
          shiny::tags$br(),
          shiny::tags$a(
            href = "https://forge.ird.fr/intertryp/shiny_pop_gen",
            target = "_blank",
            style = "color:#6B64EF; font-weight:500;",
            shiny::icon("code-branch"), " forge.ird.fr/intertryp/shiny_pop_gen"
          )
        )
      )
    ),

    # ── Partner logos footer ──────────────────────────────────────────────────
    shiny::div(
      class = "spg-footer",
      # IRD
      shiny::tags$a(
        href = "https://www.ird.fr/en", target = "_blank",
        style = "text-decoration:none;",
        if (!is.null(logos$ird))
          shiny::tags$img(src = logos$ird, height = "52px",
            alt = "IRD", title = "Institut de Recherche pour le Developpement",
            style = "opacity:0.9;")
        else
          shiny::tags$span(style = "font-weight:700;font-size:1.3rem;color:#26306B;letter-spacing:1px;", "IRD")
      ),
      # UCAD
      shiny::tags$a(
        href = "https://www.ucad.sn", target = "_blank",
        style = "text-decoration:none;",
        if (!is.null(logos$ucad))
          shiny::tags$img(src = logos$ucad, height = "52px",
            alt = "UCAD", title = "Université Cheikh Anta Diop de Dakar",
            style = "opacity:0.9;")
        else
          shiny::tags$span(style = "font-weight:700;font-size:1.3rem;color:#26306B;letter-spacing:1px;", "UCAD")
      ),
      # CIRAD
      shiny::tags$a(
        href = "https://www.cirad.fr/en", target = "_blank",
        style = "text-decoration:none;",
        if (!is.null(logos$cirad))
          shiny::tags$img(src = logos$cirad, height = "52px",
            alt = "CIRAD", title = "Agricultural Research for Development",
            style = "opacity:0.9;")
        else
          shiny::tags$span(style = "font-weight:700;font-size:1.3rem;color:#26306B;letter-spacing:1px;", "CIRAD")
      ),
      # INTERTRYP
      shiny::tags$a(
        href = "https://umr-intertryp.cirad.fr/en", target = "_blank",
        style = "text-decoration:none;",
        if (!is.null(logos$intertryp))
          shiny::tags$img(src = logos$intertryp, height = "52px",
            alt = "INTERTRYP", title = "Hosts, Vectors and Infectious Agents",
            style = "opacity:0.9;")
        else
          shiny::tags$span(style = "font-weight:700;font-size:1.3rem;color:#26306B;letter-spacing:1px;", "INTERTRYP")
      )
    )
  )

  # -- Help tab content -------------------------------------------------------

  help_content <- shiny::tagList(
    module_banner("question-circle", "Help & Documentation",
      "Data format requirements \u00b7 Module-by-module guide \u00b7 FAQ \u00b7 Glossary \u00b7 Statistical methods & key references",
      "#6B64EF"),
    shiny::fluidRow(
      shinydashboard::box(
        width = 12, solidHeader = FALSE,
        title = shiny::div(
          style = "background:#FFFFFF; padding:10px; color:#333a43; font-weight:600;",
          shiny::icon("file-alt"), " Data requirements"
        ),

        # Quick navigation — jump straight to a section instead of scrolling
        shiny::div(
          class = "spg-help-nav",
          shiny::tags$a(href = "#help-formats",  shiny::icon("table"),        " File formats"),
          shiny::tags$a(href = "#help-encoding", shiny::icon("columns"),      " Encoding"),
          shiny::tags$a(href = "#help-modules",  shiny::icon("th"),          " Module guide"),
          shiny::tags$a(href = "#help-tips",     shiny::icon("lightbulb"),    " Tips"),
          shiny::tags$a(href = "#help-faq",      shiny::icon("question"),     " FAQ"),
          shiny::tags$a(href = "#help-glossary", shiny::icon("spell-check"),  " Glossary"),
          shiny::tags$a(href = "#help-macos",    shiny::icon("apple"),        " macOS setup"),
          shiny::tags$a(href = "#help-workflow", shiny::icon("route"),        " Workflow"),
          shiny::tags$a(href = "#help-methods",  shiny::icon("book-open"),    " References")
        ),

        # Format overview
        shiny::div(
          class = "spg-help-section",
          id = "help-formats",
          shiny::tags$h3(shiny::icon("table"), " Accepted file formats"),
          shiny::div(
            class = "spg-format-note",
            shiny::icon("info-circle"),
            shiny::HTML(" <strong>ShinyPopGen accepts CSV and tabulation-delimited TXT files.</strong>
              The file must contain at least one <em>population</em> column and one or more
              <em>genetic marker</em> columns. Additional metadata columns (e.g. latitude,
              longitude, individual ID) are optional.")
          ),
          shiny::tags$ul(
            style = "font-size:14px; line-height:1.9;",
            shiny::tags$li(shiny::tags$strong("Header row:"), " required (first row = column names)."),
            shiny::tags$li(shiny::tags$strong("Separator:"), " comma (,), semicolon (;), or tab (\\t) — detected automatically."),
            shiny::tags$li(shiny::tags$strong("Population column:"), " any column name; one value per individual."),
            shiny::tags$li(shiny::tags$strong("Marker columns:"), " single-column (", shiny::tags$code("192/194"), ") or two-column (", shiny::tags$code("192"), " + ", shiny::tags$code("194"), ") encoding."),
            shiny::tags$li(shiny::tags$strong("Missing data:"), " coded as ", shiny::tags$code("0"), " or ", shiny::tags$code("0/0"), " by default — customisable on import."),
            shiny::tags$li(shiny::tags$strong("Upload limit:"), " 500 MB.")
          )
        ),

        shiny::tags$hr(),

        # Table examples
        shiny::div(
          class = "spg-help-section",
          id = "help-encoding",
          shiny::tags$h3(shiny::icon("columns"), " Encoding formats"),
          shiny::fluidRow(
            shiny::column(6,
              shiny::div(
                class = "spg-format-note",
                shiny::icon("compress-alt"),
                shiny::HTML(" <strong>Single-column</strong> &mdash; both alleles in one cell, separated by <code>/</code>, <code>-</code> or <code>_</code>.")
              ),
              shiny::div(
                style = "overflow-x:auto;",
                shiny::HTML(kableExtra::kable_styling(
                  knitr::kable(data_one_col, "html", align = "l"),
                  full_width = FALSE, position = "left",
                  bootstrap_options = c("striped", "hover", "condensed", "bordered")
                ))
              )
            ),
            shiny::column(6,
              shiny::div(
                class = "spg-format-note",
                shiny::icon("expand-alt"),
                shiny::HTML(" <strong>Two-column</strong> &mdash; each allele in its own column. The first allele column carries the locus name (e.g. <code>B12</code>); the second must be named <code>B12_1</code> (or <code>B12.1</code>). Paired columns do not need to be adjacent.")
              ),
              shiny::div(
                style = "overflow-x:auto;",
                shiny::HTML(kableExtra::kable_styling(
                  knitr::kable(data_two_col, "html", align = "l",
                               col.names = c("Population","B12","B12_1","C07","C07_1")),
                  full_width = FALSE, position = "left",
                  bootstrap_options = c("striped", "hover", "condensed", "bordered")
                ))
              )
            )
          ),  # end fluidRow columns

          shiny::tags$hr(style = "margin: 10px 0;"),

          # --- Auto-detection explanation
          shiny::div(
            class = "spg-format-note",
            style = "background:#f0f4ff; border-left:4px solid #6B64EF; padding:10px 14px; border-radius:4px; font-size:13px; line-height:1.7;",
            shiny::tags$p(
              style = "margin-top:0; font-weight:600;",
              shiny::icon("magic"), " Auto-detection algorithm for two-column format"
            ),
            shiny::tags$p(
              "When you load a file, the app scans every column name for a suffix matching ",
              shiny::tags$code("_1"), " or ", shiny::tags$code(".1"), "\u2013", shiny::tags$code(".9"),
              ". Any column that matches is treated as the second allele of the locus whose name is obtained by removing that suffix."
            ),
            shiny::tags$p(
              "Example: columns ", shiny::tags$code("B12"), " and ", shiny::tags$code("B12_1"),
              " are automatically paired into a single locus called ", shiny::tags$code("B12"), ".",
              " The pair is merged as ", shiny::tags$code("192/194"), " in the preview and stored as a packed integer internally."
            ),
            shiny::tags$p(
              style = "color:#c0392b; margin-bottom:0;",
              shiny::icon("exclamation-triangle"),
              shiny::tags$strong(" Caveat:"),
              " any non-marker column whose name ends in ", shiny::tags$code("_1"), " (e.g. a metadata field called ",
              shiny::tags$code("Site_1"), " or a sample ID like ", shiny::tags$code("Ind_1"),
              ") will also be detected as a marker allele column. If this happens, use the manual assignment below."
            )
          ),

          shiny::tags$hr(style = "margin: 10px 0;"),

          # --- Manual assignment guide
          shiny::div(
            class = "spg-format-note",
            style = "background:#fff8e1; border-left:4px solid #EBCC2A; padding:10px 14px; border-radius:4px; font-size:13px; line-height:1.7;",
            shiny::tags$p(
              style = "margin-top:0; font-weight:600;",
              shiny::icon("sliders-h"), " Manual column assignment"
            ),
            shiny::tags$p(
              "If auto-detection picks up the wrong columns (or misses some), use the ",
              shiny::tags$strong("Manual column assignment"), " panel on the Import page:"
            ),
            shiny::tags$ol(
              style = "margin-bottom:0;",
              shiny::tags$li(shiny::HTML(
                "<strong>Population name</strong> &mdash; select the column that contains the population label."
              )),
              shiny::tags$li(shiny::HTML(
                "<strong>Latitude / Longitude</strong> &mdash; select the GPS coordinate columns (optional)."
              )),
              shiny::tags$li(shiny::HTML(
                "<strong>Metadata columns (indices)</strong> &mdash; enter a range of column indices for any extra metadata
                (individual ID, host, sampling date, \u2026). Format: <code>2-5</code>, <code>2,4,6</code>, or <code>2-4,7</code>.
                GPS columns are included here automatically."
              )),
              shiny::tags$li(shiny::HTML(
                "<strong>Marker locus columns</strong> &mdash; enter the column range that covers <em>all</em> allele columns
                for your markers, including the <code>_1</code> / <code>.1</code> companion columns.
                Example: if loci start at column\u00a08 and the last <code>_1</code> companion is column\u00a041, enter <code>8-41</code>.
                The app will pair them automatically within that range."
              ))
            )
          )
        ),

        shiny::tags$hr(),

        # Module-by-module guide
        shiny::div(
          class = "spg-help-section",
          id = "help-modules",
          shiny::tags$h3(shiny::icon("th"), " Module-by-module guide"),
          shiny::div(
            class = "spg-format-note",
            shiny::icon("info-circle"),
            shiny::HTML(" Every module reads from the same imported dataset (no re-upload needed) and every results table has a matching <code>.csv</code>/<code>.txt</code> export.")
          ),
          shiny::div(
            style = "overflow-x:auto;",
            shiny::HTML(paste0(
              "<table class='spg-modguide-table'><thead><tr>",
              "<th>Module</th><th>What it computes</th><th>Key inputs</th><th>Key outputs</th>",
              "</tr></thead><tbody>",

              "<tr><td><b>Import Data</b></td>",
              "<td>Parses the file, auto-detects population/marker columns, builds the DuckDB tables used by every other module.</td>",
              "<td>CSV/TXT file, separator, header, missing-data code, optional manual column assignment.</td>",
              "<td>Formatted preview table, interactive map (if GPS columns found), import summary.</td></tr>",

              "<tr><td><b>Allele Freq</b></td>",
              "<td>Per-population and global allele frequencies, sample sizes, missing-data rates.</td>",
              "<td>Population / marker filters.</td>",
              "<td>Allele-frequency table (Na, Ne, He, Ho, Fis per locus), missing-data value boxes.</td></tr>",

              "<tr><td><b>General Stats</b></td>",
              "<td>Na, Ne, Ho, He and F-statistics (WC84) per allele and per locus.</td>",
              "<td>None beyond the imported dataset.</td>",
              "<td>Per-allele / per-locus statistics table.</td></tr>",

              "<tr><td><b>Local Panmixia</b></td>",
              "<td>Within-population Hardy-Weinberg test. FIS per locus and population (WC84).</td>",
              "<td>Number of bootstrap replicates, number of permutations, confidence level.</td>",
              "<td>FIS table with bootstrap CI and permutation p-value.</td></tr>",

              "<tr><td><b>Global Panmixia</b></td>",
              "<td>Overall Hardy-Weinberg test across all populations. Multilocus FIT.</td>",
              "<td>Number of bootstrap replicates, number of permutations, confidence level.</td>",
              "<td>FIT table with bootstrap CI and permutation p-value.</td></tr>",

              "<tr><td><b>Subdivision</b></td>",
              "<td>Population differentiation: FST (WC84, population-block bootstrap + permutation) and an independent G-test (log-likelihood ratio, FSTAT-style, complete multilocus genotypes only).</td>",
              "<td>Number of bootstrap replicates / permutations (separately for FST and the G-test), confidence level.</td>",
              "<td>FST table + plot, G-test table (two one-sided p-values, as in FSTAT).</td></tr>",

              "<tr><td><b>Diversities</b></td>",
              "<td>HS and HT (Nei 1987) per locus, with locus-bootstrap CI for the multilocus FST, FIT, FIS, HS, HT estimates.</td>",
              "<td>Shares parameters with Subdivision (same underlying computation run).</td>",
              "<td>HS/HT table, multilocus bootstrap summary.</td></tr>",

              "<tr><td><b>LD</b></td>",
              "<td>Pairwise linkage disequilibrium between all locus pairs, tested by permutation.</td>",
              "<td>Number of permutations.</td>",
              "<td>Pairwise LD table with permutation p-values.</td></tr>",

              "<tr><td><b>Null Alleles</b></td>",
              "<td>Null allele frequency per locus \u00d7 population (FreeNA / EM algorithm), FST-ENA and DCSE-INA corrections, two independent bootstraps (over loci, over sub-samples).</td>",
              "<td>Missing-genotype coding per locus, bootstrap replicate counts, confidence level.</td>",
              "<td>4 automatic export files (see the Null Alleles tab itself for the full list) + on-screen matrices.</td></tr>",

              "<tr><td><b>IBD</b> \U0001f6a7</td>",
              "<td>Isolation by distance: pairwise F<sub>ST</sub>\u2044(1\u2212F<sub>ST</sub>) vs geographic distance, Mantel test (Rousset 1997).</td>",
              "<td>Requires GPS metadata (latitude/longitude) from the import step, or an uploaded distance-matrix file.</td>",
              "<td>Scatter plot, Mantel test statistic and p-value.</td></tr>",

              "</tbody></table>"
            ))
          )
        ),

        shiny::tags$hr(),

        # Tips
        shiny::div(
          class = "spg-help-section",
          id = "help-tips",
          shiny::tags$h3(shiny::icon("lightbulb"), " Tips & common pitfalls"),
          shiny::div(class = "spg-tip", shiny::icon("check-circle"),
            shiny::HTML(" <strong>Use the default dataset</strong> to familiarise yourself with the expected format before uploading your own data.")),
          shiny::div(class = "spg-tip", shiny::icon("check-circle"),
            shiny::HTML(" <strong>Metadata columns</strong> (latitude, longitude, individual ID) can be specified as index ranges — e.g. <code>1-3</code> or <code>1,4,5</code> — and are preserved through the analysis.")),
          shiny::div(class = "spg-tip", shiny::icon("check-circle"),
            shiny::HTML(" <strong>Null alleles (FreeNA)</strong> must be coded as <code>999999/999999</code> for null homozygotes and <code>0/0</code> for missing genotypes. Use the per-locus override panel in the Null Alleles tab to recode as needed.")),
          shiny::div(class = "spg-tip", shiny::icon("exclamation-triangle"),
            shiny::HTML(" <strong>All locus columns must use the same encoding</strong> (single or two-column) within a file. Mixed encoding is not supported.")),
          shiny::div(class = "spg-tip", shiny::icon("exclamation-triangle"),
            shiny::HTML(" <strong>Population codes must be consistent</strong> across rows — trailing spaces or capitalisation differences will create duplicate populations."))
        ),

        shiny::tags$hr(),

        # FAQ / Troubleshooting
        shiny::div(
          class = "spg-help-section",
          id = "help-faq",
          shiny::tags$h3(shiny::icon("question"), " FAQ & troubleshooting"),
          shiny::div(
            class = "spg-format-note",
            shiny::icon("info-circle"),
            shiny::HTML(" These are the exact messages the app shows when something goes wrong, with what they mean and what to do about them.")
          ),
          shiny::tags$div(class = "spg-faq-item",
            shiny::tags$details(
              shiny::tags$summary("\u201cDuckDB meta table missing\u201d / \u201cDuckDB hf table missing\u201d"),
              shiny::tags$div(class = "spg-faq-body",
                shiny::HTML("You've opened a module (e.g. Allele Freq, General Stats) before finishing the Import step, or the import failed silently. Go back to <b>Import Data</b>, load a file (or the default dataset), and confirm the preview table and map show data before switching tabs.")
              )
            )
          ),
          shiny::tags$div(class = "spg-faq-item",
            shiny::tags$details(
              shiny::tags$summary("\u201chf must contain (individual,locus,g) or (indiv_id,locus_id,gt)\u201d"),
              shiny::tags$div(class = "spg-faq-body",
                shiny::HTML("Internal schema check: the genotype table built at import doesn't have the expected column names. This normally means the import step itself failed or was interrupted \u2014 re-run <b>Load Data</b> rather than trying to fix this from a later tab.")
              )
            )
          ),
          shiny::tags$div(class = "spg-faq-item",
            shiny::tags$details(
              shiny::tags$summary("\u201cPopulation column not detected\u201d / \u201cNo column names available (header not read)\u201d"),
              shiny::tags$div(class = "spg-faq-body",
                shiny::HTML("Auto-detection couldn't find a population column, usually because the separator (comma / semicolon / tab) doesn't match the file, or <b>Header</b> is unticked for a file that does have one. Try the other separator, or use the <b>Manual column assignment</b> panel to pick the population column yourself.")
              )
            )
          ),
          shiny::tags$div(class = "spg-faq-item",
            shiny::tags$details(
              shiny::tags$summary("\u201cTry again, your range is out of bounds\u201d"),
              shiny::tags$div(class = "spg-faq-body",
                shiny::HTML("The column range typed into <b>Marker locus columns</b> or <b>Metadata columns</b> refers to a column index that doesn't exist in the file (e.g. <code>8-45</code> on a 40-column file). Count the columns in the preview table and adjust the range.")
              )
            )
          ),
          shiny::tags$div(class = "spg-faq-item",
            shiny::tags$details(
              shiny::tags$summary("An error message starting with \u201cErreur lors de \u2026\u201d"),
              shiny::tags$div(class = "spg-faq-body",
                shiny::HTML("A database query failed while computing a result (dropped connection, unexpected column). The rest of the message names which computation failed. Try reloading the data; if it persists with the same file, it's worth reporting (see Contact on the Welcome page) with that exact message.")
              )
            )
          ),
          shiny::tags$div(class = "spg-faq-item",
            shiny::tags$details(
              shiny::tags$summary("My upload is rejected or times out"),
              shiny::tags$div(class = "spg-faq-body",
                shiny::HTML("The upload limit is <b>500\u00a0MB</b>. Larger files need to be split or pre-filtered before import. If a file under that limit still times out, check your network connection to the server (this is more likely on a remote/Docker deployment than a local install).")
              )
            )
          ),
          shiny::tags$div(class = "spg-faq-item",
            shiny::tags$details(
              shiny::tags$summary("My FIS / He values look wrong on a locus I know has null alleles"),
              shiny::tags$div(class = "spg-faq-body",
                shiny::HTML("General Stats / Allele Freq do not correct for null alleles. Go to the <b>Null Alleles</b> tab: it estimates null-allele frequency per locus and reports FST-ENA / DCSE-INA, which are the corrected versions of the differentiation statistics. A locus with a null-allele frequency above ~0.10 should be treated with caution for FST/FIS interpretation.")
              )
            )
          ),
          shiny::tags$div(class = "spg-faq-item",
            shiny::tags$details(
              shiny::tags$summary("The IBD tab looks incomplete or behaves oddly"),
              shiny::tags$div(class = "spg-faq-body",
                shiny::HTML("Isolation by Distance is flagged \U0001f6a7 (work in progress) on the Welcome page and in the Module guide above. Treat its output as provisional.")
              )
            )
          )
        ),

        shiny::tags$hr(),

        # Glossary
        shiny::div(
          class = "spg-help-section",
          id = "help-glossary",
          shiny::tags$h3(shiny::icon("spell-check"), " Glossary"),
          shiny::tags$dl(
            class = "spg-glossary",
            shiny::tags$dt("Na"), shiny::tags$dd("Number of alleles observed at a locus."),
            shiny::tags$dt("Ne"), shiny::tags$dd("Effective number of alleles \u2014 the number of equally frequent alleles that would give the same heterozygosity as observed."),
            shiny::tags$dt("Ho"), shiny::tags$dd("Observed heterozygosity \u2014 proportion of heterozygous genotypes actually observed."),
            shiny::tags$dt("He / HS / HT"), shiny::tags$dd("Expected (gene diversity) heterozygosity. He/HS = within-population; HT = total, across all populations pooled (Nei 1987)."),
            shiny::tags$dt("FIS"), shiny::tags$dd("Inbreeding coefficient within populations relative to their own allele frequencies (Weir & Cockerham 1984). Positive = heterozygote deficit; negative = heterozygote excess."),
            shiny::tags$dt("FIT"), shiny::tags$dd("Overall inbreeding coefficient of individuals relative to the total (multi-population) allele frequencies."),
            shiny::tags$dt("FST"), shiny::tags$dd("Differentiation among populations \u2014 the proportion of total genetic variance explained by allele-frequency differences between populations."),
            shiny::tags$dt("FST-ENA"), shiny::tags$dd("FST 'Excluding Null Alleles' \u2014 FST recomputed on EM-corrected allele frequencies (Chapuis & Estoup 2007)."),
            shiny::tags$dt("p_nulls"), shiny::tags$dd("Estimated null-allele frequency at a given locus \u00d7 population, from the EM algorithm (Dempster, Laird & Rubin 1977) as implemented in FreeNA."),
            shiny::tags$dt("DCSE / DCSE-INA"), shiny::tags$dd("Cavalli-Sforza & Edwards (1967) chord genetic distance between populations; DCSE-INA is the version with the null allele added as an extra allelic state."),
            shiny::tags$dt("EM algorithm"), shiny::tags$dd("Expectation-Maximisation \u2014 the iterative procedure used to estimate null-allele frequencies from observed genotype counts until convergence."),
            shiny::tags$dt("WC84"), shiny::tags$dd("Shorthand used throughout the app for the unbiased moment estimators of Weir & Cockerham (1984) for FIS/FIT/FST."),
            shiny::tags$dt("Bootstrap CI vs. permutation p-value"), shiny::tags$dd("Two different resampling procedures reported side by side: the bootstrap CI resamples loci (or individuals) to give a confidence interval on the estimate itself; the permutation p-value reshuffles individuals/genotypes under a null hypothesis of no structure to test significance. They answer different questions and are not interchangeable.")
          )
        ),

        shiny::tags$hr(),

        # Mac installation note
        shiny::div(
          class = "spg-help-section",
          id = "help-macos",
          shiny::tags$h3(shiny::icon("apple"), " macOS: prerequisites for installation"),
          shiny::div(
            class = "spg-format-note",
            shiny::icon("exclamation-triangle"),
            shiny::HTML(" <strong>macOS users only.</strong> ShinyPopGen contains C++ code compiled with OpenMP.
              Apple's default clang does <em>not</em> include OpenMP or gfortran.
              Install the following before running <code>remotes::install_git()</code>:")
          ),
          shiny::tags$ol(
            style = "font-size:14px; line-height:2.0;",
            shiny::tags$li(shiny::HTML(
              "<strong>gfortran</strong> &mdash; required for R package compilation on macOS.<br>
               Download the official R-project gfortran from
               <a href='https://mac.r-project.org/tools/' target='_blank' style='color:#6B64EF;'>mac.r-project.org/tools</a>
               and install <code>gfortran-14.2-universal.pkg</code> (or the current version)."
            )),
            shiny::tags$li(shiny::HTML(
              "<strong>libomp</strong> &mdash; OpenMP runtime library, via Homebrew:<br>
               <code>brew install libomp</code>"
            )),
            shiny::tags$li(shiny::HTML(
              "(Apple Silicon only) Add to <code>~/.R/Makevars</code> (create the file if it does not exist):<br>
               <code>LDFLAGS += -L/opt/homebrew/opt/libomp/lib -lomp</code><br>
               <code>CPPFLAGS += -I/opt/homebrew/opt/libomp/include -Xclang -fopenmp</code>"
            ))
          )
        ),

        shiny::tags$hr(),

        # Workflow
        shiny::div(
          class = "spg-help-section",
          id = "help-workflow",
          shiny::tags$h3(shiny::icon("route"), " Recommended workflow"),
          shiny::tags$ol(
            style = "font-size:14px; line-height:2.0;",
            shiny::tags$li(shiny::HTML("<strong>Import Data</strong> &mdash; load file, check the preview table and the map.")),
            shiny::tags$li(shiny::HTML("<strong>Allele Freq</strong> &mdash; inspect missing data rates; flag loci with >20% missing.")),
            shiny::tags$li(shiny::HTML("<strong>General Stats</strong> &mdash; obtain Na, Ne, Ho, He per locus and per population.")),
            shiny::tags$li(shiny::HTML("<strong>Null Alleles</strong> &mdash; if high Ho/He ratio is suspected, estimate null allele frequencies and correct.")),
            shiny::tags$li(shiny::HTML("<strong>Local Panmixia</strong> &mdash; test for HWE within each population (FIS).")),
            shiny::tags$li(shiny::HTML("<strong>Subdivision</strong> &mdash; estimate FST; evaluate global and pairwise differentiation.")),
            shiny::tags$li(shiny::HTML("<strong>Diversities</strong> &mdash; obtain HS/HT and locus bootstrap CI for all multilocus estimators.")),
            shiny::tags$li(shiny::HTML("<strong>LD</strong> &mdash; test pairwise linkage disequilibrium across loci.")),
            shiny::tags$li(shiny::HTML("<strong>IBD</strong> &mdash; test isolation by distance: pairwise F<sub>ST</sub>\u2044(1\u2212F<sub>ST</sub>) vs geographic distance (requires GPS data)."))

          )
        ),

        shiny::tags$hr(),

        # Statistical methods & references
        shiny::div(
          class = "spg-help-section",
          id = "help-methods",
          shiny::tags$h3(shiny::icon("book-open"), " Statistical methods & key references"),
          shiny::fluidRow(
            shiny::column(6,
              shiny::div(
                style = "padding:12px 14px; background:#f8f9fc; border-radius:6px; border-left:3px solid #6B64EF; margin-bottom:12px;",
                shiny::tags$strong("F-statistics (WC84)"),
                shiny::tags$p(style = "font-size:13px; margin:6px 0 0; line-height:1.7;",
                  "FIS, FST and FIT are estimated following the unbiased moment estimators of ",
                  shiny::tags$strong("Weir & Cockerham (1984)"), ". These estimators are robust to unequal sample sizes across populations and loci. Confidence intervals are computed by non-parametric bootstrap over loci; p-values by permutation of individuals across populations."),
                shiny::tags$p(style = "font-size:12px; margin:4px 0 0; color:#777;",
                  "Weir BS, Cockerham CC. 1984. Estimating F-statistics for the analysis of population structure. Evolution 38:1358-1370.")
              ),
              shiny::div(
                style = "padding:12px 14px; background:#f8f9fc; border-radius:6px; border-left:3px solid #2CBF9F; margin-bottom:12px;",
                shiny::tags$strong("Gene diversity (Nei 1987)"),
                shiny::tags$p(style = "font-size:13px; margin:6px 0 0; line-height:1.7;",
                  "HS (within-population gene diversity) and HT (total gene diversity) are computed following ",
                  shiny::tags$strong("Nei (1987)"), ". Per-locus and multilocus estimates are reported, with bootstrap CI derived by resampling over loci."),
                shiny::tags$p(style = "font-size:12px; margin:4px 0 0; color:#777;",
                  "Nei M. 1987. Molecular Evolutionary Genetics. Columbia University Press, New York.")
              ),
              shiny::div(
                style = "padding:12px 14px; background:#f8f9fc; border-radius:6px; border-left:3px solid #EBCC2A; margin-bottom:12px;",
                shiny::tags$strong("Linkage disequilibrium"),
                shiny::tags$p(style = "font-size:13px; margin:6px 0 0; line-height:1.7;",
                  "Pairwise LD is tested for all locus pairs using a permutation approach: alleles at one locus are permuted across individuals (within each population) and the observed association is compared to the permutation distribution. Significant LD between physically unlinked loci may reflect selection, admixture, or small sample size."),
                shiny::tags$p(style = "font-size:12px; margin:4px 0 0; color:#777;",
                  "Rousset F. 2008. Genepop'007: a complete re-implementation of the Genepop software. Mol Ecol Resour 8:103-106.")
              )
            ),
            shiny::column(6,
              shiny::div(
                style = "padding:12px 14px; background:#f8f9fc; border-radius:6px; border-left:3px solid #B40F20; margin-bottom:12px;",
                shiny::tags$strong("Null alleles (FreeNA / EM algorithm)"),
                shiny::tags$p(style = "font-size:13px; margin:6px 0 0; line-height:1.7;",
                  "Null allele frequencies are estimated per locus \u00d7 population by the Expectation-Maximisation (EM) algorithm implemented in ",
                  shiny::tags$strong("FreeNA"), " (Chapuis & Estoup 2007). The algorithm iterates between estimating null allele frequency from observed genotype counts and updating expected genotype frequencies until convergence. Null alleles inflate apparent FIS and bias FST downward."),
                shiny::tags$p(style = "font-size:12px; margin:4px 0 0; color:#777;",
                  "Chapuis MP, Estoup A. 2007. Microsatellite null alleles and estimation of population differentiation. Mol Biol Evol 24:621-631.")
              ),
              shiny::div(
                style = "padding:12px 14px; background:#f8f9fc; border-radius:6px; border-left:3px solid #9986A5; margin-bottom:12px;",
                shiny::tags$strong("Hardy-Weinberg equilibrium tests"),
                shiny::tags$p(style = "font-size:13px; margin:6px 0 0; line-height:1.7;",
                  "HWE departure within populations is quantified via FIS (WC84). Permutation p-values are obtained by randomly reassigning alleles to diploid genotypes within each population. Multilocus FIS is the weighted composite across loci. Significant positive FIS indicates excess homozygosity (inbreeding, null alleles, Wahlund effect); negative FIS indicates heterozygote excess."),
                shiny::tags$p(style = "font-size:12px; margin:4px 0 0; color:#777;",
                  "Guo SW, Thompson EA. 1992. Performing the exact test of Hardy-Weinberg proportion for multiple alleles. Biometrics 48:361-372.")
              ),
              shiny::div(
                style = "padding:12px 14px; background:#f8f9fc; border-radius:6px; border-left:3px solid #3B9AB2; margin-bottom:12px;",
                shiny::tags$strong("Interpretation thresholds"),
                shiny::tags$p(style = "font-size:13px; margin:6px 0 0; line-height:1.7;",
                  shiny::tags$strong("FST:"), " <0.05 little differentiation; 0.05-0.15 moderate; 0.15-0.25 great; >0.25 very great (Wright 1978).", shiny::tags$br(),
                  shiny::tags$strong("FIS:"), " p<0.05 significant HWE departure. Bonferroni correction recommended for multiple loci.", shiny::tags$br(),
                  shiny::tags$strong("Null alleles:"), " frequency >0.10 at a locus warrants removal or correction before FST estimation.", shiny::tags$br(),
                  shiny::tags$strong("LD:"), " p-values are not corrected by default; apply FDR or Bonferroni for multiple comparisons.")
              )
            )
          )
        )
      )
    )
  )

  # -- Assemble page ---------------------------------------------------------

  bslib::page_navbar(
    id    = "main_nav",
    title = "ShinyPopGen",
    theme          = spg_theme,
    window_title   = "ShinyPopGen",
    navbar_options = bslib::navbar_options(collapsible = TRUE),
    fillable       = FALSE,

    # Inject AdminLTE CSS for shinydashboard boxes + our custom overrides
    header = shiny::tags$head(
      shiny::tags$title("ShinyPopGen"),
      shiny::tags$link(
        rel  = "stylesheet",
        href = "sdb/AdminLTE/AdminLTE.min.css"
      ),
      shiny::tags$link(
        rel  = "stylesheet",
        href = "sdb/shinydashboard.css"
      ),
      shiny::tags$style(custom_css),
      shiny::tags$script(shiny::HTML("
(function () {
  var nav, lastPane, frameId;

  function init() {
    nav = document.querySelector('nav.navbar');
    if (!nav) { setTimeout(init, 150); return; }
    document.addEventListener('shown.bs.tab', schedule, true);
    var mo = new MutationObserver(function (muts) {
      if (muts.some(function (m) {
        return m.attributeName === 'class' &&
               m.target.classList.contains('tab-pane') &&
               m.target.classList.contains('active');
      })) schedule();
    });
    mo.observe(document.body, { subtree: true, attributes: true, attributeFilter: ['class'] });
    schedule();
  }

  function schedule() {
    if (frameId) cancelAnimationFrame(frameId);
    frameId = requestAnimationFrame(doLayout);
  }

  function doLayout() {
    frameId = null;
    var pane = document.querySelector('.tab-pane.active');
    if (!pane || pane === lastPane) return;
    lastPane = pane;
    if (pane.querySelector('.spg-hero')) {
      document.body.classList.remove('spg-sidebar-mode');
      document.body.classList.add('spg-welcome-mode');
    } else {
      document.body.classList.remove('spg-welcome-mode');
      document.body.classList.add('spg-sidebar-mode');
    }
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    setTimeout(init, 0);
  }
})();
      "))
    ),

    # ── Module tabs ──────────────────────────────────────────────────────────
    bslib::nav_panel(
      title = "Welcome",
      icon  = shiny::icon("home"),
      value = "welcome",
      welcome_content
    ),
    bslib::nav_panel(
      title = "Import Data",
      icon  = shiny::icon("upload"),
      value = "import",
      import_data_ui("import")
    ),
    bslib::nav_panel(
      title = "Allele Freq",
      icon  = shiny::icon("chart-pie"),
      value = "allele_frequencies",
      ui_allele_frequencies("allele")
    ),
    bslib::nav_panel(
      title = "General Stats",
      icon  = shiny::icon("table"),
      value = "general_stats",
      mod_general_stats_ui("general_stats")
    ),
    bslib::nav_panel(
      title = "Local Panmixia",
      icon  = shiny::icon("flask"),
      value = "local_panmixia",
      mod_local_panmixia_ui("general_stats")
    ),
    bslib::nav_panel(
      title = "Global Panmixia",
      icon  = shiny::icon("globe"),
      value = "global_panmixia",
      mod_global_panmixia_ui("general_stats")
    ),
    bslib::nav_panel(
      title = "Subdivision",
      icon  = shiny::icon("sitemap"),
      value = "subdivision",
      mod_subdivision_ui("general_stats")
    ),
    bslib::nav_panel(
      title = "Diversities",
      icon  = shiny::icon("chart-line"),
      value = "genetic_diversities",
      mod_genetic_diversities_ui("general_stats")
    ),
    bslib::nav_panel(
      title = "LD",
      icon  = shiny::icon("link"),
      value = "linkage_desequilibrium",
      linkage_desequilibrium_UI("ld")
    ),
    bslib::nav_panel(
      title = "Null Alleles",
      icon  = shiny::icon("circle-notch"),
      value = "null_alleles",
      null_alleles_UI("null_alleles")
    ),
    bslib::nav_panel(
      title = HTML('IBD'),
      icon  = shiny::icon("map-marker-alt"),
      value = "isolation_by_distance",
      isolation_by_distance_UI("ibd")
    ),
    bslib::nav_panel(
      title = "Help",
      icon  = shiny::icon("question-circle"),
      value = "help",
      help_content
    ),

    # ── Right-side controls ──────────────────────────────────────────────────
    bslib::nav_spacer(),
    bslib::nav_item(
      bslib::input_dark_mode(id = "color_mode")
    )
  )
}