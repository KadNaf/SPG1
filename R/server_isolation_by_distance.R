# server_isolation_by_distance.R
# Tab 1 — Geographic distances (Haversine, km), bootstrap CI over individuals.
# Tab 2 — Mantel test: uploaded Matrix 1 (square or long/rectangular) vs.
#         internal Dgeo / ln(Dgeo). Joint row/column permutation — valid on
#         rectangular (incomplete) matrices.

# ============================================================
# File-local helpers
# ============================================================

.iod_haversine_km <- function(lat1, lon1, lat2, lon2) {
  R    <- 6371.0
  dlat <- (lat2 - lat1) * pi / 180
  dlon <- (lon2 - lon1) * pi / 180
  a    <- sin(dlat / 2)^2 +
    cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(dlon / 2)^2
  2 * R * asin(sqrt(a))
}

# Continuous-gradient colored half-matrix (HTML), NA-safe.
.iod_render_mat_html <- function(mat, digits = 2,
                                  low_col = "#eff6ff", high_col = "#1e3a8a") {
  labs <- rownames(mat); n <- length(labs)
  vals <- mat[lower.tri(mat)]
  vals <- vals[is.finite(vals)]
  rng  <- if (length(vals) >= 1L) range(vals) else c(0, 1)
  if (diff(rng) <= 0) rng <- c(rng[1] - 1, rng[1] + 1)
  ramp <- grDevices::colorRampPalette(c(low_col, high_col))(100)
  col_for <- function(v) {
    if (!is.finite(v)) return(NULL)
    idx <- round(99 * (v - rng[1]) / diff(rng)) + 1L
    idx <- max(1L, min(100L, idx))
    ramp[idx]
  }
  cell <- function(i, j) {
    if (i == j)  return('<td style="background:#f1f5f9;color:#94a3b8;text-align:center;">\u2014</td>')
    if (i < j)   return('<td style="color:#cbd5e1;text-align:center;">\u00b7</td>')
    v <- mat[i, j]
    if (!is.finite(v)) return('<td style="color:#94a3b8;text-align:center;">NA</td>')
    bg <- col_for(v)
    txt_col <- if (!is.null(bg) && (v - rng[1]) / diff(rng) > 0.6) "#ffffff" else "#0f172a"
    sprintf('<td style="background:%s;color:%s;text-align:right;padding:4px 9px;">%s</td>',
            bg, txt_col, round(v, digits))
  }
  thead <- paste0('<tr><th></th>',
                  paste(sprintf('<th style="padding:4px 9px;">%s</th>', labs[-n]), collapse = ""),
                  '</tr>')
  tbody <- paste(sapply(seq_len(n), function(i) {
    if (i == 1L) return("")
    paste0('<tr><td style="font-weight:700;white-space:nowrap;padding:4px 9px;">', labs[i], '</td>',
           paste(sapply(seq_len(n), function(j) cell(i, j)), collapse = ""), '</tr>')
  }), collapse = "")
  HTML(sprintf(
    '<div style="overflow-x:auto;"><table style="border-collapse:collapse;font-size:11px;width:100%%;">
       <thead>%s</thead><tbody>%s</tbody></table></div>', thead, tbody))
}

# Mantel test via JOINT row/column permutation of Matrix 2.
# mat1, mat2 : square matrices with population-name dimnames; NA allowed
#              (rectangular / incomplete matrices supported on either side).
.iod_mantel_matrix <- function(mat1, mat2, n_perm = 9999L, stat = "r") {
  common <- intersect(rownames(mat1), rownames(mat2))
  if (length(common) < 3L)
    return(list(stat_obs = NA_real_, p_value = NA_real_, n_pairs = 0L,
                n_perm_valid = 0L, slope = NA_real_, intercept = NA_real_,
                r2 = NA_real_, x = numeric(0), y = numeric(0), common = common))

  m1 <- mat1[common, common, drop = FALSE]
  m2 <- mat2[common, common, drop = FALSE]
  n  <- length(common)
  lower_idx <- which(lower.tri(matrix(TRUE, n, n)))
  x_all <- m1[lower_idx]
  y_all <- m2[lower_idx]

  stat_fn <- function(xx, yy) {
    ok <- is.finite(xx) & is.finite(yy)
    if (sum(ok) < 3L) return(NA_real_)
    if (stat == "b") unname(coef(lm(yy[ok] ~ xx[ok]))[2L])
    else             suppressWarnings(cor(xx[ok], yy[ok]))
  }

  ok_obs   <- is.finite(x_all) & is.finite(y_all)
  stat_obs <- stat_fn(x_all, y_all)

  perm_stats <- vapply(seq_len(n_perm), function(.b) {
    perm <- sample.int(n)
    m2p  <- m2[perm, perm, drop = FALSE]
    yp   <- m2p[lower_idx]
    stat_fn(x_all, yp)
  }, numeric(1L))

  perm_fin <- perm_stats[is.finite(perm_stats)]
  p_value  <- if (length(perm_fin) > 0L && is.finite(stat_obs))
                mean(perm_fin >= stat_obs) else NA_real_

  lm0 <- tryCatch(lm(y_all[ok_obs] ~ x_all[ok_obs]), error = function(e) NULL)
  slp <- if (!is.null(lm0)) unname(coef(lm0)[2L]) else NA_real_
  icp <- if (!is.null(lm0)) unname(coef(lm0)[1L]) else NA_real_
  r2  <- if (!is.null(lm0)) summary(lm0)$r.squared else NA_real_

  list(stat_obs = stat_obs, p_value = p_value, n_pairs = sum(ok_obs),
       n_perm_valid = length(perm_fin), slope = slp, intercept = icp, r2 = r2,
       x = x_all[ok_obs], y = y_all[ok_obs], common = common)
}

# ============================================================
# Module server
# ============================================================

server_isolation_by_distance <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    `%||%` <- function(x, y)
      if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x

    # ── DB plumbing ────────────────────────────────────────────────────────
    db_tick    <- reactive({ rv$db_tick })
    con_r      <- reactive({ shiny::req(rv$con); rv$con })
    tbl_meta_r <- reactive({ rv$tbl_meta %||% "meta" })

    db_ready <- reactive({
      db_tick(); con <- con_r()
      shiny::req(isTRUE(rv$db_ready))
      shiny::validate(shiny::need(DBI::dbExistsTable(con, tbl_meta_r()),
                                   "DuckDB meta table missing."))
      TRUE
    })

    # ── Individual-level GPS coordinates ───────────────────────────────────
    ind_coords_r <- reactive({
      db_ready()
      con  <- con_r()
      cols <- DBI::dbGetQuery(con, sprintf(
        "SELECT column_name FROM information_schema.columns WHERE table_name = '%s'",
        tbl_meta_r()))$column_name
      shiny::validate(shiny::need(
        all(c("Latitude", "Longitude") %in% cols),
        "No GPS data found. Re-import your dataset and assign Latitude/Longitude columns."))

      ms_ind <- if ("individual" %in% tolower(cols)) cols[tolower(cols) == "individual"][1L] else "individual"
      df <- DBI::dbGetQuery(con, sprintf(
        "SELECT Population,
                CAST(%s AS VARCHAR)   AS Individual,
                CAST(Latitude  AS DOUBLE) AS Latitude,
                CAST(Longitude AS DOUBLE) AS Longitude
         FROM %s
         WHERE Population IS NOT NULL
           AND Latitude IS NOT NULL AND Longitude IS NOT NULL",
        sql_ident(con, ms_ind), sql_ident(con, tbl_meta_r())))
      shiny::validate(shiny::need(
        length(unique(df$Population)) >= 2L,
        "At least 2 populations with GPS coordinates are required."))
      df
    })

    # ── TAB 1: Geographic distances + bootstrap CI ─────────────────────────
    geo_results_r <- eventReactive(input$run_geo, {
      shiny::req(db_ready())
      ind <- ind_coords_r()
      pops <- sort(unique(ind$Population))
      n    <- length(pops)
      shiny::validate(shiny::need(n >= 2L, "At least 2 populations are required."))

      n_boot <- as.integer(input$n_boot_geo)
      conf   <- input$conf_level_geo / 100
      alpha  <- (1 - conf) / 2

      withProgress(message = "Computing geographic distances\u2026", value = 0.1, {

        cent <- aggregate(cbind(Latitude, Longitude) ~ Population, data = ind, FUN = mean)
        rownames(cent) <- cent$Population

        dist_obs <- matrix(NA_real_, n, n, dimnames = list(pops, pops))
        for (i in seq_len(n - 1L)) for (j in (i + 1L):n) {
          dist_obs[j, i] <- .iod_haversine_km(
            cent$Latitude[cent$Population == pops[i]], cent$Longitude[cent$Population == pops[i]],
            cent$Latitude[cent$Population == pops[j]], cent$Longitude[cent$Population == pops[j]])
        }

        setProgress(0.3, detail = sprintf("Bootstrap over individuals (%d reps)\u2026", n_boot))
        ind_by_pop <- split(ind[, c("Latitude", "Longitude")], ind$Population)
        boot_arr   <- array(NA_real_, dim = c(n, n, n_boot))
        for (b in seq_len(n_boot)) {
          cent_b <- sapply(pops, function(p) {
            df  <- ind_by_pop[[p]]
            idx <- sample.int(nrow(df), nrow(df), replace = TRUE)
            c(mean(df$Latitude[idx]), mean(df$Longitude[idx]))
          })
          for (i in seq_len(n - 1L)) for (j in (i + 1L):n) {
            boot_arr[j, i, b] <- .iod_haversine_km(
              cent_b[1L, i], cent_b[2L, i], cent_b[1L, j], cent_b[2L, j])
          }
        }
        setProgress(0.85, detail = "Assembling results\u2026")

        rows <- vector("list", n * (n - 1L) / 2L); k <- 1L
        for (i in seq_len(n - 1L)) for (j in (i + 1L):n) {
          bd <- boot_arr[j, i, ]; bd <- bd[is.finite(bd)]
          ci <- if (length(bd) >= 2L) unname(quantile(bd, c(alpha, 1 - alpha))) else c(NA_real_, NA_real_)
          d_obs  <- dist_obs[j, i]
          ln_obs <- if (is.finite(d_obs) && d_obs > 0) log(d_obs) else NA_real_
          ln_bd  <- ifelse(bd > 0, log(bd), NA_real_)
          ci_ln  <- if (length(bd) >= 2L) unname(quantile(ln_bd, c(alpha, 1 - alpha), na.rm = TRUE))
                    else c(NA_real_, NA_real_)
          rows[[k]] <- data.frame(
            Pop1 = pops[i], Pop2 = pops[j],
            Dgeo_km    = d_obs,  Dgeo_ci_l  = ci[1L],    Dgeo_ci_u  = ci[2L],
            lnDgeo     = ln_obs, lnDgeo_ci_l = ci_ln[1L], lnDgeo_ci_u = ci_ln[2L],
            stringsAsFactors = FALSE)
          k <- k + 1L
        }
        long_df <- do.call(rbind, rows)
        setProgress(1.0)
      })

      list(matrix = dist_obs, long = long_df, pops = pops, n_boot = n_boot)
    })

    # ── Value boxes (Tab 1) ─────────────────────────────────────────────────
    output$box_npops_geo <- renderValueBox({
      valueBox(length(geo_results_r()$pops), HTML("Populations"),
               icon = icon("map-marker-alt"), color = "teal")
    })
    output$box_npairs_geo <- renderValueBox({
      valueBox(nrow(geo_results_r()$long), HTML("Pairs"),
               icon = icon("project-diagram"), color = "blue")
    })
    output$box_avg_dgeo <- renderValueBox({
      v <- mean(geo_results_r()$long$Dgeo_km, na.rm = TRUE)
      valueBox(round(v, 1), HTML("Avg D<sub>geo</sub><br>(km)"),
               icon = icon("ruler"), color = "purple")
    })
    output$box_max_dgeo <- renderValueBox({
      v <- max(geo_results_r()$long$Dgeo_km, na.rm = TRUE)
      valueBox(round(v, 1), HTML("Max D<sub>geo</sub><br>(km)"),
               icon = icon("expand-arrows-alt"), color = "navy")
    })

    # ── Half-matrix display ─────────────────────────────────────────────────
    output$ui_geo_matrix <- renderUI({
      r <- tryCatch(geo_results_r(), error = function(e) NULL)
      if (is.null(r)) return(tags$p("Click 'Compute Geographic Distances' first.",
                                    style = "color:#94a3b8;"))
      .iod_render_mat_html(r$matrix, digits = 1)
    })

    # ── Long format table + downloads ───────────────────────────────────────
    output$dt_geo_long <- DT::renderDT({
      df <- geo_results_r()$long
      num_cols <- setdiff(names(df), c("Pop1", "Pop2"))
      df[num_cols] <- lapply(df[num_cols], round, 4)
      DT::datatable(df, rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 15, dom = "lrtip"),
        class   = "compact stripe hover",
        colnames = c("Pop\u00a01","Pop\u00a02","Dgeo (km)","CI lo","CI hi",
                     "ln(Dgeo)","CI lo (ln)","CI hi (ln)"))
    })
    output$dl_geo_csv <- downloadHandler(
      filename = function() paste0("geographic_distances_", Sys.Date(), ".csv"),
      content  = function(file) write.csv(geo_results_r()$long, file, row.names = FALSE)
    )
    output$dl_geo_txt <- downloadHandler(
      filename = function() paste0("geographic_distances_", Sys.Date(), ".txt"),
      content  = function(file)
        write.table(geo_results_r()$long, file, sep = "\t", row.names = FALSE, quote = FALSE)
    )

    # ══════════════════════════════════════════════════════════════════════
    # TAB 2 — Mantel test
    # ══════════════════════════════════════════════════════════════════════

    # ── Matrix 2: internal Dgeo / ln(Dgeo), from Tab 1 results ──────────────
    matrix2_r <- reactive({
      r <- tryCatch(geo_results_r(), error = function(e) NULL)
      shiny::validate(shiny::need(!is.null(r),
        "Please compute geographic distances in Tab 1 first."))
      m <- r$matrix
      # symmetrize (matrix is currently lower-triangle only)
      m_full <- m
      for (i in seq_len(nrow(m))) for (j in seq_len(ncol(m)))
        if (is.na(m_full[i, j]) && !is.na(m_full[j, i])) m_full[i, j] <- m_full[j, i]
      if (identical(input$mat2_choice, "ln")) {
        m_full <- ifelse(m_full > 0, log(m_full), NA_real_)
        dimnames(m_full) <- dimnames(m)
      }
      m_full
    })

    # ── Matrix 1: parse uploaded file ───────────────────────────────────────
    mat1_raw_df_r <- reactive({
      shiny::req(input$mat1_file)
      df <- tryCatch(
        read.table(input$mat1_file$datapath, header = TRUE, sep = input$mat1_sep,
                   stringsAsFactors = FALSE, check.names = FALSE,
                   fill = TRUE, quote = "\""),
        error = function(e) NULL)
      shiny::validate(shiny::need(!is.null(df) && nrow(df) >= 1L,
        "Could not parse the uploaded file. Check separator / header settings."))
      df
    })

    output$ui_mat1_cols <- renderUI({
      df   <- tryCatch(mat1_raw_df_r(), error = function(e) NULL)
      cols <- if (is.null(df)) character(0) else names(df)
      tagList(
        selectInput(session$ns("mat1_pop1col"), "Population 1 column:",
                    choices = cols, selected = cols[1]),
        selectInput(session$ns("mat1_pop2col"), "Population 2 column:",
                    choices = cols, selected = cols[min(2L, length(cols))]),
        selectInput(session$ns("mat1_valcol"), "Value column:",
                    choices = cols, selected = cols[min(3L, length(cols))])
      )
    })

    matrix1_r <- reactive({
      df <- mat1_raw_df_r()

      if (identical(input$mat1_format, "square")) {
        labs <- as.character(df[[1L]])
        vals <- as.matrix(df[, -1, drop = FALSE])
        vals <- apply(vals, c(1L, 2L), function(x) suppressWarnings(as.numeric(x)))
        shiny::validate(shiny::need(nrow(vals) == ncol(vals),
          "Square format requires the same number of data rows as data columns ",
          "(first column = row labels, header = column labels)."))
        rownames(vals) <- labs
        colnames(vals) <- labs
        for (i in seq_len(nrow(vals))) for (j in seq_len(ncol(vals)))
          if (is.na(vals[i, j]) && !is.na(vals[j, i])) vals[i, j] <- vals[j, i]
        diag(vals) <- NA_real_
        vals

      } else {
        shiny::req(input$mat1_pop1col, input$mat1_pop2col, input$mat1_valcol)
        p1  <- as.character(df[[input$mat1_pop1col]])
        p2  <- as.character(df[[input$mat1_pop2col]])
        val <- suppressWarnings(as.numeric(df[[input$mat1_valcol]]))
        labels <- sort(unique(c(p1, p2)))
        m <- matrix(NA_real_, length(labels), length(labels),
                    dimnames = list(labels, labels))
        for (k in seq_along(p1)) { m[p1[k], p2[k]] <- val[k]; m[p2[k], p1[k]] <- val[k] }
        m
      }
    })

    # ── Run Mantel test ──────────────────────────────────────────────────────
    mantel_result_r <- eventReactive(input$run_mantel, {
      m1 <- matrix1_r()
      m2 <- matrix2_r()
      shiny::validate(shiny::need(
        length(intersect(rownames(m1), rownames(m2))) >= 3L,
        "Fewer than 3 population labels are common to both matrices. ",
        "Check that population names match exactly (case-sensitive) between ",
        "your uploaded Matrix 1 and this app's population names."
      ))
      n_perm <- as.integer(input$n_perm_mantel)
      stat   <- input$mantel_stat

      withProgress(message = "Running Mantel test\u2026",
                   detail  = sprintf("%d permutations\u2026", n_perm),
                   value   = 0.2, {
        res <- .iod_mantel_matrix(m1, m2, n_perm = n_perm, stat = stat)
        setProgress(1.0)
      })

      res$stat_label  <- if (stat == "b") "Slope b" else "Pearson r"
      res$mat2_choice <- input$mat2_choice
      res
    })

    # ── Value boxes (Tab 2) ──────────────────────────────────────────────────
    output$box_m_stat <- renderValueBox({
      r <- mantel_result_r()
      valueBox(round(r$stat_obs, 4), HTML(paste0(r$stat_label, "<br>(observed)")),
               icon = icon("chart-line"), color = "purple")
    })
    output$box_m_pval <- renderValueBox({
      r   <- mantel_result_r(); pv <- r$p_value
      col <- if (is.na(pv)) "yellow" else if (pv < 0.05) "green" else if (pv < 0.10) "yellow" else "red"
      valueBox(if (is.na(pv)) "NA" else formatC(pv, format = "f", digits = 4),
                HTML("p-value<br>(one-sided)"), icon = icon("check-circle"), color = col)
    })
    output$box_m_n <- renderValueBox({
      valueBox(mantel_result_r()$n_pairs, HTML("Pairs used (n)"),
               icon = icon("project-diagram"), color = "blue")
    })
    output$box_m_r2 <- renderValueBox({
      r2 <- mantel_result_r()$r2
      valueBox(if (is.na(r2)) "NA" else round(r2, 4), HTML("R\u00b2 (OLS)"),
               icon = icon("percentage"), color = "teal")
    })

    # ── Scatter plot ─────────────────────────────────────────────────────────
    output$mantel_scatter <- plotly::renderPlotly({
      r <- mantel_result_r()
      shiny::req(length(r$x) > 0L)

      x_s <- seq(min(r$x), max(r$x), length.out = 100)
      y_s <- r$intercept + r$slope * x_s
      x_lab <- if (identical(r$mat2_choice, "ln")) "ln(Dgeo)" else "Dgeo (km)"

      plotly::plot_ly() %>%
        plotly::add_markers(
          x = r$x, y = r$y,
          marker = list(color = "#2CBF9F", size = 8, opacity = 0.8),
          name = "Pairs"
        ) %>%
        plotly::add_lines(
          x = x_s, y = y_s,
          line = list(color = "#B40F20", width = 2),
          name = sprintf("OLS: b=%.4f, R\u00b2=%.4f", r$slope, r$r2)
        ) %>%
        plotly::layout(
          xaxis  = list(title = x_lab),
          yaxis  = list(title = "Matrix 1 value"),
          title  = list(
            text = sprintf("%s = %.4f, p = %.4f (n = %d)",
                           r$stat_label, r$stat_obs, r$p_value, r$n_pairs),
            font = list(size = 13)),
          legend = list(x = 0.02, y = 0.98, bgcolor = "rgba(255,255,255,0.8)"),
          margin = list(t = 45)
        )
    })

    # ── Full results text + download ────────────────────────────────────────
    output$mantel_full_results <- renderText({
      r <- mantel_result_r()
      x_lab <- if (identical(r$mat2_choice, "ln")) "ln(Dgeo)" else "Dgeo (km)"
      p_neg <- if (is.na(r$p_value)) NA_real_ else 1 - r$p_value
      paste(
        "Mantel test — joint row/column permutation",
        "================================================",
        sprintf("Matrix 1 vs Matrix 2 (%s)", x_lab),
        sprintf("Statistic            : %s", r$stat_label),
        sprintf("Observed value       : %.6f", r$stat_obs),
        sprintf("Slope (OLS)          : %.6f", r$slope),
        sprintf("Intercept (OLS)      : %.6f", r$intercept),
        sprintf("R-squared (OLS)      : %.6f", r$r2),
        sprintf("Pairs used (n)       : %d", r$n_pairs),
        sprintf("Common populations   : %d", length(r$common)),
        sprintf("Valid permutations   : %d", r$n_perm_valid),
        sprintf("p-value (positive)   : %s", if (is.na(r$p_value)) "NA" else formatC(r$p_value, format="f", digits=4)),
        sprintf("p-value (negative)   : %s", if (is.na(p_neg))    "NA" else formatC(p_neg,    format="f", digits=4)),
        "",
        "Population labels used:",
        paste(r$common, collapse = ", "),
        sep = "\n"
      )
    })

    output$dl_mantel_results <- downloadHandler(
      filename = function() paste0("mantel_results_", Sys.Date(), ".txt"),
      content  = function(file) {
        r <- mantel_result_r()
        x_lab <- if (identical(r$mat2_choice, "ln")) "ln(Dgeo)" else "Dgeo (km)"
        p_neg <- if (is.na(r$p_value)) NA_real_ else 1 - r$p_value
        lines <- c(
          "# Mantel test results — Isolation by Distance module",
          "# Joint row/column permutation (rectangular-matrix safe)",
          paste0("# Matrix 2: ", x_lab),
          paste0("Statistic: ", r$stat_label),
          paste0("Observed value: ", r$stat_obs),
          paste0("Slope: ", r$slope),
          paste0("Intercept: ", r$intercept),
          paste0("R2: ", r$r2),
          paste0("N pairs: ", r$n_pairs),
          paste0("Valid permutations: ", r$n_perm_valid),
          paste0("p-value (positive): ", r$p_value),
          paste0("p-value (negative): ", p_neg),
          "",
          "Pairwise data used (Matrix1_value, Matrix2_value):"
        )
        writeLines(lines, con = file)
        write.table(data.frame(Matrix1 = r$x, Matrix2 = r$y),
                    file, sep = "\t", row.names = FALSE, append = TRUE)
      }
    )

  })
}