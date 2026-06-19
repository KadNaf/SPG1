# module/server_mantel_test.R
# Mantel test on rectangular column-format matrices.
#
# NO genetic distance is recomputed here. This module reads:
#   rv$na_pairwise_table  — produced by server_null_alleles.R (see integration
#                            snippet below) — Pop1, Pop2, FST_raw, FST_ENA,
#                            DCSE_raw, DCSE_INA (+ bootstrap CI columns)
# or an externally uploaded column file (RT / Fstat 2.9.4 layout:
#   Pop1, Pop2, dist1, dist2, ...).
#
# ── Integration snippet to add inside server_null_alleles.R ────────────────
#
#   observeEvent(results_r(), {
#     rv$na_pairwise_table <- file3_data()$data
#   }, ignoreInit = TRUE)
#
# Add this anywhere after `file3_data` is defined (e.g. right after the
# file3_data <- reactive({...}) block). It publishes the same merged
# long-format pairwise table used for File 3 download, so the Mantel module
# can consume it without recalculating anything.
# ─────────────────────────────────────────────────────────────────────────
#
# Mantel test: LABEL permutation (not raw-value shuffling). Population
# labels are randomly permuted; the genetic distance for each *original*
# pair is then looked up at the *permuted* pair position. Pairs whose
# permuted partner has no corresponding entry (because the input table is
# RECTANGULAR / incomplete) are simply skipped for that replicate — this is
# what makes the permutation valid on rectangular matrices.

server_isolation_by_distance <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    `%||%` <- function(a, b) if (!is.null(a)) a else b

    # ══════════════════════════════════════════════════════════════════════
    # Helpers
    # ══════════════════════════════════════════════════════════════════════

    # Order-independent pair key, vectorised
    .mt_key <- function(a, b) {
      a <- as.character(a); b <- as.character(b)
      ifelse(a <= b, paste(a, b, sep = "__"), paste(b, a, sep = "__"))
    }

    # Read an uploaded delimited file
    .mt_read_file <- function(fileinfo, sep, header) {
      df <- tryCatch(
        read.table(fileinfo$datapath, header = header, sep = sep,
                   stringsAsFactors = FALSE, check.names = FALSE,
                   fill = TRUE, quote = "\""),
        error = function(e) NULL)
      shiny::validate(shiny::need(!is.null(df) && nrow(df) >= 3L,
        "Could not parse the file. Check separator / header settings."))
      df
    }

    # Merge an extra distance file onto the base table by normalised
    # (Pop1, Pop2) pair key. The extra file's first two columns are assumed
    # to be the pair identifiers (RT / Fstat convention).
    .mt_merge_extra <- function(base_df, extra_df) {
      shiny::validate(shiny::need(ncol(extra_df) >= 3L,
        "Extra file must have at least 2 ID columns + 1 distance column."))
      id_cols   <- names(extra_df)[1:2]
      val_cols  <- setdiff(names(extra_df), id_cols)
      extra_keep <- extra_df[, val_cols, drop = FALSE]
      extra_keep$.key <- .mt_key(extra_df[[1L]], extra_df[[2L]])
      # In case the extra file has duplicate pairs, keep the first occurrence only
      extra_keep <- extra_keep[!duplicated(extra_keep$.key), , drop = FALSE]

      base_df$.key <- .mt_key(base_df$Pop1, base_df$Pop2)
      merged <- merge(base_df, extra_keep, by = ".key", all.x = TRUE, sort = FALSE)
      merged$.key <- NULL
      merged
    }

    # Core Mantel test with label permutation — rectangular-matrix safe.
    # df must contain pop1_col, pop2_col, x_col, y_col.
    .mt_mantel <- function(df, pop1_col, pop2_col, x_col, y_col,
                            n_perm = 9999L, stat = "r") {

      df <- df[is.finite(df[[x_col]]) & is.finite(df[[y_col]]), , drop = FALSE]
      n  <- nrow(df)
      if (n < 3L)
        return(list(stat_obs = NA_real_, p_value = NA_real_, n_pairs = n,
                    r2 = NA_real_, slope = NA_real_, intercept = NA_real_,
                    perm_stats = numeric(0L), data_used = df))

      x  <- df[[x_col]]; y <- df[[y_col]]
      p1 <- as.character(df[[pop1_col]]); p2 <- as.character(df[[pop2_col]])
      all_labels <- unique(c(p1, p2))

      keys   <- .mt_key(p1, p2)
      lookup <- setNames(x, keys)   # X value indexed by normalised pair key

      stat_fn <- function(xv, yv) {
        ok <- is.finite(xv) & is.finite(yv)
        if (sum(ok) < 3L) return(NA_real_)
        if (stat == "b") unname(coef(lm(yv[ok] ~ xv[ok]))[2L])
        else             suppressWarnings(cor(xv[ok], yv[ok]))
      }

      stat_obs <- stat_fn(x, y)

      # Permutation: randomly relabel populations, then re-look-up X at the
      # permuted (i,j) position; Y (and the pair list) stay fixed.
      perm_stats <- vapply(seq_len(n_perm), function(.b) {
        sigma   <- setNames(sample(all_labels), all_labels)
        perm_k  <- .mt_key(sigma[p1], sigma[p2])
        x_perm  <- unname(lookup[perm_k])     # NA where the permuted pair
        stat_fn(x_perm, y)                    # is absent (rectangular matrix)
      }, numeric(1L))

      perm_fin <- perm_stats[is.finite(perm_stats)]
      p_value  <- if (length(perm_fin) > 0L && is.finite(stat_obs))
                    mean(perm_fin >= stat_obs) else NA_real_

      lm0 <- tryCatch(lm(y ~ x), error = function(e) NULL)
      slp <- if (!is.null(lm0)) unname(coef(lm0)[2L]) else NA_real_
      icp <- if (!is.null(lm0)) unname(coef(lm0)[1L]) else NA_real_
      r2  <- if (!is.null(lm0)) summary(lm0)$r.squared else NA_real_

      list(stat_obs = stat_obs, p_value = p_value, n_pairs = n,
           r2 = r2, slope = slp, intercept = icp,
           perm_stats = perm_fin, data_used = df)
    }

    .guess_col <- function(cols, patterns, fallback) {
      for (pat in patterns) {
        hit <- grep(pat, cols, value = TRUE, ignore.case = TRUE)
        if (length(hit)) return(hit[1L])
      }
      fallback
    }

    # ══════════════════════════════════════════════════════════════════════
    # Data source
    # ══════════════════════════════════════════════════════════════════════

    base_df_r <- reactive({
      if (input$mt_source == "computed") {
        shiny::validate(shiny::need(
          !is.null(rv$na_pairwise_table) && nrow(rv$na_pairwise_table) > 0L,
          "No pairwise table found yet. Please run the Null Allele / FST-ENA module first."
        ))
        df <- rv$na_pairwise_table
        if (isTRUE(input$mt_use_extra)) {
          shiny::req(input$mt_extra_file)
          extra <- .mt_read_file(input$mt_extra_file, input$mt_extra_sep, input$mt_extra_header)
          df <- .mt_merge_extra(df, extra)
        }
        df
      } else {
        shiny::req(input$mt_file)
        .mt_read_file(input$mt_file, input$mt_sep, input$mt_header)
      }
    })

    output$dt_preview <- DT::renderDT({
      df <- base_df_r()
      DT::datatable(df, rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 10, dom = "lrtip"),
        class = "compact stripe hover")
    })

    output$dl_mantel_data <- downloadHandler(
      filename = function() paste0("mantel_input_data_", Sys.Date(), ".csv"),
      content  = function(file) write.csv(base_df_r(), file, row.names = FALSE)
    )

    output$vb_npairs_avail <- renderUI({
      tryCatch(tags$span(nrow(base_df_r())), error = function(e) tags$span("\u2014"))
    })

    # ── Dynamic column selectors ─────────────────────────────────────────
    output$col_pop1_ui <- renderUI({
      cols <- tryCatch(names(base_df_r()), error = function(e) character(0))
      def  <- .guess_col(cols, c("^Pop1$", "Pop.?1", "^From$"), cols[1])
      selectInput(session$ns("col_pop1"), "Population 1 column:",
                  choices = cols, selected = def)
    })
    output$col_pop2_ui <- renderUI({
      cols <- tryCatch(names(base_df_r()), error = function(e) character(0))
      def  <- .guess_col(cols, c("^Pop2$", "Pop.?2", "^To$"), cols[min(2L, length(cols))])
      selectInput(session$ns("col_pop2"), "Population 2 column:",
                  choices = cols, selected = def)
    })
    output$col_x_ui <- renderUI({
      df   <- tryCatch(base_df_r(), error = function(e) NULL)
      cols <- if (is.null(df)) character(0) else names(df)[sapply(df, is.numeric)]
      def  <- .guess_col(cols, c("Dist_km", "Dgeo", "geo", "Dist", "ECOL"),
                         if (length(cols)) cols[1] else NULL)
      selectInput(session$ns("col_x"), "X column (predictor distance):",
                  choices = cols, selected = def)
    })
    output$col_y_ui <- renderUI({
      df   <- tryCatch(base_df_r(), error = function(e) NULL)
      cols <- if (is.null(df)) character(0) else names(df)[sapply(df, is.numeric)]
      def  <- .guess_col(cols, c("^FST_ENA$", "^FST_raw$", "^DCSE_INA$", "^DCSE_raw$"),
                         if (length(cols) >= 2L) cols[2] else if (length(cols)) cols[1] else NULL)
      selectInput(session$ns("col_y"), "Y column (genetic / response distance):",
                  choices = cols, selected = def)
    })

    # ══════════════════════════════════════════════════════════════════════
    # Run Mantel test
    # ══════════════════════════════════════════════════════════════════════

    mantel_result_r <- eventReactive(input$run_mantel, {
      df <- base_df_r()
      shiny::req(input$col_pop1, input$col_pop2, input$col_x, input$col_y)

      pop1c <- input$col_pop1; pop2c <- input$col_pop2
      xcol  <- input$col_x;   ycol  <- input$col_y

      shiny::validate(
        shiny::need(all(c(pop1c, pop2c, xcol, ycol) %in% names(df)),
          "Selected columns not found in the loaded data."),
        shiny::need(pop1c != pop2c, "Population 1 and Population 2 must be different columns."),
        shiny::need(xcol  != ycol,  "X and Y must be different columns.")
      )

      df[[xcol]] <- suppressWarnings(as.numeric(df[[xcol]]))
      df[[ycol]] <- suppressWarnings(as.numeric(df[[ycol]]))

      # Optional exclusion of specific pairs — demonstrates rectangular support
      if (nzchar(trimws(input$mt_exclude %||% ""))) {
        excl <- trimws(strsplit(input$mt_exclude, ",")[[1L]])
        excl <- excl[nzchar(excl)]
        if (length(excl)) {
          key_df   <- .mt_key(df[[pop1c]], df[[pop2c]])
          key_excl <- vapply(excl, function(s) {
            ids <- trimws(strsplit(s, "-")[[1L]])
            if (length(ids) == 2L) .mt_key(ids[1], ids[2]) else NA_character_
          }, character(1L))
          df <- df[!(key_df %in% key_excl), , drop = FALSE]
        }
      }

      if (isTRUE(input$mt_log_x)) df[[xcol]] <- ifelse(df[[xcol]] > 0, log(df[[xcol]]), NA_real_)
      if (isTRUE(input$mt_log_y)) df[[ycol]] <- ifelse(df[[ycol]] > 0, log(df[[ycol]]), NA_real_)

      n_perm <- as.integer(input$mt_n_perm)
      stat   <- input$mt_stat

      withProgress(message = "Running Mantel test\u2026",
                   detail  = sprintf("%d permutations\u2026", n_perm),
                   value   = 0.1, {
        res <- .mt_mantel(df, pop1c, pop2c, xcol, ycol, n_perm = n_perm, stat = stat)
        setProgress(1.0)
      })

      res$xcol_label <- xcol
      res$ycol_label <- ycol
      res$stat_label <- if (stat == "b") "Slope b" else "Pearson r"
      res
    })

    # ── Status banner ───────────────────────────────────────────────────
    output$ui_mantel_status <- renderUI({
      r <- tryCatch(mantel_result_r(), error = function(e) NULL)
      if (is.null(r)) return(NULL)
      tags$div(class = "mt-info", style = "margin-top:.6rem;",
        icon("check-circle"), " ",
        sprintf("Mantel test complete \u2014 %d pairs used, %d valid permutations.",
                r$n_pairs, length(r$perm_stats))
      )
    })

    # ── Value boxes ──────────────────────────────────────────────────────
    output$vb_npairs_used <- renderUI({
      tryCatch(tags$span(mantel_result_r()$n_pairs), error = function(e) tags$span("\u2014"))
    })
    output$vb_stat <- renderUI({
      tryCatch({
        r <- mantel_result_r()
        tags$span(round(r$stat_obs, 4))
      }, error = function(e) tags$span("\u2014"))
    })
    output$vb_pval <- renderUI({
      tryCatch({
        r  <- mantel_result_r(); pv <- r$p_value
        col <- if (is.na(pv)) "#64748b" else if (pv < 0.05) "#166534" else if (pv < 0.10) "#854d0e" else "#9d174d"
        tags$span(style = paste0("color:", col, ";"),
                  if (is.na(pv)) "\u2014" else formatC(pv, format = "f", digits = 4))
      }, error = function(e) tags$span("\u2014"))
    })
    output$vb_r2 <- renderUI({
      tryCatch({
        r2 <- mantel_result_r()$r2
        tags$span(if (is.na(r2)) "\u2014" else round(r2, 4))
      }, error = function(e) tags$span("\u2014"))
    })

    # ── Summary box ──────────────────────────────────────────────────────
    output$ui_mantel_summary <- renderUI({
      r <- tryCatch(mantel_result_r(), error = function(e) NULL)
      if (is.null(r)) return(tags$p("Run the Mantel test to see results.", style = "color:#94a3b8;"))
      p_neg <- if (is.na(r$p_value)) NA_real_ else 1 - r$p_value
      tags$div(class = "mt-result-box",
        tags$strong(sprintf("%s = %.4f", r$stat_label, r$stat_obs)), tags$br(),
        sprintf("One-sided p (positive association) = %s",
                if (is.na(r$p_value)) "NA" else formatC(r$p_value, format="f", digits=4)), tags$br(),
        sprintf("One-sided p (negative association) = %s",
                if (is.na(p_neg)) "NA" else formatC(p_neg, format="f", digits=4)), tags$br(),
        sprintf("R\u00b2 (OLS) = %s", if (is.na(r$r2)) "NA" else formatC(r$r2, format="f", digits=4)), tags$br(),
        sprintf("Regression: Y = %.4f + %.4f \u00d7 X", r$intercept, r$slope), tags$br(),
        sprintf("Pairs used: %d  \u00b7  Valid permutations: %d", r$n_pairs, length(r$perm_stats))
      )
    })

    # ── Histogram of permutation distribution ───────────────────────────
    output$mt_hist <- plotly::renderPlotly({
      r  <- mantel_result_r()
      pv <- r$perm_stats
      shiny::req(length(pv) > 0L)

      plotly::plot_ly() %>%
        plotly::add_histogram(
          x = pv, nbinsx = 60,
          marker = list(color = "rgba(124,58,237,0.55)",
                        line  = list(color = "rgba(124,58,237,1)", width = 0.4)),
          name = "Permuted"
        ) %>%
        plotly::layout(
          shapes = list(list(type = "line", x0 = r$stat_obs, x1 = r$stat_obs,
                              y0 = 0, y1 = 1, yref = "paper",
                              line = list(color = "#B40F20", width = 2, dash = "dash"))),
          xaxis  = list(title = r$stat_label),
          yaxis  = list(title = "Count"),
          title  = list(text = sprintf("n = %d permutations", length(pv)), font = list(size = 11)),
          margin = list(t = 40, b = 40),
          showlegend = FALSE,
          annotations = list(list(
            x = r$stat_obs, y = 0.95, xref = "x", yref = "paper",
            text = sprintf("obs. = %.4f<br>p = %.4f", r$stat_obs, r$p_value),
            showarrow = TRUE, arrowhead = 2,
            font = list(size = 10, color = "#B40F20"),
            bgcolor = "rgba(255,255,255,0.85)"
          ))
        )
    })

    # ── Scatter plot ─────────────────────────────────────────────────────
    output$mt_scatter <- plotly::renderPlotly({
      r  <- mantel_result_r()
      df <- r$data_used
      shiny::req(!is.null(df) && nrow(df) > 0L)

      x_v <- df[[r$xcol_label]]; y_v <- df[[r$ycol_label]]
      x_s <- seq(min(x_v, na.rm = TRUE), max(x_v, na.rm = TRUE), length.out = 100)
      y_s <- r$intercept + r$slope * x_s

      plotly::plot_ly() %>%
        plotly::add_markers(
          x = x_v, y = y_v,
          marker = list(color = "#7c3aed", size = 7, opacity = 0.75),
          name = "Pairs"
        ) %>%
        plotly::add_lines(
          x = x_s, y = y_s,
          line = list(color = "#B40F20", width = 1.5),
          name = sprintf("OLS: b=%.4f, R\u00b2=%.4f", r$slope, r$r2)
        ) %>%
        plotly::layout(
          xaxis  = list(title = r$xcol_label),
          yaxis  = list(title = r$ycol_label),
          title  = list(text = sprintf("%s = %.4f, p = %.4f", r$stat_label, r$stat_obs, r$p_value),
                        font = list(size = 11)),
          legend = list(x = 0.02, y = 0.98, bgcolor = "rgba(255,255,255,0.8)"),
          margin = list(t = 40, b = 40),
          showlegend = TRUE
        )
    })

    # ── Pairs-used table & download ─────────────────────────────────────
    output$dt_pairs_used <- DT::renderDT({
      r <- tryCatch(mantel_result_r(), error = function(e) NULL)
      shiny::validate(shiny::need(!is.null(r), "Run the Mantel test first."))
      df <- r$data_used
      DT::datatable(df, rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 10, dom = "lrtip"),
        class = "compact stripe hover")
    })

    output$dl_pairs_used <- downloadHandler(
      filename = function() paste0("mantel_pairs_used_", Sys.Date(), ".csv"),
      content  = function(file) write.csv(mantel_result_r()$data_used, file, row.names = FALSE)
    )

  })
}