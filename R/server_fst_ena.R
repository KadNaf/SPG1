# module/server_fst_ena.R
# FST-ENA (correction allèles nuls) et DCSE-INA (distance Cavalli-Sforza & Edwards corrigée)
# Traduction exacte des algorithmes FreeNA Pascal — Chapuis & Estoup (2007)
#
# Références :
#   Weir (1996)                    — Méthode Genepop pour FST
#   Cavalli-Sforza & Edwards (1967) — Distance génétique DCSE
#   Chapuis & Estoup (2007)        — Algorithmes ENA et INA, correction allèles nuls
#   Dempster, Laird & Rubin (1977) — Algorithme EM (estimation fréquence allèles nuls)

server_fst_ena <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    # ── Helpers ────────────────────────────────────────────────────────────
    `%||%` <- function(a, b) if (!is.null(a)) a else b
    safe_choice <- function(x, default = "all") {
      if (is.null(x) || length(x) == 0L || identical(x, "") || all(is.na(x))) default
      else as.character(x[[1]])
    }
    sql_id  <- function(con, x) as.character(DBI::dbQuoteIdentifier(con, x))
    sql_str <- function(con, x) as.character(DBI::dbQuoteString(con, x))
    na_val  <- function(x) is.null(x) || length(x) == 0L || is.na(x) || is.nan(x) ||
                            (!is.na(x) && x >= 20000)

    # ── DB plumbing ────────────────────────────────────────────────────────
    db_tick    <- reactive({ rv$db_tick })
    con_r      <- reactive({ req(rv$con); rv$con })
    tbl_meta_r <- reactive({ rv$tbl_meta %||% "meta" })

    tbl_hf_r <- reactive({
      con <- con_r()
      if (exists("duck_tbl_exists", mode = "function", inherits = TRUE) &&
          exists(".duckdb_get_params", mode = "function", inherits = TRUE) &&
          duck_tbl_exists(con, "params")) {
        p <- .duckdb_get_params(con)
        return(as.character(p$tbl_hf %||% "hf"))
      }
      "hf"
    })

    db_ready <- reactive({
      db_tick(); con <- con_r()
      shiny::req(isTRUE(rv$db_ready))
      shiny::validate(
        shiny::need(DBI::dbExistsTable(con, tbl_meta_r()), "DuckDB meta table manquante."),
        shiny::need(DBI::dbExistsTable(con, tbl_hf_r()),   "DuckDB hf table manquante.")
      )
      TRUE
    })

    base_r <- reactive({
      db_ready()
      b <- rv$base_af %||% rv$base %||% rv$base_r %||% rv$genotype_base
      b <- suppressWarnings(as.integer(b))
      if (length(b) == 1L && is.finite(b) && b > 1L) return(as.integer(b))
      con <- con_r()
      if (DBI::dbExistsTable(con, "params") &&
          exists(".duckdb_get_params", mode = "function", inherits = TRUE)) {
        p <- .duckdb_get_params(con)
        b <- suppressWarnings(as.integer(
          p$base %||% p$base_scalar_full %||% p$base_scalar_preview))
        if (length(b) == 1L && is.finite(b) && b > 1L) return(as.integer(b))
      }
      1000L
    })

    hf_schema_r <- reactive({
      db_ready(); con <- con_r()
      info <- DBI::dbGetQuery(con,
        sprintf("PRAGMA table_info(%s)", DBI::dbQuoteIdentifier(con, tbl_hf_r())))
      cols <- info$name
      if (all(c("individual","locus","g") %in% cols))
        return(list(ind_col="individual", locus_col="locus",    gt_col="g"))
      if (all(c("indiv_id","locus_id","gt") %in% cols))
        return(list(ind_col="indiv_id",   locus_col="locus_id", gt_col="gt"))
      shiny::validate(shiny::need(FALSE,
        "hf doit contenir (individual,locus,g) ou (indiv_id,locus_id,gt)."))
    })

    meta_schema_r <- reactive({
      db_ready(); con <- con_r()
      info <- DBI::dbGetQuery(con,
        sprintf("PRAGMA table_info(%s)", DBI::dbQuoteIdentifier(con, tbl_meta_r())))
      cols    <- info$name
      ind_col <- if ("individual" %in% cols) "individual"
                 else if ("indiv_id" %in% cols) "indiv_id"
                 else shiny::validate(shiny::need(FALSE, "Pas de colonne individual dans meta."))
      pop_col <- c("Population","population","pop","pop_code")[
        c("Population","population","pop","pop_code") %in% cols][1]
      shiny::validate(shiny::need(!is.na(pop_col), "Pas de colonne population dans meta."))
      list(ind_col=ind_col, pop_col=pop_col)
    })

    locus_order_cte <- function(con, hf_tbl_q, hl_q)
      sprintf("locus_order AS (
  SELECT CAST(%s AS VARCHAR) AS _lo_marker, MIN(rowid) AS _lo_rank
  FROM %s GROUP BY CAST(%s AS VARCHAR))", hl_q, hf_tbl_q, hl_q)

    # ── Listes markers / populations ──────────────────────────────────────
    pops_r <- reactive({
      db_ready(); con <- con_r(); ms <- meta_schema_r()
      as.character(DBI::dbGetQuery(con, sprintf(
        "SELECT DISTINCT CAST(%s AS VARCHAR) AS p FROM %s
         WHERE %s IS NOT NULL ORDER BY p",
        sql_id(con,ms$pop_col), sql_id(con,tbl_meta_r()),
        sql_id(con,ms$pop_col)))$p)
    })

    markers_r <- reactive({
      db_ready(); con <- con_r(); hs <- hf_schema_r()
      hf_q <- sql_id(con,tbl_hf_r()); hl_q <- sql_id(con,hs$locus_col)
      as.character(DBI::dbGetQuery(con, sprintf("
        WITH %s
        SELECT DISTINCT CAST(%s AS VARCHAR) AS Marker, lo._lo_rank
        FROM %s h
        LEFT JOIN locus_order lo ON CAST(%s AS VARCHAR) = lo._lo_marker
        ORDER BY lo._lo_rank ASC",
        locus_order_cte(con,hf_q,hl_q), hl_q, hf_q, hl_q))$Marker)
    })

    observe({
      markers <- markers_r(); pops <- pops_r()
      updateSelectInput(session, "fl_locus",
        choices  = c("Tous les loci"="all", stats::setNames(markers,markers)),
        selected = "all")
      updateSelectInput(session, "fl_pop1",
        choices  = c("Toutes les paires"="all", stats::setNames(pops,pops)),
        selected = "all")
      updateSelectInput(session, "fl_pop2",
        choices  = c("Toutes les paires"="all", stats::setNames(pops,pops)),
        selected = "all")
    })

    # ══════════════════════════════════════════════════════════════════════
    #  EM ALGORITHM — Traduction exacte de rDempster_per_locus (Pascal FreeNA)
    #
    #  N = efpop[pop] - absentgeno (individus valides, null homos inclus)
    #  rd initialisation :
    #    si nnullhomo > 0 : rd = sqrt(nnullhomo / N)
    #    sinon            : rd = sqrt(1 / (N + 1))
    #
    #  Boucle EM :
    #    corrdgenefreq_new[a] = (p_a + rd) / (p_a + 2*rd) * (H_aa / N)
    #                         + H_aX / (2*N)
    #    rd_new = Σ_a [ rd / (p_a + 2*rd) * (H_aa / N) ]
    #           + 2*nnullhomo / (2*N)          ← contribution des null homos
    #
    #  Retourne :
    #    $rd     : fréquence allèle nul par population
    #    $pfreq  : fréquences alléliques corrigées (vecteur nommé par allèle)
    #    $efpop  : effectif total pop (avec null homos)
    #    $absent : nombre de génotypes absents
    #    $nnullhomo : nombre de null homozygotes
    # ══════════════════════════════════════════════════════════════════════
    em_freena <- function(gt_vec, base) {
      # gt_vec : vecteur d'entiers codés (base×a1 + a2), 0/NA = absent
      efpop     <- length(gt_vec)
      absent_mask <- is.na(gt_vec) | gt_vec <= 0L
      n_absent  <- sum(absent_mask)
      valid_gt  <- gt_vec[!absent_mask]

      if (length(valid_gt) == 0L)
        return(list(rd=0.0, pfreq=numeric(0), efpop=efpop,
                    absent=n_absent, nnullhomo=0L, alleles=integer(0)))

      a1_all <- floor(valid_gt / base)
      a2_all <- valid_gt %% base

      # Allèle nul = valeur maximale (99 ou 999 selon base)
      null_code <- if (base >= 1000L) 999L else 99L

      # Null homozygotes : les deux allèles sont null_code
      null_homo_mask <- (a1_all == null_code) & (a2_all == null_code)
      n_null_homo    <- sum(null_homo_mask)

      # Génotypes valides (ni absents, ni null homos)
      valid_a1 <- a1_all[!null_homo_mask]
      valid_a2 <- a2_all[!null_homo_mask]
      all_alleles <- sort(unique(c(valid_a1, valid_a2)))
      all_alleles <- all_alleles[all_alleles >= 0L & all_alleles != null_code]

      N <- efpop - n_absent   # individus sans absent (null homos inclus)

      if (N == 0L || length(all_alleles) == 0L)
        return(list(rd=0.0, pfreq=numeric(0), efpop=efpop,
                    absent=n_absent, nnullhomo=n_null_homo, alleles=integer(0)))

      # Fréquences alléliques observées (dénominateur = 2*(N - n_null_homo))
      n_valid_geno <- N - n_null_homo
      genefreq <- sapply(all_alleles, function(a)
        (sum(valid_a1==a) + sum(valid_a2==a)) / (2L * n_valid_geno))

      # Initialisation rd (Pascal : rDempster_per_locus)
      rd <- if (n_null_homo > 0L) sqrt(n_null_homo / N)
            else                  sqrt(1.0 / (N + 1.0))

      # Comptages homozygotes et hétérozygotes par allèle
      H_ii <- sapply(all_alleles, function(a)
        sum(valid_a1==a & valid_a2==a))
      H_iX <- sapply(all_alleles, function(a)
        sum((valid_a1==a & valid_a2!=a) | (valid_a2==a & valid_a1!=a)))
      hotot <- sum(H_ii)

      # Initialisation corrdgenefreq (cpt=0, Pascal exact)
      p <- numeric(length(all_alleles))
      for (ai in seq_along(all_alleles)) {
        if (genefreq[ai] <= 0) { p[ai] <- 0.0; next }
        ii <- H_ii[ai]; jj <- H_iX[ai]
        if (n_null_homo > 0L) {
          X <- n_null_homo + hotot - ii +
               ((N - n_null_homo) - hotot) - jj
          Y <- N
        } else {
          X <- 1.0 + hotot - ii + (N - hotot) - jj
          Y <- N + 1.0
        }
        p[ai] <- 1.0 - sqrt(max(0.0, X / Y))
      }

      # Boucle EM (Pascal : repeat … until re=0)
      for (iter in seq_len(5000L)) {
        new_p <- numeric(length(all_alleles))
        rdi   <- 0.0; re <- 0L
        for (ai in seq_along(all_alleles)) {
          if (genefreq[ai] <= 0) { new_p[ai] <- 0.0; next }
          pa    <- p[ai]
          denom <- pa + 2.0 * rd
          if (denom <= 0) { new_p[ai] <- 0.0; next }
          p_new     <- (pa + rd) / denom * (H_ii[ai] / N) +
                       H_iX[ai] / (2.0 * N)
          rdi       <- rdi + rd / denom * (H_ii[ai] / N)
          new_p[ai] <- p_new
          if (abs(p_new - pa) > 1e-6) re <- re + 1L
        }
        # Pascal : rd = rdi + 2*nnullhomo / (2*N)
        rd_new <- rdi + (2.0 * n_null_homo) / (2.0 * N)
        if (abs(rd_new - rd) > 1e-6) re <- re + 1L
        p  <- new_p
        rd <- max(0.0, rd_new)
        if (re == 0L) break
      }

      pfreq <- stats::setNames(p, as.character(all_alleles))
      list(rd=rd, pfreq=pfreq, efpop=efpop,
           absent=n_absent, nnullhomo=n_null_homo, alleles=all_alleles,
           genefreq_obs=stats::setNames(genefreq, as.character(all_alleles)),
           H_ii=stats::setNames(H_ii, as.character(all_alleles)),
           H_iX=stats::setNames(H_iX, as.character(all_alleles)),
           N=N, n_valid_geno=n_valid_geno)
    }

    # ══════════════════════════════════════════════════════════════════════
    #  FST (WEIR 1996) — Traduction de loc_gFst_Genepop / loc_pFst_Genepop
    #  et de loc_gFst_Genepop_correction / loc_pFst_Genepop_correction
    #
    #  Composantes par allèle :
    #    MSG = (0.5*ΣnA - ΣAA) / N_tot
    #    MSI = (0.5*ΣnA + ΣAA - Σ(nA²/n)) / (N_tot - r)
    #    MSP = (Σ(nA²/n) - 0.5*ΣnA²/N_tot) / (r - 1)
    #    s²G = MSG ;  s²I = (MSI − MSG)/2 ;  s²P = (MSP − MSI)/(2*nc)
    #    nc  = (N_tot − Σni²/N_tot) / (r − 1)
    #  FST_locus = S1 / S3 = ΣS²P / Σ(S²P + S²I + S²G)
    #
    #  ENA : nA = corrdgenefreq * 2*ni  ;  AA_corr = AA * p/(p + 2r)
    #        ni = efpop - absentgeno   (null homos inclus dans N)
    #  Brut : nA = genefreq_obs * 2*ni  ;  AA non corrigé
    #         ni = efpop - absent - nnullhomo
    # ══════════════════════════════════════════════════════════════════════

    # Calcule S1 et S3 pour un allèle, une paire/ensemble de pops
    # pop_data = liste de listes : $ni, $nA, $AA, [optionnel $AA_corr]
    weir_fst_allele <- function(pop_data, use_corr = FALSE) {
      r      <- length(pop_data)
      N_tot  <- sum(sapply(pop_data, `[[`, "ni"))
      N_tot2 <- sum(sapply(pop_data, function(p) p$ni^2))
      if (N_tot == 0L || r < 2L) return(list(s1=0.0, s3=0.0))

      nc <- (N_tot - N_tot2 / N_tot) / (r - 1)
      if (nc <= 0 || N_tot - r <= 0) return(list(s1=0.0, s3=0.0))

      snA <- sum(sapply(pop_data, `[[`, "nA"))
      s2A <- sum(sapply(pop_data, function(p) if (p$ni > 0) p$nA^2 / (2 * p$ni) else 0.0))
      sAA <- if (use_corr) sum(sapply(pop_data, `[[`, "AA_corr"))
             else           sum(sapply(pop_data, `[[`, "AA"))

      MSG  <- (0.5 * snA - sAA) / N_tot
      dMSI <- N_tot - r
      MSI  <- if (dMSI > 0) (0.5 * snA + sAA - s2A) / dMSI else 0.0
      MSP  <- (s2A - 0.5 * snA^2 / N_tot) / (r - 1)
      s2G  <- MSG
      s2I  <- 0.5 * (MSI - MSG)
      s2P  <- (MSP - MSI) / (2 * nc)
      list(s1 = s2P, s3 = s2P + s2I + s2G)
    }

    # ══════════════════════════════════════════════════════════════════════
    #  DISTANCE CAVALLI-SFORZA & EDWARDS (1967) — prod_CS / prod_CS_correction
    #
    #  CSprod(i,j) = Σ_k √(p_ik × p_jk)      k = allèles
    #  DCSE(i,j)   = (2/π) × √(2 × (1 − CSprod))   si CSprod ≤ 1
    #  INA : k inclut l'allèle nul (freq = rd[pop])
    # ══════════════════════════════════════════════════════════════════════
    cs_distance <- function(freq_i, freq_j) {
      alleles <- union(names(freq_i), names(freq_j))
      csprod  <- 0.0
      for (a in alleles) {
        pi <- freq_i[a] %||% 0.0
        pj <- freq_j[a] %||% 0.0
        if (!is.na(pi) && !is.na(pj) && pi > 0 && pj > 0)
          csprod <- csprod + sqrt(pi * pj)
      }
      if (csprod > 1.0) return(NA_real_)  # non applicable (arrondi numérique)
      (2.0 / pi) * sqrt(2.0 * (1.0 - csprod))
    }

    # ══════════════════════════════════════════════════════════════════════
    #  EXTRACTION DES DONNÉES + CALCUL EM PAR LOCUS × POPULATION
    # ══════════════════════════════════════════════════════════════════════
    fetch_em_results <- reactive({
      db_ready()
      con   <- con_r(); hs <- hf_schema_r(); ms <- meta_schema_r()
      base  <- as.integer(base_r())
      hf_q  <- sql_id(con, tbl_hf_r());  meta_q <- sql_id(con, tbl_meta_r())
      hi_q  <- sql_id(con, hs$ind_col);  hl_q   <- sql_id(con, hs$locus_col)
      hg_q  <- sql_id(con, hs$gt_col);   mi_q   <- sql_id(con, ms$ind_col)
      pop_q <- sql_id(con, ms$pop_col)

      sql <- sprintf("
        WITH %s
        SELECT
          CAST(m.%s AS VARCHAR) AS Population,
          CAST(h.%s AS VARCHAR) AS Marker,
          h.%s                  AS gt,
          lo._lo_rank
        FROM %s h
        INNER JOIN %s m
          ON CAST(h.%s AS VARCHAR) = CAST(m.%s AS VARCHAR)
        LEFT JOIN locus_order lo
          ON CAST(h.%s AS VARCHAR) = lo._lo_marker
        WHERE m.%s IS NOT NULL
        ORDER BY lo._lo_rank ASC, Population",
        locus_order_cte(con, hf_q, hl_q),
        pop_q, hl_q, hg_q,
        hf_q, meta_q, hi_q, mi_q,
        hl_q, pop_q)

      raw <- DBI::dbGetQuery(con, sql)
      if (nrow(raw) == 0L) return(list())

      markers <- markers_r()
      pops    <- pops_r()

      # Résultats EM : em_res[[locus]][[pop]] = sortie de em_freena()
      em_res <- list()
      for (loc in markers) {
        em_res[[loc]] <- list()
        for (pop in pops) {
          gts <- raw$gt[raw$Marker == loc & raw$Population == pop]
          if (length(gts) == 0L) {
            em_res[[loc]][[pop]] <- list(rd=0.0, pfreq=numeric(0),
              efpop=0L, absent=0L, nnullhomo=0L, alleles=integer(0),
              genefreq_obs=numeric(0), H_ii=numeric(0), H_iX=numeric(0),
              N=0L, n_valid_geno=0L)
          } else {
            em_res[[loc]][[pop]] <- em_freena(gts, base)
          }
        }
      }
      em_res
    })

    # ══════════════════════════════════════════════════════════════════════
    #  CALCUL FST GLOBAL MULTILOCUS (brut + ENA) — sum_stats + sum_stats_correction
    # ══════════════════════════════════════════════════════════════════════
    compute_fst_global <- function(em_res) {
      markers <- names(em_res)
      pops    <- names(em_res[[markers[1]]])
      n_pops  <- length(pops)

      # s1, s3 (brut) et s1_corr, s3_corr (ENA), accumulateurs sur loci
      s1 <- s3 <- s1c <- s3c <- 0.0
      rows <- vector("list", length(markers))

      for (li in seq_along(markers)) {
        loc     <- markers[li]
        em_loc  <- em_res[[loc]]
        # Allèles observés à ce locus (union sur pops)
        alleles_obs  <- sort(unique(unlist(lapply(em_loc, function(e) e$alleles))))
        alleles_corr <- alleles_obs  # même liste ; null ajouté séparément pour INA

        # ── Effectifs ──────────────────────────────────────────────────
        # Brut  : ni = efpop - absent - nnullhomo
        # Corr  : ni = efpop - absent   (null homos inclus)
        ni_raw  <- sapply(pops, function(p) {
          e <- em_loc[[p]]
          max(0L, e$efpop - e$absent - e$nnullhomo)
        })
        ni_corr <- sapply(pops, function(p) {
          e <- em_loc[[p]]
          max(0L, e$efpop - e$absent)
        })

        # Populations effectives (≥ 1 individu)
        r_raw  <- sum(ni_raw  > 0L)
        r_corr <- sum(ni_corr > 0L)

        N_raw   <- sum(ni_raw);   N2_raw  <- sum(ni_raw^2)
        N_corr  <- sum(ni_corr);  N2_corr <- sum(ni_corr^2)

        nc_raw  <- if (N_raw  > 0 && r_raw  > 1) (N_raw  - N2_raw  / N_raw)  / (r_raw  - 1) else 0.0
        nc_corr <- if (N_corr > 0 && r_corr > 1) (N_corr - N2_corr / N_corr) / (r_corr - 1) else 0.0

        s1l <- s3l <- s1lc <- s3lc <- 0.0

        # ── Loop allèles brut ──────────────────────────────────────────
        for (a in alleles_obs) {
          a_chr <- as.character(a)
          pop_data <- lapply(pops, function(p) {
            e  <- em_loc[[p]]
            ni <- max(0L, e$efpop - e$absent - e$nnullhomo)
            pf <- if (!is.null(e$genefreq_obs) && a_chr %in% names(e$genefreq_obs))
                    e$genefreq_obs[a_chr] else 0.0
            nA <- pf * 2L * ni
            AA <- if (!is.null(e$H_ii) && a_chr %in% names(e$H_ii)) e$H_ii[a_chr] else 0L
            list(ni=ni, nA=nA, AA=AA, AA_corr=AA)
          })
          cmp <- weir_fst_allele(pop_data, use_corr = FALSE)
          s1l <- s1l + cmp$s1; s3l <- s3l + cmp$s3
        }

        # ── Loop allèles ENA (corrdgenefreq) ──────────────────────────
        for (a in alleles_obs) {
          a_chr <- as.character(a)
          pop_data_c <- lapply(pops, function(p) {
            e  <- em_loc[[p]]
            ni <- max(0L, e$efpop - e$absent)
            pf <- if (!is.null(e$pfreq) && a_chr %in% names(e$pfreq))
                    e$pfreq[a_chr] else 0.0
            rd <- e$rd
            nA <- pf * 2L * ni
            AA <- if (!is.null(e$H_ii) && a_chr %in% names(e$H_ii)) e$H_ii[a_chr] else 0L
            # AA_corr = AA * p/(p + 2r)   (Pascal : cAA)
            denom <- pf + 2.0 * rd
            AA_c  <- if (AA > 0 && denom > 0) AA * (pf / denom) else 0.0
            list(ni=ni, nA=nA, AA=AA, AA_corr=AA_c)
          })
          cmp_c <- weir_fst_allele(pop_data_c, use_corr = TRUE)
          s1lc <- s1lc + cmp_c$s1; s3lc <- s3lc + cmp_c$s3
        }

        fst_loc  <- if (s3l  != 0) s1l  / s3l  else NA_real_
        fst_locc <- if (s3lc != 0) s1lc / s3lc else NA_real_

        # Accumulation multilocus pondérée par nc (Pascal : sum_stats)
        if (!is.na(fst_loc)  && nc_raw  > 0)
          { s1 <- s1 + s1l * nc_raw;   s3 <- s3 + s3l * nc_raw }
        if (!is.na(fst_locc) && nc_corr > 0)
          { s1c <- s1c + s1lc * nc_corr; s3c <- s3c + s3lc * nc_corr }

        rows[[li]] <- data.frame(
          Locus          = loc,
          FST_brut       = round(fst_loc,  6),
          FST_ENA        = round(fst_locc, 6),
          Delta_FST      = round(fst_locc - fst_loc, 6),
          N_pops_eff_brut  = r_raw,
          N_pops_eff_ENA   = r_corr,
          stringsAsFactors = FALSE
        )
      }

      out_loci   <- do.call(rbind, rows)
      fst_global <- if (s3  > 0) s1  / s3  else NA_real_
      fst_ena    <- if (s3c > 0) s1c / s3c else NA_real_

      list(
        global_raw  = fst_global,
        global_ena  = fst_ena,
        per_locus   = out_loci
      )
    }

    # ══════════════════════════════════════════════════════════════════════
    #  FST PAIRWISE (brut + ENA) — loc_pFst + loc_pFst_correction
    # ══════════════════════════════════════════════════════════════════════
    compute_fst_pairwise <- function(em_res) {
      markers <- names(em_res)
      pops    <- names(em_res[[markers[1]]])
      n_pops  <- length(pops)
      if (n_pops < 2L) return(list(matrix_raw=NULL, matrix_ena=NULL, long=data.frame()))

      # Initialiser accumulateurs pairwise
      s12p  <- matrix(0.0, n_pops, n_pops, dimnames=list(pops,pops))
      s32p  <- matrix(0.0, n_pops, n_pops, dimnames=list(pops,pops))
      s12pc <- matrix(0.0, n_pops, n_pops, dimnames=list(pops,pops))
      s32pc <- matrix(0.0, n_pops, n_pops, dimnames=list(pops,pops))

      for (loc in markers) {
        em_loc <- em_res[[loc]]
        alleles_obs <- sort(unique(unlist(lapply(em_loc, function(e) e$alleles))))

        for (ii in seq_len(n_pops - 1L)) {
          for (jj in seq(ii + 1L, n_pops)) {
            pi_name <- pops[ii]; pj_name <- pops[jj]
            ei <- em_loc[[pi_name]]; ej <- em_loc[[pj_name]]

            ni_raw_i <- max(0L, ei$efpop - ei$absent - ei$nnullhomo)
            ni_raw_j <- max(0L, ej$efpop - ej$absent - ej$nnullhomo)
            ni_c_i   <- max(0L, ei$efpop - ei$absent)
            ni_c_j   <- max(0L, ej$efpop - ej$absent)

            # ── Brut ──────────────────────────────────────────────────
            if (ni_raw_i > 0L && ni_raw_j > 0L) {
              for (a in alleles_obs) {
                a_chr <- as.character(a)
                pop_d <- list(
                  list(ni=ni_raw_i,
                       nA=(if (!is.null(ei$genefreq_obs) && a_chr %in% names(ei$genefreq_obs))
                             ei$genefreq_obs[a_chr] else 0.0) * 2L * ni_raw_i,
                       AA=(if (!is.null(ei$H_ii) && a_chr %in% names(ei$H_ii)) ei$H_ii[a_chr] else 0L),
                       AA_corr=0.0),
                  list(ni=ni_raw_j,
                       nA=(if (!is.null(ej$genefreq_obs) && a_chr %in% names(ej$genefreq_obs))
                             ej$genefreq_obs[a_chr] else 0.0) * 2L * ni_raw_j,
                       AA=(if (!is.null(ej$H_ii) && a_chr %in% names(ej$H_ii)) ej$H_ii[a_chr] else 0L),
                       AA_corr=0.0)
                )
                cmp <- weir_fst_allele(pop_d, use_corr=FALSE)
                N2p <- ni_raw_i + ni_raw_j
                N22p <- ni_raw_i^2 + ni_raw_j^2
                nc  <- if (N2p > 0) (N2p - N22p/N2p) / 1.0 else 0.0
                s12p[ii, jj]  <- s12p[ii, jj]  + cmp$s1 * nc
                s32p[ii, jj]  <- s32p[ii, jj]  + cmp$s3 * nc
              }
            }

            # ── ENA ───────────────────────────────────────────────────
            if (ni_c_i > 0L && ni_c_j > 0L) {
              for (a in alleles_obs) {
                a_chr <- as.character(a)
                pf_i <- if (!is.null(ei$pfreq) && a_chr %in% names(ei$pfreq)) ei$pfreq[a_chr] else 0.0
                pf_j <- if (!is.null(ej$pfreq) && a_chr %in% names(ej$pfreq)) ej$pfreq[a_chr] else 0.0
                AA_i <- if (!is.null(ei$H_ii) && a_chr %in% names(ei$H_ii)) ei$H_ii[a_chr] else 0L
                AA_j <- if (!is.null(ej$H_ii) && a_chr %in% names(ej$H_ii)) ej$H_ii[a_chr] else 0L
                denom_i <- pf_i + 2.0 * ei$rd; AAc_i <- if (AA_i > 0 && denom_i > 0) AA_i*(pf_i/denom_i) else 0.0
                denom_j <- pf_j + 2.0 * ej$rd; AAc_j <- if (AA_j > 0 && denom_j > 0) AA_j*(pf_j/denom_j) else 0.0
                pop_dc <- list(
                  list(ni=ni_c_i, nA=pf_i*2L*ni_c_i, AA=AA_i, AA_corr=AAc_i),
                  list(ni=ni_c_j, nA=pf_j*2L*ni_c_j, AA=AA_j, AA_corr=AAc_j)
                )
                cmp_c <- weir_fst_allele(pop_dc, use_corr=TRUE)
                N2p_c  <- ni_c_i + ni_c_j; N22p_c <- ni_c_i^2 + ni_c_j^2
                nc_c   <- if (N2p_c > 0) (N2p_c - N22p_c/N2p_c) / 1.0 else 0.0
                s12pc[ii, jj] <- s12pc[ii, jj] + cmp_c$s1 * nc_c
                s32pc[ii, jj] <- s32pc[ii, jj] + cmp_c$s3 * nc_c
              }
            }
          }
        }
      }

      # Matrices finales
      mat_raw <- matrix(NA_real_, n_pops, n_pops, dimnames=list(pops,pops))
      mat_ena <- matrix(NA_real_, n_pops, n_pops, dimnames=list(pops,pops))
      for (ii in seq_len(n_pops - 1L)) {
        for (jj in seq(ii + 1L, n_pops)) {
          mat_raw[jj, ii] <- if (s32p[ii,jj]  > 0) s12p[ii,jj]  / s32p[ii,jj]  else NA_real_
          mat_ena[jj, ii] <- if (s32pc[ii,jj] > 0) s12pc[ii,jj] / s32pc[ii,jj] else NA_real_
        }
      }

      # Format long
      long_rows <- list()
      for (ii in seq_len(n_pops - 1L)) {
        for (jj in seq(ii + 1L, n_pops)) {
          long_rows[[length(long_rows)+1]] <- data.frame(
            Pop1      = pops[ii], Pop2 = pops[jj],
            FST_brut  = round(mat_raw[jj,ii], 6),
            FST_ENA   = round(mat_ena[jj,ii], 6),
            Delta_FST = round(mat_ena[jj,ii] - mat_raw[jj,ii], 6),
            stringsAsFactors = FALSE
          )
        }
      }

      list(matrix_raw = mat_raw, matrix_ena = mat_ena,
           long = do.call(rbind, long_rows))
    }

    # ══════════════════════════════════════════════════════════════════════
    #  DISTANCE DCSE PAIRWISE (brut + INA) — prod_CS + prod_CS_correction
    # ══════════════════════════════════════════════════════════════════════
    compute_dc_pairwise <- function(em_res) {
      markers <- names(em_res)
      pops    <- names(em_res[[markers[1]]])
      n_pops  <- length(pops)
      if (n_pops < 2L) return(list(matrix_raw=NULL, matrix_ina=NULL, long=data.frame()))

      # Accumulateurs DCSE
      dc_sum_raw  <- matrix(0.0, n_pops, n_pops, dimnames=list(pops,pops))
      dc_sum_ina  <- matrix(0.0, n_pops, n_pops, dimnames=list(pops,pops))
      nloc_eff_raw <- matrix(length(markers), n_pops, n_pops, dimnames=list(pops,pops))
      nloc_eff_ina <- matrix(length(markers), n_pops, n_pops, dimnames=list(pops,pops))

      for (loc in markers) {
        em_loc <- em_res[[loc]]

        for (ii in seq_len(n_pops - 1L)) {
          for (jj in seq(ii + 1L, n_pops)) {
            pi_n <- pops[ii]; pj_n <- pops[jj]
            ei <- em_loc[[pi_n]]; ej <- em_loc[[pj_n]]

            ni_raw_i <- ei$efpop - ei$absent - ei$nnullhomo
            ni_raw_j <- ej$efpop - ej$absent - ej$nnullhomo

            # ── DCSE brute (genefreq_obs, allèles nuls exclus) ────────
            if (ni_raw_i > 0L && ni_raw_j > 0L &&
                !is.null(ei$genefreq_obs) && !is.null(ej$genefreq_obs)) {
              d_raw <- cs_distance(ei$genefreq_obs, ej$genefreq_obs)
              if (!is.na(d_raw)) dc_sum_raw[jj, ii] <- dc_sum_raw[jj, ii] + d_raw
              else               nloc_eff_raw[jj, ii] <- nloc_eff_raw[jj, ii] - 1L
            } else {
              nloc_eff_raw[jj, ii] <- nloc_eff_raw[jj, ii] - 1L
            }

            # ── DCSE-INA (corrdgenefreq + allèle nul comme état supp) ─
            ni_c_i <- ei$efpop - ei$absent
            ni_c_j <- ej$efpop - ej$absent
            if (ni_c_i > 0L && ni_c_j > 0L &&
                !is.null(ei$pfreq) && !is.null(ej$pfreq)) {
              # Ajout de l'allèle nul avec freq = rd  (Pascal : ajustement_r)
              freq_ina_i <- c(ei$pfreq, `null`=ei$rd)
              freq_ina_j <- c(ej$pfreq, `null`=ej$rd)
              d_ina <- cs_distance(freq_ina_i, freq_ina_j)
              if (!is.na(d_ina)) dc_sum_ina[jj, ii] <- dc_sum_ina[jj, ii] + d_ina
              else               nloc_eff_ina[jj, ii] <- nloc_eff_ina[jj, ii] - 1L
            } else {
              nloc_eff_ina[jj, ii] <- nloc_eff_ina[jj, ii] - 1L
            }
          }
        }
      }

      # Matrices finales (moyennes sur loci valides)
      mat_raw <- matrix(NA_real_, n_pops, n_pops, dimnames=list(pops,pops))
      mat_ina <- matrix(NA_real_, n_pops, n_pops, dimnames=list(pops,pops))
      for (ii in seq_len(n_pops - 1L)) {
        for (jj in seq(ii + 1L, n_pops)) {
          if (nloc_eff_raw[jj,ii] > 0L)
            mat_raw[jj,ii] <- dc_sum_raw[jj,ii] / nloc_eff_raw[jj,ii]
          if (nloc_eff_ina[jj,ii] > 0L)
            mat_ina[jj,ii] <- dc_sum_ina[jj,ii] / nloc_eff_ina[jj,ii]
        }
      }

      long_rows <- list()
      for (ii in seq_len(n_pops - 1L)) {
        for (jj in seq(ii + 1L, n_pops)) {
          long_rows[[length(long_rows)+1]] <- data.frame(
            Pop1      = pops[ii], Pop2 = pops[jj],
            DCSE_brut = round(mat_raw[jj,ii], 6),
            DCSE_INA  = round(mat_ina[jj,ii], 6),
            Delta_DCSE = round(mat_ina[jj,ii] - mat_raw[jj,ii], 6),
            stringsAsFactors = FALSE
          )
        }
      }

      list(matrix_raw=mat_raw, matrix_ina=mat_ina,
           long=do.call(rbind, long_rows))
    }

    # ══════════════════════════════════════════════════════════════════════
    #  FST PAR LOCUS × PAIRE
    # ══════════════════════════════════════════════════════════════════════
    compute_fst_per_locus_pair <- function(em_res, sel_locus="all",
                                           sel_pop1="all", sel_pop2="all") {
      markers <- names(em_res)
      pops    <- names(em_res[[markers[1]]])

      if (!identical(sel_locus,"all")) markers <- markers[markers == sel_locus]
      pairs <- if (!identical(sel_pop1,"all") && !identical(sel_pop2,"all"))
                 list(c(sel_pop1, sel_pop2))
               else {
                 pp <- combn(pops, 2, simplify=FALSE)
                 if (!identical(sel_pop1,"all"))
                   pp <- pp[sapply(pp, function(x) sel_pop1 %in% x)]
                 else if (!identical(sel_pop2,"all"))
                   pp <- pp[sapply(pp, function(x) sel_pop2 %in% x)]
                 pp
               }

      rows <- list()
      for (loc in markers) {
        em_loc <- em_res[[loc]]
        alleles_obs <- sort(unique(unlist(lapply(em_loc, function(e) e$alleles))))

        for (pair in pairs) {
          pi_n <- pair[1]; pj_n <- pair[2]
          if (!pi_n %in% pops || !pj_n %in% pops) next
          ei <- em_loc[[pi_n]]; ej <- em_loc[[pj_n]]

          ni_raw_i <- max(0L, ei$efpop - ei$absent - ei$nnullhomo)
          ni_raw_j <- max(0L, ej$efpop - ej$absent - ej$nnullhomo)
          ni_c_i   <- max(0L, ei$efpop - ei$absent)
          ni_c_j   <- max(0L, ej$efpop - ej$absent)

          s1_r <- s3_r <- s1_c <- s3_c <- 0.0

          for (a in alleles_obs) {
            a_chr <- as.character(a)
            # Brut
            pf_i_obs <- if (!is.null(ei$genefreq_obs) && a_chr %in% names(ei$genefreq_obs)) ei$genefreq_obs[a_chr] else 0.0
            pf_j_obs <- if (!is.null(ej$genefreq_obs) && a_chr %in% names(ej$genefreq_obs)) ej$genefreq_obs[a_chr] else 0.0
            AA_i <- if (!is.null(ei$H_ii) && a_chr %in% names(ei$H_ii)) ei$H_ii[a_chr] else 0L
            AA_j <- if (!is.null(ej$H_ii) && a_chr %in% names(ej$H_ii)) ej$H_ii[a_chr] else 0L
            if (ni_raw_i > 0L && ni_raw_j > 0L) {
              pd <- list(
                list(ni=ni_raw_i, nA=pf_i_obs*2L*ni_raw_i, AA=AA_i, AA_corr=AA_i),
                list(ni=ni_raw_j, nA=pf_j_obs*2L*ni_raw_j, AA=AA_j, AA_corr=AA_j))
              cmp <- weir_fst_allele(pd, use_corr=FALSE)
              N2p <- ni_raw_i+ni_raw_j; nc <- if (N2p>0) (N2p-(ni_raw_i^2+ni_raw_j^2)/N2p)/1.0 else 0.0
              s1_r <- s1_r + cmp$s1*nc; s3_r <- s3_r + cmp$s3*nc
            }
            # ENA
            pf_i <- if (!is.null(ei$pfreq) && a_chr %in% names(ei$pfreq)) ei$pfreq[a_chr] else 0.0
            pf_j <- if (!is.null(ej$pfreq) && a_chr %in% names(ej$pfreq)) ej$pfreq[a_chr] else 0.0
            denom_i <- pf_i + 2.0*ei$rd; AAc_i <- if (AA_i>0&&denom_i>0) AA_i*(pf_i/denom_i) else 0.0
            denom_j <- pf_j + 2.0*ej$rd; AAc_j <- if (AA_j>0&&denom_j>0) AA_j*(pf_j/denom_j) else 0.0
            if (ni_c_i > 0L && ni_c_j > 0L) {
              pdc <- list(
                list(ni=ni_c_i, nA=pf_i*2L*ni_c_i, AA=AA_i, AA_corr=AAc_i),
                list(ni=ni_c_j, nA=pf_j*2L*ni_c_j, AA=AA_j, AA_corr=AAc_j))
              cmp_c <- weir_fst_allele(pdc, use_corr=TRUE)
              N2pc <- ni_c_i+ni_c_j; nc_c <- if (N2pc>0) (N2pc-(ni_c_i^2+ni_c_j^2)/N2pc)/1.0 else 0.0
              s1_c <- s1_c + cmp_c$s1*nc_c; s3_c <- s3_c + cmp_c$s3*nc_c
            }
          }

          fst_r <- if (s3_r != 0) round(s1_r/s3_r, 6) else NA_real_
          fst_c <- if (s3_c != 0) round(s1_c/s3_c, 6) else NA_real_

          rows[[length(rows)+1]] <- data.frame(
            Locus    = loc, Pop1 = pi_n, Pop2 = pj_n,
            FST_brut = fst_r, FST_ENA = fst_c,
            Delta    = round(fst_c - fst_r, 6),
            N_i_brut = ni_raw_i, N_j_brut = ni_raw_j,
            N_i_ENA  = ni_c_i,   N_j_ENA  = ni_c_j,
            stringsAsFactors = FALSE
          )
        }
      }
      if (length(rows) == 0L) return(data.frame())
      do.call(rbind, rows)
    }

    # ══════════════════════════════════════════════════════════════════════
    #  REACTIVES PRINCIPAUX
    # ══════════════════════════════════════════════════════════════════════
    em_r <- reactive({
      db_ready()
      withProgress(message = "EM FreeNA — calcul fréquences allèles nuls...", value=0.1, {
        res <- fetch_em_results()
        setProgress(1); res
      })
    })

    fst_global_r <- eventReactive(input$run_fst_global, {
      req(length(em_r()) > 0)
      withProgress(message = "Calcul FST global (ENA)...", value=0.2, {
        res <- compute_fst_global(em_r())
        setProgress(1); res
      })
    })

    fst_pair_r <- eventReactive(input$run_fst_pair, {
      req(length(em_r()) > 0)
      withProgress(message = "Calcul FST pairwise (ENA)...", value=0.2, {
        res <- compute_fst_pairwise(em_r())
        setProgress(1); res
      })
    })

    dc_r <- eventReactive(input$run_dc, {
      req(length(em_r()) > 0)
      withProgress(message = "Calcul DCSE pairwise (INA)...", value=0.2, {
        res <- compute_dc_pairwise(em_r())
        setProgress(1); res
      })
    })

    fst_locus_r <- eventReactive(input$run_fst_locus, {
      req(length(em_r()) > 0)
      withProgress(message = "Calcul FST par locus × paire...", value=0.2, {
        res <- compute_fst_per_locus_pair(em_r(),
          sel_locus = safe_choice(input$fl_locus, "all"),
          sel_pop1  = safe_choice(input$fl_pop1,  "all"),
          sel_pop2  = safe_choice(input$fl_pop2,  "all"))
        setProgress(1); res
      })
    })

    # ── Value boxes ────────────────────────────────────────────────────────
    output$vb_loci <- renderUI({
      tryCatch(tags$span(length(markers_r())),
               error=function(e) tags$span("\u2014"))
    })
    output$vb_pops <- renderUI({
      tryCatch(tags$span(length(pops_r())),
               error=function(e) tags$span("\u2014"))
    })
    output$vb_fst_raw <- renderUI({
      tryCatch({
        r <- fst_global_r()
        v <- round(r$global_raw, 4)
        col <- if (!is.na(v) && v > 0.15) "#9d174d" else if (!is.na(v) && v > 0.05) "#854d0e" else "#166534"
        tags$span(style=paste0("color:",col,";"), if (is.na(v)) "—" else v)
      }, error=function(e) tags$span("\u2014"))
    })
    output$vb_fst_ena <- renderUI({
      tryCatch({
        r <- fst_global_r()
        v <- round(r$global_ena, 4)
        col <- if (!is.na(v) && v > 0.15) "#9d174d" else if (!is.na(v) && v > 0.05) "#854d0e" else "#166534"
        tags$span(style=paste0("color:",col,";"), if (is.na(v)) "—" else v)
      }, error=function(e) tags$span("\u2014"))
    })
    output$vb_dc_ina <- renderUI({
      tryCatch({
        r <- dc_r()
        vals <- r$matrix_ina[lower.tri(r$matrix_ina)]
        v <- round(mean(vals, na.rm=TRUE), 4)
        tags$span(if (is.nan(v) || is.na(v)) "—" else v)
      }, error=function(e) tags$span("\u2014"))
    })

    # ── Helper matrice HTML ────────────────────────────────────────────────
    render_matrix_html <- function(mat, fmt=6, color_thresh=c(0.05,0.15,0.25),
                                   colors=c("#f0fdf4","#dcfce7","#fefce8","#fef2f2")) {
      pops <- rownames(mat)
      n    <- length(pops)
      cells <- function(i,j) {
        v <- mat[i,j]
        if (i == j)
          return(sprintf('<td class="diag">—</td>'))
        if (i < j || is.na(v))
          return('<td style="color:#cbd5e1;">·</td>')
        bg <- colors[findInterval(v, color_thresh) + 1L]
        sprintf('<td style="background:%s;">%s</td>', bg, round(v, fmt))
      }
      thead <- paste0('<tr><th></th>',
        paste(sprintf('<th>%s</th>', pops[-n]), collapse=""), '</tr>')
      tbody <- paste(sapply(seq_len(n), function(i) {
        if (i == 1L) return("")
        paste0('<tr><td class="pop-label">', pops[i], '</td>',
               paste(sapply(seq_len(n), function(j) cells(i,j)), collapse=""),
               '</tr>')
      }), collapse="")
      HTML(sprintf('<div class="fe-matrix-wrap"><table class="fe-matrix"><thead>%s</thead><tbody>%s</tbody></table></div>',
                   thead, tbody))
    }

    # ── Tab FST global ─────────────────────────────────────────────────────
    output$dt_fst_global <- DT::renderDT({
      r <- fst_global_r()
      d <- r$per_locus
      shiny::validate(shiny::need(nrow(d) > 0, "Aucune donnée. Cliquez sur Calculer."))

      # Ligne résumé multilocus
      summary_row <- data.frame(
        Locus          = paste0("[Multilocus FST brut=", round(r$global_raw,6),
                                " | FST-ENA=", round(r$global_ena,6), "]"),
        FST_brut       = r$global_raw,
        FST_ENA        = r$global_ena,
        Delta_FST      = r$global_ena - r$global_raw,
        N_pops_eff_brut  = NA_integer_,
        N_pops_eff_ENA   = NA_integer_,
        stringsAsFactors = FALSE
      )
      disp <- rbind(summary_row, d)
      names(disp) <- c("Locus","FST brut","FST-ENA","ΔFST (ENA−brut)",
                       "N pops eff. (brut)","N pops eff. (ENA)")

      DT::datatable(disp, rownames=FALSE,
        options=list(pageLength=25, scrollX=TRUE, dom="lftip",
          columnDefs=list(list(className="dt-right", targets=1:5))),
        class="compact hover stripe") |>
        DT::formatRound("FST brut",        6) |>
        DT::formatRound("FST-ENA",         6) |>
        DT::formatRound("ΔFST (ENA−brut)", 6) |>
        DT::formatStyle("FST-ENA",
          backgroundColor=DT::styleInterval(c(0.05,0.15,0.25),
            c("#f0fdf4","#dcfce7","#fefce8","#fef2f2"))) |>
        DT::formatStyle("Locus", fontWeight="600", color="#0f172a")
    }, server=TRUE)

    # ── Tab FST pairwise ───────────────────────────────────────────────────
    output$ui_fst_pair_matrix <- renderUI({
      r <- fst_pair_r(); typ <- input$fst_pair_type
      shiny::validate(shiny::need(!is.null(r$matrix_raw), "Cliquez sur Calculer."))
      if (identical(typ, "both")) {
        tags$div(
          tags$strong("FST brut"),
          render_matrix_html(r$matrix_raw),
          tags$br(),
          tags$strong("FST-ENA"),
          render_matrix_html(r$matrix_ena)
        )
      } else if (identical(typ, "raw")) {
        render_matrix_html(r$matrix_raw)
      } else {
        render_matrix_html(r$matrix_ena)
      }
    })

    output$dt_fst_pair <- DT::renderDT({
      r <- fst_pair_r()
      d <- r$long
      shiny::validate(shiny::need(nrow(d) > 0, "Aucune donnée."))
      names(d) <- c("Pop 1","Pop 2","FST brut","FST-ENA","ΔFST (ENA−brut)")
      DT::datatable(d, rownames=FALSE,
        options=list(pageLength=20, scrollX=TRUE, dom="lftip",
          columnDefs=list(list(className="dt-right", targets=2:4))),
        class="compact hover stripe") |>
        DT::formatRound("FST brut",        6) |>
        DT::formatRound("FST-ENA",         6) |>
        DT::formatRound("ΔFST (ENA−brut)", 6) |>
        DT::formatStyle("FST-ENA",
          backgroundColor=DT::styleInterval(c(0.05,0.15,0.25),
            c("#f0fdf4","#dcfce7","#fefce8","#fef2f2")))
    }, server=TRUE)

    # ── Tab DCSE ───────────────────────────────────────────────────────────
    output$ui_dc_matrix <- renderUI({
      r <- dc_r(); typ <- input$dc_type
      shiny::validate(shiny::need(!is.null(r$matrix_raw), "Cliquez sur Calculer."))
      clr <- c("#eff6ff","#dbeafe","#fef9c3","#fef2f2")
      thr <- c(0.1, 0.25, 0.4)
      if (identical(typ, "both")) {
        tags$div(
          tags$strong("DCSE brute"),
          render_matrix_html(r$matrix_raw, color_thresh=thr, colors=clr),
          tags$br(),
          tags$strong("DCSE-INA"),
          render_matrix_html(r$matrix_ina, color_thresh=thr, colors=clr)
        )
      } else if (identical(typ, "raw")) {
        render_matrix_html(r$matrix_raw, color_thresh=thr, colors=clr)
      } else {
        render_matrix_html(r$matrix_ina, color_thresh=thr, colors=clr)
      }
    })

    output$dt_dc <- DT::renderDT({
      r <- dc_r(); d <- r$long
      shiny::validate(shiny::need(nrow(d) > 0, "Aucune donnée."))
      names(d) <- c("Pop 1","Pop 2","DCSE brute","DCSE-INA","ΔDCSE (INA−brut)")
      DT::datatable(d, rownames=FALSE,
        options=list(pageLength=20, scrollX=TRUE, dom="lftip",
          columnDefs=list(list(className="dt-right", targets=2:4))),
        class="compact hover stripe") |>
        DT::formatRound("DCSE brute",       6) |>
        DT::formatRound("DCSE-INA",         6) |>
        DT::formatRound("ΔDCSE (INA−brut)", 6)
    }, server=TRUE)

    # ── Tab FST par locus ──────────────────────────────────────────────────
    output$dt_fst_locus <- DT::renderDT({
      d <- fst_locus_r()
      shiny::validate(shiny::need(nrow(d) > 0, "Aucune donnée."))
      names(d) <- c("Locus","Pop 1","Pop 2","FST brut","FST-ENA",
                    "ΔFST","N_i brut","N_j brut","N_i ENA","N_j ENA")
      DT::datatable(d, rownames=FALSE,
        options=list(pageLength=25, scrollX=TRUE, dom="lftip",
          columnDefs=list(list(className="dt-right", targets=3:9))),
        class="compact hover stripe") |>
        DT::formatRound("FST brut", 6) |>
        DT::formatRound("FST-ENA",  6) |>
        DT::formatRound("ΔFST",     6) |>
        DT::formatStyle("FST-ENA",
          backgroundColor=DT::styleInterval(c(0.05,0.15,0.25),
            c("#f0fdf4","#dcfce7","#fefce8","#fef2f2"))) |>
        DT::formatStyle("Locus", fontWeight="600", color="#0f172a")
    }, server=TRUE)

    # ── Downloads ──────────────────────────────────────────────────────────
    dl_helper <- function(data_fn, filename_base, col_names=NULL) {
      list(
        csv = downloadHandler(
          filename = function() paste0(filename_base, "_", Sys.Date(), ".csv"),
          content  = function(file) {
            d <- data_fn(); if (is.null(d) || nrow(d)==0) return(invisible(NULL))
            if (!is.null(col_names)) names(d) <- col_names
            write.csv(d, file, row.names=FALSE)
          }
        ),
        txt = downloadHandler(
          filename = function() paste0(filename_base, "_", Sys.Date(), ".txt"),
          content  = function(file) {
            d <- data_fn(); if (is.null(d) || nrow(d)==0) return(invisible(NULL))
            if (!is.null(col_names)) names(d) <- col_names
            write.table(d, file, sep="\t", row.names=FALSE, quote=FALSE)
          }
        )
      )
    }

    # FST global
    dl_fg <- dl_helper(function() fst_global_r()$per_locus, "fst_global_ena",
      c("Locus","FST_brut","FST_ENA","Delta_FST","N_pops_eff_brut","N_pops_eff_ENA"))
    output$dl_fst_global_csv <- dl_fg$csv
    output$dl_fst_global_txt <- dl_fg$txt

    # FST pairwise matrice
    output$dl_fst_pair_csv <- downloadHandler(
      filename = function() paste0("fst_pairwise_ena_", Sys.Date(), ".csv"),
      content  = function(file) {
        r <- fst_pair_r(); mat <- r$matrix_ena
        d <- as.data.frame(round(mat, 6))
        d <- cbind(Population=rownames(d), d)
        write.csv(d, file, row.names=FALSE)
      }
    )
    output$dl_fst_pair_txt <- downloadHandler(
      filename = function() paste0("fst_pairwise_ena_", Sys.Date(), ".txt"),
      content  = function(file) {
        r <- fst_pair_r(); mat <- r$matrix_ena
        d <- as.data.frame(round(mat, 6))
        d <- cbind(Population=rownames(d), d)
        write.table(d, file, sep="\t", row.names=FALSE, quote=FALSE)
      }
    )

    dl_fp <- dl_helper(function() fst_pair_r()$long, "fst_pairwise_long_ena",
      c("Pop1","Pop2","FST_brut","FST_ENA","Delta_FST"))
    output$dl_fst_pair_long_csv <- dl_fp$csv
    output$dl_fst_pair_long_txt <- dl_fp$txt

    # DCSE matrice
    output$dl_dc_csv <- downloadHandler(
      filename = function() paste0("dcse_ina_", Sys.Date(), ".csv"),
      content  = function(file) {
        r <- dc_r(); mat <- r$matrix_ina
        d <- as.data.frame(round(mat, 6)); d <- cbind(Population=rownames(d), d)
        write.csv(d, file, row.names=FALSE)
      }
    )
    output$dl_dc_txt <- downloadHandler(
      filename = function() paste0("dcse_ina_", Sys.Date(), ".txt"),
      content  = function(file) {
        r <- dc_r(); mat <- r$matrix_ina
        d <- as.data.frame(round(mat, 6)); d <- cbind(Population=rownames(d), d)
        write.table(d, file, sep="\t", row.names=FALSE, quote=FALSE)
      }
    )

    dl_dc <- dl_helper(function() dc_r()$long, "dcse_pairwise_long_ina",
      c("Pop1","Pop2","DCSE_brut","DCSE_INA","Delta_DCSE"))
    output$dl_dc_long_csv <- dl_dc$csv
    output$dl_dc_long_txt <- dl_dc$txt

    dl_fl <- dl_helper(function() fst_locus_r(), "fst_per_locus_pair_ena",
      c("Locus","Pop1","Pop2","FST_brut","FST_ENA","Delta_FST",
        "N_i_brut","N_j_brut","N_i_ENA","N_j_ENA"))
    output$dl_fst_locus_csv <- dl_fl$csv
    output$dl_fst_locus_txt <- dl_fl$txt

  })
}
