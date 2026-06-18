# server_genetic_distances.R
# Pairwise Genetic Distances & Mantel Test — module server
#
# Distances computed:
#   FST      : Weir & Cockerham 1984 (standard, missing/null excluded)
#   FST-ENA  : FreeNA ENA-corrected (Chapuis & Estoup 2007) via EM on null allele
#   DCSE     : Cavalli-Sforza & Edwards 1967 chord distance (standard freqs)
#   DCSE-INA : chord distance on ENA-corrected frequencies
#
# 95 % CI : bootstrap over loci (FSTAT 2.9.4 convention)
#
# Mantel test: label-permutation on column-format data (rectangular-matrix safe)

# ============================================================
# SECTION 1 – File-local statistical helpers
# ============================================================

# ── 1.1 Genotype parsing ────────────────────────────────────────────────────

# Parse a vector of string genotypes ("a/b", "a-b", or single code → hom)
# Returns: list of character(2) for each valid individual, NULL entry for
# missing/null genotypes.
.gd_parse_vec <- function(g_vec, null_code = "999999",
                           miss_codes  = c("0", "")) {
  g <- as.character(g_vec)
  lapply(g, function(gg) {
    gg <- trimws(gg)
    if (is.na(gg) || gg %in% c("NA", "")) return(NULL)
    if (grepl("/", gg, fixed = TRUE)) al <- strsplit(gg, "/", fixed = TRUE)[[1L]]
    else if (grepl("-", gg, fixed = TRUE)) al <- strsplit(gg, "-", fixed = TRUE)[[1L]]
    else al <- c(gg, gg)                       # single-field → treat as homozygote
    al <- trimws(al)
    a1 <- al[1L]; a2 <- al[2L]
    if (a1 %in% c(miss_codes, "NA") && a2 %in% c(miss_codes, "NA")) return(NULL)
    if (a1 == null_code || a2 == null_code) return(NULL)  # exclude null carriers
    c(a1, a2)
  })
}

# From a list returned by .gd_parse_vec, return only the valid (non-NULL) entries
.gd_valid_genos <- function(parsed) Filter(Negate(is.null), parsed)

# ── 1.2 EM null allele estimation (faithful FreeNA; same algorithm as IBD module) ─

.gd_em_null <- function(g_vec, null_code = "999999", miss_code = "0",
                         tol = 1e-6, max_iter = 10000L) {
  g <- as.character(g_vec)
  g <- g[!is.na(g) & nzchar(trimws(g))]

  parse2 <- function(gg) {
    if (grepl("/", gg, fixed = TRUE)) strsplit(gg, "/", fixed = TRUE)[[1L]]
    else if (grepl("-", gg, fixed = TRUE)) strsplit(gg, "-", fixed = TRUE)[[1L]]
    else c(gg, gg)
  }

  n_nullhom <- 0L; allele_list <- character(0); genos_obs <- list()

  for (gg in g) {
    al <- parse2(trimws(gg))
    a1 <- trimws(al[1L]); a2 <- trimws(al[2L])
    if ((a1 %in% c("0", "", miss_code)) && (a2 %in% c("0", "", miss_code))) next
    if (a1 == null_code && a2 == null_code) { n_nullhom <- n_nullhom + 1L; next }
    if (a1 == null_code || a2 == null_code) next
    allele_list <- c(allele_list, a1, a2)
    genos_obs[[length(genos_obs) + 1L]] <- c(a1, a2)
  }

  N <- n_nullhom + length(genos_obs)
  empty <- list(r = 0.0, cq = setNames(numeric(0), character(0)),
                N = 0L, n_het = setNames(integer(0), character(0)),
                alleles_uniq = character(0))
  if (N == 0L) return(empty)
  if (length(genos_obs) == 0L)
    return(list(r = 1.0, cq = setNames(numeric(0), character(0)),
                N = N, n_het = setNames(integer(0), character(0)),
                alleles_uniq = character(0)))

  alleles_uniq <- sort(unique(allele_list)); A <- length(alleles_uniq)
  N_obs    <- length(genos_obs)
  al_table <- table(factor(allele_list, levels = alleles_uniq))
  genefreq <- as.numeric(al_table) / (2.0 * N_obs); names(genefreq) <- alleles_uniq

  n_hom <- setNames(integer(A), alleles_uniq)
  n_het <- setNames(integer(A), alleles_uniq)
  for (pair in genos_obs) {
    a1 <- pair[1L]; a2 <- pair[2L]
    if (a1 == a2) n_hom[a1] <- n_hom[a1] + 1L
    else { n_het[a1] <- n_het[a1] + 1L; n_het[a2] <- n_het[a2] + 1L }
  }

  rd  <- if (n_nullhom > 0L) sqrt(n_nullhom / N) else sqrt(1.0 / (N + 1L))
  cq  <- setNames(numeric(A), alleles_uniq)
  for (k in seq_len(A)) {
    a <- alleles_uniq[k]
    if (genefreq[k] <= 0) next
    ii <- n_hom[a]; jj <- n_het[a]
    cq[k] <- if (n_nullhom > 0L) 1 - sqrt((n_nullhom + N - ii - jj) / N)
              else                1 - sqrt((1 + N - ii - jj) / (N + 1L))
    cq[k] <- min(max(cq[k], 1e-10), 1 - 1e-10)
  }
  old_cq <- cq
  for (iter in seq_len(max_iter)) {
    rdi <- 0.0
    for (k in seq_len(A)) {
      a <- alleles_uniq[k]; if (genefreq[k] <= 0) next
      ii <- n_hom[a]; jj <- n_het[a]; cq_old <- cq[k]
      denom <- cq_old + 2 * rd
      if (denom > 0) {
        cq[k] <- ((cq_old + rd) / denom) * (ii / N) + jj / (2 * N)
        rdi    <- rdi + (rd / denom) * (ii / N)
      }
      cq[k] <- min(max(cq[k], 1e-10), 1 - 1e-10)
    }
    rd_new  <- min(max(rdi + n_nullhom / N, 1e-10), 1 - 1e-10)
    cq_chg  <- if (A > 0) max(abs(cq - old_cq), na.rm = TRUE) else 0
    old_cq  <- cq; rd <- rd_new
    if (abs(rd_new - rd) < tol && cq_chg < tol) break
  }
  list(r = rd, cq = cq, N = N, n_het = n_het, alleles_uniq = alleles_uniq)
}

# ── 1.3 WC84 per-locus a / b / c components ─────────────────────────────────

# Standard WC84 (two populations, one locus, null/missing already excluded)
# Returns list(a, b, c) summed over alleles, or NULL if not enough data.
.gd_wc84_std_locus <- function(p1_parsed, p2_parsed) {
  # p1_parsed, p2_parsed : output of .gd_valid_genos()
  N1 <- length(p1_parsed); N2 <- length(p2_parsed)
  if (N1 < 2L || N2 < 2L) return(NULL)

  K <- 2L; n_tot <- N1 + N2; n_bar <- n_tot / K
  n_c <- (n_tot - (N1^2 + N2^2) / n_tot) / (K - 1L)
  if (n_c <= 0 || n_bar <= 1) return(NULL)

  mat1 <- do.call(rbind, p1_parsed)   # N1 × 2
  mat2 <- do.call(rbind, p2_parsed)   # N2 × 2
  al1  <- c(mat1[, 1L], mat1[, 2L])
  al2  <- c(mat2[, 1L], mat2[, 2L])
  all_alleles <- sort(unique(c(al1, al2)))
  if (!length(all_alleles)) return(NULL)

  a_s <- 0; b_s <- 0; c_s <- 0

  for (allele in all_alleles) {
    p1    <- sum(al1 == allele) / (2 * N1)
    p2    <- sum(al2 == allele) / (2 * N2)
    p_bar <- (N1 * p1 + N2 * p2) / n_tot
    s2    <- (N1 * (p1 - p_bar)^2 + N2 * (p2 - p_bar)^2) / ((K - 1L) * n_bar)

    # Frequency of heterozygotes for this allele in each pop
    het1  <- mean((mat1[, 1L] == allele) != (mat1[, 2L] == allele))
    het2  <- mean((mat2[, 1L] == allele) != (mat2[, 2L] == allele))
    h_bar <- (N1 * het1 + N2 * het2) / n_tot

    tp  <- p_bar * (1 - p_bar)
    a_s <- a_s + (n_bar / n_c) *
             (s2 - (1 / (n_bar - 1)) * (tp - (K-1)/K * s2 - h_bar / 4))
    b_s <- b_s + (n_bar / (n_bar - 1)) *
             (tp - (K-1)/K * s2 - (2*n_bar - 1) / (4*n_bar) * h_bar)
    c_s <- c_s + h_bar / 2
  }
  list(a = a_s, b = b_s, c = c_s)
}

# ENA-corrected WC84 (uses EM output): same formula but with corrected cq freqs
# Returns list(a, b, c) or NULL.
.gd_wc84_ena_locus <- function(em1, em2) {
  K    <- 2L
  N_k  <- c(em1$N, em2$N)
  if (any(N_k < 2L)) return(NULL)
  n_tot <- sum(N_k); n_bar <- n_tot / K
  n_c   <- (n_tot - sum(N_k^2) / n_tot) / (K - 1L)
  if (n_c <= 0 || n_bar <= 1) return(NULL)

  all_alleles <- unique(c(em1$alleles_uniq, em2$alleles_uniq))
  if (!length(all_alleles)) return(NULL)

  a_s <- 0; b_s <- 0; c_s <- 0

  for (allele in all_alleles) {
    p1    <- if (allele %in% names(em1$cq)) em1$cq[[allele]] else 0.0
    p2    <- if (allele %in% names(em2$cq)) em2$cq[[allele]] else 0.0
    p_bar <- (N_k[1L] * p1 + N_k[2L] * p2) / n_tot
    s2    <- (N_k[1L] * (p1 - p_bar)^2 + N_k[2L] * (p2 - p_bar)^2) /
               ((K - 1L) * n_bar)
    h1    <- if (allele %in% names(em1$n_het)) em1$n_het[[allele]] else 0L
    h2    <- if (allele %in% names(em2$n_het)) em2$n_het[[allele]] else 0L
    h_bar <- (h1 + h2) / n_tot

    tp  <- p_bar * (1 - p_bar)
    a_s <- a_s + (n_bar / n_c) *
             (s2 - (1 / (n_bar - 1)) * (tp - (K-1)/K * s2 - h_bar / 4))
    b_s <- b_s + (n_bar / (n_bar - 1)) *
             (tp - (K-1)/K * s2 - (2*n_bar - 1) / (4*n_bar) * h_bar)
    c_s <- c_s + h_bar / 2
  }
  list(a = a_s, b = b_s, c = c_s)
}

# ── 1.4 DCSE per-locus u_l computation ──────────────────────────────────────
# u_l = sum_a sqrt(p1_a * p2_a)   (Helinger inner product on sqrt-frequency sphere)
# DCSE (multi-locus) = sqrt( 2 * mean_l(1 - u_l) )   ∈ [0, sqrt(2)]

.gd_dcse_u_std <- function(p1_parsed, p2_parsed) {
  N1 <- length(p1_parsed); N2 <- length(p2_parsed)
  if (N1 < 1L || N2 < 1L) return(NA_real_)
  al1 <- c(sapply(p1_parsed, `[[`, 1L), sapply(p1_parsed, `[[`, 2L))
  al2 <- c(sapply(p2_parsed, `[[`, 1L), sapply(p2_parsed, `[[`, 2L))
  all_alleles <- unique(c(al1, al2))
  sum(vapply(all_alleles, function(a)
    sqrt(sum(al1 == a) / (2*N1) * sum(al2 == a) / (2*N2)),
    numeric(1L)))
}

.gd_dcse_u_ena <- function(em1, em2) {
  all_alleles <- unique(c(em1$alleles_uniq, em2$alleles_uniq))
  if (!length(all_alleles)) return(NA_real_)
  sum(vapply(all_alleles, function(a) {
    p1 <- if (a %in% names(em1$cq)) em1$cq[[a]] else 0.0
    p2 <- if (a %in% names(em2$cq)) em2$cq[[a]] else 0.0
    sqrt(p1 * p2)
  }, numeric(1L)))
}

# ── 1.5 Main pairwise computation ────────────────────────────────────────────
#
# Returns a data.frame with one row per population pair:
#   Pop1, Pop2, Dist_km, n_loci_std, n_loci_ena,
#   FST, FST_ci_l, FST_ci_u,
#   FST_ENA, FST_ENA_ci_l, FST_ENA_ci_u,
#   DCSE, DCSE_ci_l, DCSE_ci_u,
#   DCSE_INA, DCSE_INA_ci_l, DCSE_INA_ci_u,
#   FR, FR_ci_l, FR_ci_u,             <- FST-ENA linearised for IBD
#   FR_ENA, FR_ENA_ci_l, FR_ENA_ci_u
#
.gd_compute_pairwise <- function(hap_df, pop_vector,
                                  null_code = "999999",
                                  n_boot    = 1000L,
                                  conf      = 0.95,
                                  use_gps   = FALSE,
                                  coords    = NULL) {
  pops   <- sort(unique(pop_vector))
  loci   <- colnames(hap_df)
  L      <- length(loci)
  np     <- length(pops)
  alpha  <- (1 - conf) / 2

  # Haversine distance (km)
  .hav_km <- function(la1, lo1, la2, lo2) {
    R    <- 6371.0
    dlat <- (la2 - la1) * pi / 180; dlon <- (lo2 - lo1) * pi / 180
    a    <- sin(dlat/2)^2 + cos(la1*pi/180)*cos(la2*pi/180)*sin(dlon/2)^2
    2 * R * asin(sqrt(a))
  }

  # Linearise FST → FR = FST/(1-FST)  (for export / IBD use)
  .lin <- function(x) { x <- pmin(pmax(x, 0), 0.9999); x / (1 - x) }

  # FST from a,b,c sum vectors
  .fst_from_abc <- function(a_v, b_v, c_v) {
    ok <- !is.na(a_v); if (!any(ok)) return(NA_real_)
    d  <- sum(a_v[ok]) + sum(b_v[ok]) + sum(c_v[ok])
    if (d == 0) return(NA_real_)
    sum(a_v[ok]) / d
  }

  # --- Pre-compute per-population parsed genotypes and EM caches --------
  parsed_cache <- vector("list", np); names(parsed_cache) <- pops
  em_cache     <- vector("list", np); names(em_cache)     <- pops

  for (pop in pops) {
    idx <- which(pop_vector == pop)
    parsed_cache[[pop]] <- lapply(loci, function(l) {
      .gd_valid_genos(.gd_parse_vec(hap_df[[l]][idx], null_code))
    })
    names(parsed_cache[[pop]]) <- loci
    em_cache[[pop]] <- lapply(loci, function(l)
      .gd_em_null(hap_df[[l]][idx], null_code))
    names(em_cache[[pop]]) <- loci
  }

  # --- Per-locus components for each pair --------------------------------
  result <- vector("list", np * (np - 1L) / 2L)
  k <- 1L

  for (i in seq_len(np - 1L)) {
    for (j in (i + 1L):np) {
      pi <- pops[i]; pj <- pops[j]

      # Per-locus: WC84 a/b/c (std + ENA) and DCSE u (std + ENA)
      abc_std <- matrix(NA_real_, L, 3L)
      abc_ena <- matrix(NA_real_, L, 3L)
      u_std   <- rep(NA_real_, L)
      u_ena   <- rep(NA_real_, L)

      for (li in seq_len(L)) {
        l <- loci[li]
        # Standard
        ab <- .gd_wc84_std_locus(parsed_cache[[pi]][[l]],
                                  parsed_cache[[pj]][[l]])
        if (!is.null(ab)) {
          abc_std[li, ] <- c(ab$a, ab$b, ab$c)
          u_std[li]     <- .gd_dcse_u_std(parsed_cache[[pi]][[l]],
                                           parsed_cache[[pj]][[l]])
        }
        # ENA-corrected
        ab_e <- .gd_wc84_ena_locus(em_cache[[pi]][[l]],
                                    em_cache[[pj]][[l]])
        if (!is.null(ab_e)) {
          abc_ena[li, ] <- c(ab_e$a, ab_e$b, ab_e$c)
          u_ena[li]     <- .gd_dcse_u_ena(em_cache[[pi]][[l]],
                                           em_cache[[pj]][[l]])
        }
      }

      ok_std <- !is.na(abc_std[, 1L]) & !is.na(u_std)
      ok_ena <- !is.na(abc_ena[, 1L]) & !is.na(u_ena)
      L_std  <- sum(ok_std); L_ena <- sum(ok_ena)

      # Point estimates
      fst_obs     <- .fst_from_abc(abc_std[, 1L], abc_std[, 2L], abc_std[, 3L])
      fst_ena_obs <- .fst_from_abc(abc_ena[, 1L], abc_ena[, 2L], abc_ena[, 3L])
      dcse_obs    <- if (L_std >= 1L) sqrt(2 * mean(1 - u_std[ok_std])) else NA_real_
      dcse_ena_obs<- if (L_ena >= 1L) sqrt(2 * mean(1 - u_ena[ok_ena])) else NA_real_

      # Locus bootstrap (FSTAT convention: resample loci with replacement)
      loci_std_idx <- which(ok_std); loci_ena_idx <- which(ok_ena)
      n_boot_eff   <- max(n_boot, 0L)

      b_fst <- b_fst_ena <- b_dcse <- b_dcse_ena <- rep(NA_real_, n_boot_eff)

      if (n_boot_eff > 0L && L_std >= 2L) {
        for (b in seq_len(n_boot_eff)) {
          bs <- sample(loci_std_idx, L_std, replace = TRUE)
          b_fst[b]  <- .fst_from_abc(abc_std[bs, 1L], abc_std[bs, 2L], abc_std[bs, 3L])
          b_dcse[b] <- sqrt(2 * mean(1 - u_std[bs]))
        }
      }
      if (n_boot_eff > 0L && L_ena >= 2L) {
        for (b in seq_len(n_boot_eff)) {
          be <- sample(loci_ena_idx, L_ena, replace = TRUE)
          b_fst_ena[b]  <- .fst_from_abc(abc_ena[be, 1L], abc_ena[be, 2L], abc_ena[be, 3L])
          b_dcse_ena[b] <- sqrt(2 * mean(1 - u_ena[be]))
        }
      }

      ci <- function(bv) {
        v <- bv[is.finite(bv)]
        if (length(v) < 2L) return(c(NA_real_, NA_real_))
        unname(quantile(v, c(alpha, 1 - alpha)))
      }
      ci_fst     <- ci(b_fst);     ci_fst_ena <- ci(b_fst_ena)
      ci_dcse    <- ci(b_dcse);    ci_dcse_ena<- ci(b_dcse_ena)

      # Geographic distance
      dist_km <- NA_real_
      if (use_gps && !is.null(coords)) {
        ri <- coords[coords$Population == pi, ]
        rj <- coords[coords$Population == pj, ]
        if (nrow(ri) >= 1L && nrow(rj) >= 1L)
          dist_km <- .hav_km(ri$Latitude[1L], ri$Longitude[1L],
                             rj$Latitude[1L], rj$Longitude[1L])
      }

      # Linearised FST values (for Rousset IBD / Mantel export)
      result[[k]] <- data.frame(
        Pop1         = pi,  Pop2           = pj,
        Dist_km      = dist_km,
        n_loci_std   = L_std, n_loci_ena   = L_ena,
        FST          = fst_obs,
        FST_ci_l     = ci_fst[1L],     FST_ci_u     = ci_fst[2L],
        FST_ENA      = fst_ena_obs,
        FST_ENA_ci_l = ci_fst_ena[1L], FST_ENA_ci_u = ci_fst_ena[2L],
        DCSE         = dcse_obs,
        DCSE_ci_l    = ci_dcse[1L],    DCSE_ci_u    = ci_dcse[2L],
        DCSE_INA     = dcse_ena_obs,
        DCSE_INA_ci_l= ci_dcse_ena[1L],DCSE_INA_ci_u= ci_dcse_ena[2L],
        FR           = .lin(fst_obs),
        FR_ci_l      = .lin(ci_fst[1L]),   FR_ci_u    = .lin(ci_fst[2L]),
        FR_ENA       = .lin(fst_ena_obs),
        FR_ENA_ci_l  = .lin(ci_fst_ena[1L]),FR_ENA_ci_u= .lin(ci_fst_ena[2L]),
        stringsAsFactors = FALSE
      )
      k <- k + 1L
    }
  }
  do.call(rbind, result)
}

# ── 1.6 Mantel test — label permutation, rectangular-matrix safe ─────────────
#
# df       : data.frame with columns pop1_col, pop2_col, x_col, y_col
# stat     : "r" = Pearson correlation, "b" = OLS regression slope (Rousset)
# Returns  : list(stat_obs, p_value, n_pairs, r2, slope, intercept, perm_stats)
#
.gd_mantel <- function(df, pop1_col, pop2_col, x_col, y_col,
                        n_perm = 9999L, stat = "r") {

  df <- df[is.finite(df[[x_col]]) & is.finite(df[[y_col]]), , drop = FALSE]
  n  <- nrow(df)
  if (n < 3L)
    return(list(stat_obs = NA_real_, p_value = NA_real_, n_pairs = n,
                r2 = NA_real_, slope = NA_real_, intercept = NA_real_,
                perm_stats = numeric(0L)))

  x   <- df[[x_col]]; y <- df[[y_col]]
  p1  <- as.character(df[[pop1_col]]); p2 <- as.character(df[[pop2_col]])
  all_pops <- unique(c(p1, p2))

  # Symmetric lookup: normalised key Pop_A__Pop_B (sorted lexicographically)
  make_key <- function(a, b) {
    ifelse(a <= b, paste(a, b, sep = "__"), paste(b, a, sep = "__"))
  }
  keys   <- make_key(p1, p2)
  lookup <- setNames(x, keys)   # if duplicate keys exist, last value wins

  # Observed statistic
  .stat_fn <- function(xv, yv) {
    ok <- is.finite(xv) & is.finite(yv)
    if (sum(ok) < 3L) return(NA_real_)
    if (stat == "r") suppressWarnings(cor(xv[ok], yv[ok]))
    else             unname(coef(lm(yv[ok] ~ xv[ok]))[2L])
  }
  stat_obs <- .stat_fn(x, y)

  # Permutation: randomly reassign population labels, then re-look-up x values
  perm_stats <- vapply(seq_len(n_perm), function(.b) {
    sigma    <- setNames(sample(all_pops), all_pops)   # random bijection
    perm_p1  <- sigma[p1]; perm_p2 <- sigma[p2]
    perm_key <- make_key(perm_p1, perm_p2)
    x_perm   <- lookup[perm_key]                       # NA when pair absent
    .stat_fn(x_perm, y)
  }, numeric(1L))

  perm_fin <- perm_stats[is.finite(perm_stats)]
  p_value  <- if (length(perm_fin) > 0L && is.finite(stat_obs))
                mean(perm_fin >= stat_obs) else NA_real_

  # Regression summary (always OLS for display)
  lm0  <- tryCatch(lm(y ~ x), error = function(e) NULL)
  slp  <- if (!is.null(lm0)) unname(coef(lm0)[2L])        else NA_real_
  icp  <- if (!is.null(lm0)) unname(coef(lm0)[1L])        else NA_real_
  r2   <- if (!is.null(lm0)) summary(lm0)$r.squared        else NA_real_

  list(stat_obs   = stat_obs, p_value  = p_value,  n_pairs   = n,
       r2         = r2,       slope    = slp,       intercept = icp,
       perm_stats = perm_fin)
}

# ============================================================
# SECTION 2 – Module server
# ============================================================

server_genetic_distances <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    `%||%` <- function(x, y)
      if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x

    # ── DB helpers ─────────────────────────────────────────────────────────
    db_tick    <- reactive({ rv$db_tick })
    con_r      <- reactive({ shiny::req(rv$con); rv$con })
    tbl_meta_r <- reactive({ rv$tbl_meta %||% "meta" })

    db_ready <- reactive({
      db_tick()
      con <- con_r()
      shiny::req(isTRUE(rv$db_ready))
      shiny::validate(
        shiny::need(DBI::dbExistsTable(con, tbl_meta_r()), "DuckDB meta table missing.")
      )
      TRUE
    })

    # ── Population GPS centroids (optional) ────────────────────────────────
    coords_r <- reactive({
      db_ready()
      con  <- con_r()
      cols <- DBI::dbGetQuery(con, sprintf(
        "SELECT column_name FROM information_schema.columns WHERE table_name='%s'",
        tbl_meta_r()))$column_name
      if (!all(c("Latitude", "Longitude") %in% cols)) return(NULL)
      df <- DBI::dbGetQuery(con, sprintf(
        "SELECT Population,
                AVG(CAST(Latitude  AS DOUBLE)) AS Latitude,
                AVG(CAST(Longitude AS DOUBLE)) AS Longitude
         FROM %s
         WHERE Population IS NOT NULL
           AND Latitude IS NOT NULL AND Longitude IS NOT NULL
         GROUP BY Population ORDER BY Population",
        sql_ident(con, tbl_meta_r())))
      if (nrow(df) == 0L) NULL else df
    })

    # ── Raw string genotypes from DuckDB ───────────────────────────────────
    raw_genos_r <- reactive({
      db_ready()
      con     <- con_r()
      tbl_raw <- rv$tbl_raw %||% "raw"
      shiny::validate(shiny::need(
        DBI::dbExistsTable(con, tbl_raw),
        "Raw genotype table not found. Please re-import the dataset."))

      ok_par <- tryCatch(DBI::dbExistsTable(con, "params"), error = function(e) FALSE)
      shiny::validate(shiny::need(ok_par, "params table not found."))

      marker_json <- tryCatch(
        DBI::dbGetQuery(con, "SELECT value FROM params WHERE key='marker_cols_raw'")$value[1L],
        error = function(e) NA_character_)
      marker_cols_raw <- if (!is.na(marker_json) && nzchar(marker_json))
        tryCatch(jsonlite::fromJSON(marker_json), error = function(e) character(0))
      else character(0)
      shiny::validate(shiny::need(length(marker_cols_raw) > 0L, "No marker_cols_raw in params."))

      geno_fmt <- tryCatch(
        DBI::dbGetQuery(con, "SELECT value FROM params WHERE key='genotype_format'")$value[1L],
        error = function(e) NA_character_)
      if (is.na(geno_fmt) || !nzchar(geno_fmt))
        geno_fmt <- if (any(grepl("(_1|\\.[0-9]+)$", marker_cols_raw))) "paired" else "string"

      keep     <- unique(marker_cols_raw)
      keep_sql <- paste(vapply(keep, function(x)
        as.character(DBI::dbQuoteIdentifier(con, x)), character(1L)), collapse = ", ")
      raw_df <- as.data.frame(
        DBI::dbGetQuery(con, sprintf(
          "SELECT rowid AS individual, %s FROM %s", keep_sql,
          as.character(DBI::dbQuoteIdentifier(con, tbl_raw)))),
        stringsAsFactors = FALSE)
      shiny::validate(shiny::need(nrow(raw_df) > 0L, "No rows in raw table."))

      meta_pop <- DBI::dbGetQuery(con, sprintf(
        "SELECT individual, Population FROM %s WHERE Population IS NOT NULL",
        sql_ident(con, tbl_meta_r())))
      raw_df$Population <- meta_pop$Population[match(raw_df$individual, meta_pop$individual)]

      pick_b <- function(locus, nms) {
        cands <- c(paste0(locus, "_1"), paste0(locus, "_2"),
                   paste0(locus, ".", 1:9), paste0(locus, "_", 1:9))
        hit <- cands[cands %in% nms]; if (length(hit)) hit[1L] else NA_character_
      }

      pop_vector <- as.character(raw_df$Population)
      if (identical(geno_fmt, "paired")) {
        nms   <- names(raw_df)
        loci  <- unique(sub("(_1|_2|\\.[0-9]+)$", "", marker_cols_raw))
        hap_df <- data.frame(row.names = seq_len(nrow(raw_df)))
        for (locus in loci) {
          b <- pick_b(locus, nms)
          if (!locus %in% nms || is.na(b) || !b %in% nms) next
          a_v <- as.character(raw_df[[locus]])
          b_v <- as.character(raw_df[[b]])
          a_v[is.na(a_v) | trimws(a_v) == ""] <- "0"
          b_v[is.na(b_v) | trimws(b_v) == ""] <- "0"
          already <- grepl("/", a_v, fixed = TRUE) | grepl("-", a_v, fixed = TRUE)
          hap_df[[locus]] <- ifelse(already, a_v, paste0(a_v, "/", b_v))
        }
      } else {
        hap_df <- as.data.frame(raw_df[, marker_cols_raw, drop = FALSE],
                                 stringsAsFactors = FALSE)
        for (j in seq_along(hap_df)) {
          x <- as.character(hap_df[[j]])
          x[is.na(x) | trimws(x) == ""] <- "0/0"
          hap_df[[j]] <- x
        }
      }
      shiny::validate(shiny::need(ncol(hap_df) > 0L,
        "No locus columns could be reconstructed."))
      list(hap_df = hap_df, pop_vector = pop_vector,
           n_loci = ncol(hap_df))
    })

    # ── Population selector ────────────────────────────────────────────────
    all_pops_r <- reactive({
      db_ready()
      con <- con_r()
      DBI::dbGetQuery(con, sprintf(
        "SELECT DISTINCT Population FROM %s WHERE Population IS NOT NULL ORDER BY Population",
        sql_ident(con, tbl_meta_r())))$Population
    })

    output$pop_selector_ui <- renderUI({
      pops <- tryCatch(all_pops_r(), error = function(e) character(0))
      selectizeInput(session$ns("selected_pops"), NULL,
                     choices  = pops, selected = pops,
                     multiple = TRUE,
                     options  = list(placeholder = "All populations",
                                     plugins     = list("remove_button")))
    })

    # ── Main computation — triggered by Run ────────────────────────────────
    gd_results_r <- eventReactive(input$run_gd, {
      shiny::req(db_ready())
      rg     <- raw_genos_r()
      coords <- if (isTRUE(input$use_gps)) tryCatch(coords_r(), error=function(e) NULL) else NULL

      hap_df     <- rg$hap_df
      pop_vector <- rg$pop_vector

      # Filter to selected populations
      sel_pops <- input$selected_pops
      if (!is.null(sel_pops) && length(sel_pops) > 0L) {
        keep <- pop_vector %in% sel_pops
        hap_df     <- hap_df[keep, , drop = FALSE]
        pop_vector <- pop_vector[keep]
      }
      shiny::validate(shiny::need(
        length(unique(pop_vector)) >= 2L,
        "At least 2 populations with genotype data are required."))

      withProgress(
        message = "Computing pairwise genetic distances\u2026",
        detail  = "Bootstrap over loci — this may take a few minutes.",
        value   = 0.1, {
          pw <- .gd_compute_pairwise(
            hap_df      = hap_df,
            pop_vector  = pop_vector,
            null_code   = trimws(input$null_code),
            n_boot      = as.integer(input$n_boot_loci),
            conf        = input$conf_level / 100,
            use_gps     = isTRUE(input$use_gps) && !is.null(coords),
            coords      = coords
          )
          setProgress(1.0)
        })

      shiny::validate(shiny::need(!is.null(pw) && nrow(pw) > 0L,
        "No valid pairs could be computed. Check genotype format and null allele code."))
      pw
    })

    # ── Summary value boxes ────────────────────────────────────────────────
    output$box_npops <- renderValueBox({
      pw <- gd_results_r()
      n  <- length(unique(c(pw$Pop1, pw$Pop2)))
      valueBox(n, HTML("Populations"), icon = icon("users"), color = "purple")
    })
    output$box_npairs <- renderValueBox({
      valueBox(nrow(gd_results_r()), HTML("Pairs"),
               icon = icon("project-diagram"), color = "blue")
    })
    output$box_nloci <- renderValueBox({
      pw <- gd_results_r()
      valueBox(round(mean(pw$n_loci_std, na.rm = TRUE), 1),
               HTML("Avg valid loci<br>(standard)"),
               icon = icon("dna"), color = "teal")
    })
    output$box_nboot <- renderValueBox({
      valueBox(input$n_boot_loci, HTML("Bootstrap<br>replicates (loci)"),
               icon = icon("sync"), color = "green")
    })

    # ── Pairwise distance table ────────────────────────────────────────────
    output$gd_table <- DT::renderDT({
      pw  <- gd_results_r()
      r6  <- function(x) round(x, 6)
      r2  <- function(x) round(x, 2)
      display_cols <- c(
        "Pop1", "Pop2", "Dist_km",
        "n_loci_std",
        "FST",     "FST_ci_l",     "FST_ci_u",
        "FST_ENA", "FST_ENA_ci_l", "FST_ENA_ci_u",
        "DCSE",    "DCSE_ci_l",    "DCSE_ci_u",
        "DCSE_INA","DCSE_INA_ci_l","DCSE_INA_ci_u",
        "FR",      "FR_ci_l",      "FR_ci_u",
        "FR_ENA",  "FR_ENA_ci_l",  "FR_ENA_ci_u"
      )
      df <- pw[, intersect(display_cols, colnames(pw)), drop = FALSE]
      num_cols <- setdiff(colnames(df), c("Pop1","Pop2"))
      df[num_cols] <- lapply(df[num_cols], function(x)
        ifelse(colnames(df[num_cols]) %in% c("Dist_km"), r2(x), r6(x)))

      DT::datatable(
        df, rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 20,
                       dom = "lrtip", autoWidth = FALSE),
        class    = "compact stripe hover",
        colnames = c("Pop\u00a01","Pop\u00a02","Dist\u00a0(km)",
                     "L",
                     "FST","FST\u2212l","FST\u2212u",
                     "FST-ENA","ENA\u2212l","ENA\u2212u",
                     "DCSE","DCSE\u2212l","DCSE\u2212u",
                     "DCSE-INA","INA\u2212l","INA\u2212u",
                     "FR","FR\u2212l","FR\u2212u",
                     "FR-ENA","FR.ENA\u2212l","FR.ENA\u2212u")
      ) %>%
        DT::formatStyle("FST",
          backgroundColor = DT::styleInterval(
            c(0.05, 0.15, 0.25),
            c("#d4edda","#fff3cd","#f8d7da","#c3002f22"))) %>%
        DT::formatStyle("FST_ENA",
          backgroundColor = DT::styleInterval(
            c(0.05, 0.15, 0.25),
            c("#d4edda","#fff3cd","#f8d7da","#c3002f22")))
    })

    # ── Reactive: computed table exposed for Mantel (also via button) ──────
    mantel_computed_flag <- reactiveVal(FALSE)
    observeEvent(input$send_to_mantel, {
      mantel_computed_flag(TRUE)
      updateTabsetPanel(session, "main_tabs", selected = "tab_mantel")
      updateRadioButtons(session, "mantel_source", selected = "computed")
    })

    # ── Mantel: load and parse data ────────────────────────────────────────
    mantel_df_r <- reactive({
      src <- input$mantel_source
      if (src == "computed") {
        shiny::req(gd_results_r())
        gd_results_r()
      } else {
        shiny::req(input$mantel_file)
        sep <- input$mantel_sep; hdr <- input$mantel_header
        df  <- tryCatch(
          read.table(input$mantel_file$datapath, header = hdr, sep = sep,
                     stringsAsFactors = FALSE, check.names = FALSE,
                     fill = TRUE, quote = "\""),
          error = function(e) NULL)
        shiny::validate(shiny::need(!is.null(df) && nrow(df) >= 3L,
          "Could not parse the uploaded file. Check separator and header settings."))
        df
      }
    })

    # ── Dynamic column selectors for Mantel ────────────────────────────────
    mantel_cols_r <- reactive({
      df <- mantel_df_r()
      if (is.null(df)) character(0) else colnames(df)
    })

    # Guess likely pop / distance columns from names
    .guess_col <- function(cols, patterns, fallback = cols[1]) {
      for (pat in patterns) {
        hit <- grep(pat, cols, value = TRUE, ignore.case = TRUE)
        if (length(hit)) return(hit[1L])
      }
      fallback
    }

    output$mantel_pop1_ui <- renderUI({
      cols <- mantel_cols_r()
      def  <- .guess_col(cols, c("^Pop1$","Pop.?1","^pop1$","From"), cols[1])
      selectInput(session$ns("mantel_pop1"), "Column \u2014 Population 1:",
                  choices = cols, selected = def)
    })
    output$mantel_pop2_ui <- renderUI({
      cols <- mantel_cols_r()
      def  <- .guess_col(cols, c("^Pop2$","Pop.?2","^pop2$","To"), cols[min(2L,length(cols))])
      selectInput(session$ns("mantel_pop2"), "Column \u2014 Population 2:",
                  choices = cols, selected = def)
    })
    output$mantel_x_ui <- renderUI({
      cols <- mantel_cols_r()
      def  <- .guess_col(cols, c("Dist_km","Dgeo","ln.Dgeo","dist","geo"),
                         cols[min(3L, length(cols))])
      selectInput(session$ns("mantel_x"), "X column (predictor):",
                  choices = cols, selected = def)
    })
    output$mantel_y_ui <- renderUI({
      cols <- mantel_cols_r()
      def  <- .guess_col(cols, c("FST_ENA","^FR","FST","DCSE","genetic"),
                         cols[min(4L, length(cols))])
      selectInput(session$ns("mantel_y"), "Y column (response / genetic distance):",
                  choices = cols, selected = def)
    })

    # ── Data preview table ─────────────────────────────────────────────────
    output$mantel_preview_table <- DT::renderDT({
      df <- mantel_df_r()
      shiny::req(!is.null(df))
      DT::datatable(df, rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 10, dom = "lrtip"),
        class   = "compact stripe hover")
    })

    # ── Mantel run ─────────────────────────────────────────────────────────
    mantel_result_r <- eventReactive(input$run_mantel, {
      shiny::req(mantel_df_r(), input$mantel_pop1, input$mantel_pop2,
                 input$mantel_x, input$mantel_y)
      df     <- mantel_df_r()
      pop1c  <- input$mantel_pop1; pop2c <- input$mantel_pop2
      xcol   <- input$mantel_x;   ycol  <- input$mantel_y
      n_perm <- as.integer(input$n_perm_mantel)
      stat   <- input$mantel_stat

      shiny::validate(
        shiny::need(all(c(pop1c, pop2c, xcol, ycol) %in% colnames(df)),
          "Selected columns not found in the data. Please check column assignments."),
        shiny::need(pop1c != pop2c, "Pop1 and Pop2 must be different columns."),
        shiny::need(xcol  != ycol,  "X and Y must be different columns.")
      )

      # Coerce x / y to numeric
      df[[xcol]] <- suppressWarnings(as.numeric(df[[xcol]]))
      df[[ycol]] <- suppressWarnings(as.numeric(df[[ycol]]))

      withProgress(message = "Running Mantel test\u2026",
                   detail  = sprintf("%d permutations\u2026", n_perm),
                   value   = 0.1, {
        res <- .gd_mantel(df, pop1c, pop2c, xcol, ycol, n_perm, stat)
        setProgress(1.0)
      })

      # Store data subset for scatter
      df_ok <- df[is.finite(df[[xcol]]) & is.finite(df[[ycol]]), , drop=FALSE]
      res$data_plot  <- df_ok
      res$xcol_label <- xcol
      res$ycol_label <- ycol
      res$stat_label <- if (stat == "r") "Pearson r" else "Slope b"
      res
    })

    # ── Mantel value boxes ─────────────────────────────────────────────────
    output$box_m_stat <- renderValueBox({
      r <- mantel_result_r()
      valueBox(round(r$stat_obs, 4), HTML(paste0(r$stat_label, "<br>(observed)")),
               icon = icon("chart-line"), color = "purple")
    })
    output$box_m_pval <- renderValueBox({
      r   <- mantel_result_r()
      pv  <- r$p_value
      col <- if (!is.na(pv) && pv < 0.05) "green" else if (!is.na(pv) && pv < 0.1) "yellow" else "red"
      valueBox(
        if (is.na(pv)) "NA" else formatC(pv, format = "f", digits = 4),
        HTML("p-value<br>(one-sided)"),
        icon = icon("check-circle"), color = col)
    })
    output$box_m_n <- renderValueBox({
      valueBox(mantel_result_r()$n_pairs, HTML("Pairs used"),
               icon = icon("project-diagram"), color = "blue")
    })
    output$box_m_r2 <- renderValueBox({
      r2 <- mantel_result_r()$r2
      valueBox(if (is.na(r2)) "NA" else round(r2, 4),
               HTML("R\u00b2<br>(OLS regression)"),
               icon = icon("percentage"), color = "teal")
    })

    # ── Mantel permutation histogram ───────────────────────────────────────
    output$mantel_hist <- plotly::renderPlotly({
      r <- mantel_result_r()
      pv <- r$perm_stats
      shiny::req(length(pv) > 0L)

      plotly::plot_ly() %>%
        plotly::add_histogram(
          x    = pv, nbinsx = 60,
          marker = list(color = "rgba(91,78,167,0.6)",
                        line  = list(color = "rgba(91,78,167,1)", width = 0.4)),
          name = "Permuted"
        ) %>%
        plotly::add_vline(
          x = r$stat_obs,
          line = list(color = "#B40F20", width = 2, dash = "dash")
        ) %>%
        plotly::layout(
          xaxis  = list(title = r$stat_label),
          yaxis  = list(title = "Count"),
          title  = list(text  = sprintf("Permutation distribution (n=%d)", length(pv)),
                        font  = list(size = 11)),
          margin = list(t = 40, b = 40),
          showlegend = FALSE,
          annotations = list(list(
            x = r$stat_obs, y = 0.95, xref = "x", yref = "paper",
            text = sprintf("obs. %s=%.4f<br>p=%.4f",
                           r$stat_label, r$stat_obs, r$p_value %||% NA_real_),
            showarrow = TRUE, arrowhead = 2,
            font = list(size = 10, color = "#B40F20"),
            bgcolor = "rgba(255,255,255,0.8)"
          ))
        )
    })

    # ── Mantel scatter plot ────────────────────────────────────────────────
    output$mantel_scatter <- plotly::renderPlotly({
      r   <- mantel_result_r()
      df  <- r$data_plot
      shiny::req(!is.null(df) && nrow(df) > 0L)

      x_v <- df[[r$xcol_label]]; y_v <- df[[r$ycol_label]]
      x_s <- seq(min(x_v, na.rm=TRUE), max(x_v, na.rm=TRUE), length.out = 100)
      y_s <- r$intercept + r$slope * x_s

      plotly::plot_ly() %>%
        plotly::add_markers(
          x = x_v, y = y_v,
          marker = list(color = "#5B4EA7", size = 6, opacity = 0.75),
          hoverinfo = "x+y",
          name = "Pairs"
        ) %>%
        plotly::add_lines(
          x = x_s, y = y_s,
          line = list(color = "#B40F20", width = 1.5),
          name = sprintf("OLS: b=%.4f, R\u00b2=%.4f", r$slope, r$r2 %||% NA_real_)
        ) %>%
        plotly::layout(
          xaxis  = list(title = r$xcol_label),
          yaxis  = list(title = r$ycol_label),
          title  = list(text  = sprintf("r=%.4f  p=%.4f",
                                        r$stat_obs, r$p_value %||% NA_real_),
                        font  = list(size = 11)),
          legend = list(x = 0.02, y = 0.98,
                        bgcolor = "rgba(255,255,255,0.8)"),
          margin = list(t = 40, b = 40),
          font   = list(family = "Helvetica Neue, Segoe UI, Arial")
        )
    })

    # ── Downloads — pairwise distance table ────────────────────────────────
    output$dl_gd_csv <- downloadHandler(
      filename = function() paste0("pairwise_genetic_distances_", Sys.Date(), ".csv"),
      content  = function(file) {
        pw <- gd_results_r()
        write.csv(pw, file, row.names = FALSE)
      }
    )
    output$dl_gd_txt <- downloadHandler(
      filename = function() paste0("pairwise_genetic_distances_", Sys.Date(), ".txt"),
      content  = function(file) {
        pw <- gd_results_r()
        write.table(pw, file, sep = "\t", row.names = FALSE, quote = FALSE)
      }
    )

    # ── Downloads — Mantel data ────────────────────────────────────────────
    output$dl_mantel_data <- downloadHandler(
      filename = function() paste0("mantel_input_data_", Sys.Date(), ".txt"),
      content  = function(file) {
        df <- mantel_df_r()
        write.table(df, file, sep = "\t", row.names = FALSE, quote = FALSE)
      }
    )

  })
}