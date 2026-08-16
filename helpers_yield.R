
# Periodenbasierte Street-Yield mit Accrued nach ACT/ACT (ICMA)

library(lubridate)

# ---- Helfer ------------------------------------------------------------------

# Kupontermine rückwärts ab Maturity; nur Termine STRICT > settle (keine t=0-CF!)
coupon_boundaries <- function(settle, maturity, freq = 1L) {
  settle <- as.Date(settle); maturity <- as.Date(maturity)
  by <- paste0(12L %/% as.integer(freq), " months")
  seqs <- rev(seq(maturity, by = paste0("-", by), length.out = 240L))
  seqs[seqs > settle]
}

# Accrued gemäß ICMA: Anteil der aktuellen Periode (tatsächliche Tage)
accrued_icma <- function(settle, last_cp, next_cp, coupon_rate, face = 100, freq = 1L) {
  settle <- as.Date(settle); last_cp <- as.Date(last_cp); next_cp <- as.Date(next_cp)
  days_acc <- as.numeric(settle - last_cp)
  days_per <- as.numeric(next_cp - last_cp)
  if (days_acc < 0 || days_per <= 0) return(list(ai = 0, accr_frac = 0))
  coup_amt <- face * coupon_rate / freq
  ai <- coup_amt * (days_acc / days_per)
  list(ai = ai, accr_frac = days_acc / days_per)
}

# ---- Preis <- YTM (ICMA; Rückgabe PRO 100) -----------------------------------
price_from_ytm <- function(settle, maturity, coupon_rate, ytm,
                           freq = 1L, face = 100,
                           conv = "act/act-icma",
                           calendar = "TARGET", bdc = "Following") {
  
  settle <- as.Date(settle); maturity <- as.Date(maturity); freq <- as.integer(freq)
  if (maturity <= settle) return(list(clean = NA_real_, dirty = NA_real_, ai = NA_real_))
  
  cps <- coupon_boundaries(settle, maturity, freq)
  if (!length(cps)) return(list(clean = NA_real_, dirty = NA_real_, ai = NA_real_))
  
  next_cp <- min(cps)
  # Emission korrekt: wenn kein Termin < settle, dann last_cp = settle (AI=0, accr_frac=0)
  last_candidates <- cps[cps < settle]
  last_cp <- if (length(last_candidates)) max(last_candidates) else settle
  
  acc <- accrued_icma(settle, last_cp, next_cp, coupon_rate, face, freq)
  ai_abs    <- acc$ai
  accr_frac <- acc$accr_frac
  
  coup_amt <- face * coupon_rate / freq
  dfp <- 1 + ytm / freq
  N <- length(cps)
  k <- seq_len(N)
  
  pv_coupons_abs    <- sum(coup_amt / (dfp)^(k - accr_frac))
  pv_redemption_abs <- face     / (dfp)^(N - accr_frac)
  dirty_abs <- pv_coupons_abs + pv_redemption_abs
  clean_abs <- dirty_abs - ai_abs
  
  # Ausgabe pro 100 (Nominal darf die Rendite nicht beeinflussen)
  scale <- face / 100
  list(
    clean = as.numeric(clean_abs / scale),
    dirty = as.numeric(dirty_abs / scale),
    ai    = as.numeric(ai_abs    / scale)
  )
}

# ---- YTM <- Preis (ICMA; Preis PRO 100 erwartet) -----------------------------
ytm_from_price <- function(settle, maturity, coupon_rate, price_input, is_clean = TRUE,
                           freq = 1L, face = 100,
                           conv = "act/act-icma",
                           calendar = "TARGET", bdc = "Following",
                           tol = 1e-10, maxit = 200) {
  
  settle <- as.Date(settle); maturity <- as.Date(maturity); freq <- as.integer(freq)
  if (maturity <= settle) return(NA_real_)
  
  cps <- coupon_boundaries(settle, maturity, freq)
  if (!length(cps)) return(NA_real_)
  
  next_cp <- min(cps)
  last_candidates <- cps[cps < settle]
  last_cp <- if (length(last_candidates)) max(last_candidates) else settle
  
  acc <- accrued_icma(settle, last_cp, next_cp, coupon_rate, face, freq)
  ai_abs    <- acc$ai
  accr_frac <- acc$accr_frac
  
  # Zielpreis in ABSOLUTEN Einheiten (Preis pro 100 -> * face/100)
  scale <- face / 100
  target_dirty_abs <- if (is_clean) price_input * scale + ai_abs else price_input * scale
  
  coup_amt <- face * coupon_rate / freq
  N <- length(cps)
  k <- seq_len(N)
  
  f <- function(y) {
    dfp <- 1 + y / freq
    pv_coupons_abs    <- sum(coup_amt / (dfp)^(k - accr_frac))
    pv_redemption_abs <- face     / (dfp)^(N - accr_frac)
    (pv_coupons_abs + pv_redemption_abs) - target_dirty_abs
  }
  
  lower <- -0.99; upper <- 2.0
  for (i in 1:8) {
    if (f(lower) * f(upper) < 0) break
    lower <- lower - 0.5; upper <- upper + 0.5
  }
  uniroot(f, c(lower, upper), tol = tol, maxiter = maxit)$root
}

# ---- DV01 (per 100 face) -----------------------------------------------------
dv01 <- function(settle, maturity, coupon_rate, ytm,
                 freq = 1L, face = 100,
                 conv = "act/act-icma",
                 calendar = "TARGET", bdc = "Following",
                 bp = 1e-4) {
  p_up   <- price_from_ytm(settle, maturity, coupon_rate, ytm + bp, freq, face, conv, calendar, bdc)$clean
  p_down <- price_from_ytm(settle, maturity, coupon_rate, ytm - bp, freq, face, conv, calendar, bdc)$clean
  (p_down - p_up) / 2
}

# ---- FRN Discount Margin (approx) -------------------------------------------
discount_margin_approx <- function(price_clean, par = 100, wal_years) {
  if (is.na(wal_years) || wal_years <= 0) return(NA_real_)
  ((par - price_clean) / wal_years) * 10000
}
