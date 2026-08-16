# ---------- module_yield.R ----------
library(ggplot2)

yield_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "neon-title", "Rendite & Sensitivitäten"),
    div(class = "glass-box",
        # --- Eingaben ---
        fluidRow(
          column(3,
                 dateInput(ns("settle"), "Valuta", value = Sys.Date()),
                 dateInput(ns("maturity"), "Fälligkeit"),
                 selectInput(ns("freq"), "Frequenz",
                             c("jährlich" = 1, "halbjährlich" = 2, "quartal" = 4)),
                 selectInput(ns("dcc"), "Day Count",
                             c("ACT/365", "ACT/360", "30E/360", "ICMA ACT/ACT"))
          ),
          column(3,
                 numericInput(ns("coupon"), "Kupon p.a. (%)", value = 3.0, step = 0.01),
                 numericInput(ns("price"),  "Preis (clean)", value = 99.50, step = 0.01),
                 checkboxInput(ns("is_clean"), "Eingabe ist Clean Price", TRUE),
                 numericInput(ns("face"), "Nominal (pro Einheit)", value = 1000000, step = 1),
                 numericInput(ns("position_nominal"), "Positionsgröße (Nominal gesamt)", value = NA, step = 1000)
          ),
          column(3,
                 actionButton(ns("to_ytm"),  "→ YTM berechnen"),
                 actionButton(ns("to_price"), "→ Preis aus YTM"),
                 numericInput(ns("ytm"), "YTM (%)", value = NA, step = 0.01)
          ),
          column(3,
                 checkboxInput(ns("is_frn"), "FRN/Float (DM-Approx)", FALSE),
                 numericInput(ns("wal"), "WAL (Jahre, FRN)", value = NA, step = 0.01)
          )
        ),
        
        hr(),
        
        # --- Ausgaben ---
        fluidRow(
          column(5,
                 verbatimTextOutput(ns("summary_text")),
                 tags$hr(),
                 tags$strong("Daycount-Berechnung"),
                 verbatimTextOutput(ns("daycount_text"))
          ),
          column(7,
                 plotOutput(ns("price_yield_curve"), height = 280)
          )
        )
    )
  )
}

yield_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- Mapping: UI-Text -> Helper-Token ---
    dcc_token <- function(x) {
      if (identical(x, "ICMA ACT/ACT")) "act_act_icma" else x
    }
    
    # --- Kupon-Grenzen einer Periode (aus Fälligkeit rückwärts) ---
    coupon_bounds <- function(settle, maturity, freq) {
      step_months <- 12L / as.integer(freq)
      grid <- seq(as.Date(maturity), by = paste0("-", step_months, " months"), length.out = 600)
      grid <- sort(grid)
      prev_cpn <- max(grid[grid <= as.Date(settle)])
      next_cpn <- min(grid[grid >  as.Date(settle)])
      list(prev_coupon = prev_cpn, next_coupon = next_cpn)
    }
    
    
    # --- Daycount-/Stückzinsen-Detail ---
    daycount_breakdown <- function(settle, maturity, coupon, freq, face, conv, position_nominal = NA_real_) {
      cb <- coupon_bounds(settle, maturity, freq)
      prev_cpn <- cb$prev_coupon
      next_cpn <- cb$next_coupon
      
      # Sicherheit, falls Termine nicht gefunden
      if (is.infinite(prev_cpn) || is.infinite(next_cpn) || is.na(prev_cpn) || is.na(next_cpn)) {
        return(list(prev_coupon = NA, next_coupon = NA, days_accrued = NA, days_period = NA,
                    af = NA, ai_per_100 = NA, ai_total = NA))
      }
      
      days_period <- as.integer(next_cpn - prev_cpn)
      
      if (as.Date(settle) <= prev_cpn) {
        days_accrued <- 0L
      } else if (as.Date(settle) >= next_cpn) {
        days_accrued <- days_period
      } else {
        days_accrued <- as.integer(as.Date(settle) - prev_cpn)
        # ICMA: Settlement inklusiv -> +1 Tag (z. B. 01.08->31.08 = 31)
        if (identical(conv, "act_act_icma")) days_accrued <- days_accrued + 1L
      }
      
      af <- switch(conv,
                   "act_act_icma" = days_accrued / days_period,
                   "ACT/365"      = days_accrued / 365,
                   "ACT/360"      = days_accrued / 360,
                   "30E/360"      = {
                     d1 <- as.POSIXlt(prev_cpn); d2 <- as.POSIXlt(as.Date(settle))
                     d1d <- min(d1$mday, 30); d2d <- min(d2$mday, 30)
                     dc  <- 360*(d2$year - d1$year) + 30*(d2$mon - d1$mon) + (d2d - d1d)
                     dc/360
                   },
                   days_accrued / days_period)
      
      coup_per_period <- (coupon/100) / as.integer(freq)
      ai_per_100      <- 100 * coup_per_period * af
      units           <- if (!is.na(face) && face != 0) (position_nominal / face) else NA_real_
      ai_total        <- if (!is.na(units) && is.finite(units)) ai_per_100 * units else NA_real_
      
      list(prev_coupon = prev_cpn, next_coupon = next_cpn,
           days_accrued = days_accrued, days_period = days_period,
           af = af, ai_per_100 = ai_per_100, ai_total = ai_total)
    }
    
    # --- Buttons ---
    observeEvent(input$to_ytm, {
      req(input$settle, input$maturity, input$coupon, input$price)
      y <- tryCatch({
        ytm_from_price(
          input$settle, input$maturity, input$coupon/100, input$price,
          is_clean = input$is_clean, freq = as.integer(input$freq),
          face = input$face, conv = dcc_token(input$dcc)
        )
      }, error = function(e) NA_real_)
      updateNumericInput(session, "ytm", value = ifelse(is.na(y), NA, 100*y))
    })
    
    observeEvent(input$to_price, {
      req(input$settle, input$maturity, input$coupon, input$ytm)
      pr <- price_from_ytm(
        input$settle, input$maturity, input$coupon/100, input$ytm/100,
        freq = as.integer(input$freq), face = input$face, conv = dcc_token(input$dcc)
      )
      updateNumericInput(session, "price", value = round(pr$clean, 4))
    })
    
    # --- Summary-Box ---
    output$summary_text <- renderText({
      req(input$settle, input$maturity, input$coupon, input$price)
      y <- suppressWarnings(
        ytm_from_price(
          input$settle, input$maturity, input$coupon/100, input$price,
          is_clean = input$is_clean, freq = as.integer(input$freq),
          face = input$face, conv = dcc_token(input$dcc)
        )
      )
      if (is.na(y)) return("YTM: n/a")
      
      pr <- price_from_ytm(
        input$settle, input$maturity, input$coupon/100, y,
        freq = as.integer(input$freq), face = input$face, conv = dcc_token(input$dcc)
      )
      d01 <- dv01(
        input$settle, input$maturity, input$coupon/100, y,
        freq = as.integer(input$freq), face = input$face, conv = dcc_token(input$dcc)
      )
      
      out <- sprintf("YTM: %.3f %%\nAccrued: %.4f\nDirty: %.4f\nDV01 (per 100): %.4f",
                     100*y, pr$ai, pr$dirty, d01)
      
      if (isTRUE(input$is_frn) && !is.na(input$wal)) {
        dm <- discount_margin_approx(price_clean = pr$clean, par = input$face, wal_years = input$wal)
        out <- paste0(out, sprintf("\nFRN Discount-Margin (approx): %.0f bp", dm))
      }
      out
    })
    
    # --- Daycount-Details ---
    output$daycount_text <- renderText({
      req(input$settle, input$maturity, input$coupon, input$freq, input$face, input$dcc)
      dd <- daycount_breakdown(
        settle = input$settle,
        maturity = input$maturity,
        coupon = input$coupon,
        freq = as.integer(input$freq),
        face = input$face,
        conv = dcc_token(input$dcc),
        position_nominal = input$position_nominal
      )
      
      if (any(is.na(unlist(dd[c("prev_coupon","next_coupon","days_accrued","days_period","af","ai_per_100")])))) {
        return("Daycount: n/a (fehlende Periodengrenzen)")
      }
      
      lines <- c(
        sprintf("Konvention: %s   Frequenz: %sx", input$dcc, as.integer(input$freq)),
        sprintf("Voriger Kupon: %s   Nächster Kupon: %s", format(dd$prev_coupon), format(dd$next_coupon)),
        sprintf("Days accrued: %d   Days in period: %d   AF: %.9f", dd$days_accrued, dd$days_period, dd$af),
        sprintf("Stückzinsen je 100: %.6f", dd$ai_per_100)
      )
      
      if (!is.na(dd$ai_total)) {
        lines <- c(lines, sprintf("Stückzinsen gesamt (Nominal %.0f): %.2f", input$position_nominal, dd$ai_total))
      }
      paste(lines, collapse = "\n")
    })
    
    # --- Price-Yield-Plot ---
    output$price_yield_curve <- renderPlot({
      req(input$settle, input$maturity, input$coupon)
      y_grid <- seq(-0.02, 0.12, by = 0.001)
      p <- sapply(y_grid, function(y)
        price_from_ytm(
          input$settle, input$maturity, input$coupon/100, y,
          freq = as.integer(input$freq), face = input$face, conv = dcc_token(input$dcc)
        )$clean
      )
      df <- data.frame(ytm = 100*y_grid, price = p)
      ggplot(df, aes(x = ytm, y = price)) +
        geom_line() +
        geom_hline(yintercept = input$price, linetype = "dashed") +
        labs(x = "YTM (%)", y = "Clean Price", title = "Price-Yield") +
        theme_minimal(base_size = 11)
    })
  })
}
