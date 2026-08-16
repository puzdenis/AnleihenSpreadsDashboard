# module_MKK.R
library(shiny)
library(shinyvalidate)
library(lubridate)
library(DT)
library(dplyr)
library(readr)
library(stringr)

#UI
mkk_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "neon-title", "MKK Bewertung"),
    div(class = "glass-box",
        fluidRow(
          # Linke Seite - Plain Vanilla
          column(
            width = 6,
            h4("Plain-Vanilla Plausi"),
            fluidRow(
              column(6,
                     dateInput(ns("plain_settle"), "Settlement", value = Sys.Date()),
                     dateInput(ns("plain_maturity"), "Fälligkeit"),
                     selectInput(ns("plain_freq"), "Frequenz",
                                 c("jährlich"=1,"halbjährlich"=2,"quartal"=4), selected = 1),
                     selectInput(ns("plain_dcc"), "Day Count",
                                 c("ICMA ACT/ACT","30E/360","ACT/360"), selected = "ICMA ACT/ACT")
              ),
              column(6,
                     numericInput(ns("plain_coupon"), "Kupon p.a. (%)", value = 3.00, step = 0.01, min = 0, max = 100),
                     numericInput(ns("plain_price"),  "Preis (clean)", value = 99.50, step = 0.01, min = 0.0001, max = 400),
                     checkboxInput(ns("plain_is_clean"), "Eingabe ist Clean Price", TRUE),
                     numericInput(ns("plain_ytm_hint"), "YTM-Hinweis (%) (optional)", value = NA, step = 0.01),
                     numericInput(ns("plain_face"), "Nominal", value = 100, step = 1, min = 0.01)
              )
            ),
            fluidRow(
              column(6, actionButton(ns("btn_plain_run"), "Plain prüfen", class = "btn btn-primary")),
              column(6, actionButton(ns("btn_plain_demo"), "Plain Beispiel laden"))
            ),
            hr(),
            DTOutput(ns("plain_table_checks"))
          ),
          # Rechts Callable
          column(
            width = 6,
            h4("Callable Paketgeschäft"),
            fluidRow(
              column(6,
                     # Bond Terms / Call
                     dateInput(ns("call_settle"), "Settlement (Callable)", value = Sys.Date()),
                     dateInput(ns("call_maturity"), "Endfälligkeit"),
                     numericInput(ns("call_coupon"), "Kupon p.a. (%)", value = 3.00, step = 0.01, min = 0, max = 100),
                     selectInput(ns("call_freq"), "Frequenz", c("jährlich"=1,"halbjährlich"=2,"quartal"=4), selected = 1),
                     dateInput(ns("first_call"), "Erster Kündigungstermin (Call)"),
                     numericInput(ns("redemption"), "Rückzahlung (%)", value = 100, step = 0.01, min = 0.01)
              ),
              column(6,
                     # Funding + Spreads + (temporär) p_call
                     numericInput(ns("funding_bp"), "Funding Level (bp)", value = 30, step = 1),
                     fileInput(ns("scd_spreads_csv"), "SCD Spreads CSV (date; spread in % oder bp)",
                               accept = c(".csv", "text/csv", "text/comma-separated-values,text/plain")),
                     checkboxInput(ns("spreads_are_bp"), "Werte sind bereits in bp (Haken setzen, wenn ja)", value = FALSE),
                     sliderInput(ns("p_call_tmp"), "Ausübungswahrscheinlichkeit p_call (TEMP, bis DV01-Engine steht)",
                                 min = 0, max = 1, value = 0.3, step = 0.01),
                     # HW-Parameter (noch ohne Engine in Step 1)
                     numericInput(ns("hw_alpha"), "HW α (Mean Reversion)", value = 0.03, step = 0.005, min = 0),
                     numericInput(ns("hw_sigma"), "HW σ (Volatilität)", value = 0.010, step = 0.001, min = 0),
                     numericInput(ns("hw_grid"),  "HW Grid (Intervals)", value = 160, step = 10, min = 40)
              )
            ),
            fluidRow(
              column(6, actionButton(ns("btn_call_run"), "Callable prüfen", class = "btn btn-primary")),
              column(6, actionButton(ns("btn_call_demo"), "Callable Beispiel laden"))
            ),
            hr(),
            h5("SCD Spreads Vorschau"),
            DTOutput(ns("scd_spreads_preview")),
            hr(),
            h5("Callable KPIs (v1)"),
            DTOutput(ns("callable_kpis_table"))
          )
        )
    )
  )
}

# SERVER
mkk_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # LINKS: PLAIN 
    observeEvent(input$plain_settle, ignoreInit = TRUE, {
      if (is.null(input$plain_maturity) || is.na(input$plain_maturity)) {
        updateDateInput(session, "plain_maturity", value = as.Date(input$plain_settle) %m+% years(5))
      }
    })
    
    iv_plain <- InputValidator$new()
    iv_plain$add_rule("plain_settle",  sv_required("Bitte Settlement-Datum angeben."))
    iv_plain$add_rule("plain_maturity", sv_required("Bitte Fälligkeitsdatum angeben."))
    iv_plain$add_rule("plain_maturity", function(value) {
      s <- input$plain_settle
      if (!is.na(s) && !is.na(value) && as.Date(value) <= as.Date(s)) {
      }
    })
    iv_plain$add_rule("plain_coupon", sv_between(0, 100, inclusive = c(TRUE, FALSE), message_fmt = "Kupon muss in [0; 100) liegen."))
    iv_plain$add_rule("plain_price",  sv_between(0, 400, inclusive = c(FALSE, FALSE), message_fmt = "Preis muss in (0; 400) liegen."))
    iv_plain$add_rule("plain_face",   sv_gt(0, message_fmt = "Nominal muss > 0 sein."))
    iv_plain$enable()
    
    observeEvent(input$btn_plain_demo, {
      today <- Sys.Date()
      updateDateInput(session, "plain_settle", value = today)
      updateDateInput(session, "plain_maturity", value = today %m+% years(5))
      updateSelectInput(session, "plain_freq", selected = 1)
      updateSelectInput(session, "plain_dcc",  selected = "ICMA ACT/ACT")
      updateNumericInput(session, "plain_coupon", value = 3.00)
      updateNumericInput(session, "plain_price",  value = 99.50)
      updateCheckboxInput(session, "plain_is_clean", value = TRUE)
      updateNumericInput(session, "plain_face",   value = 100)
      updateNumericInput(session, "plain_ytm_hint", value = NA)
    }, ignoreInit = TRUE)
    
    plain_checks <- eventReactive(input$btn_plain_run, {
      req(iv_plain$is_valid())
      
      s <- as.Date(input$plain_settle); m <- as.Date(input$plain_maturity)
      freq <- as.integer(input$plain_freq); dcc <- input$plain_dcc
      coup <- as.numeric(input$plain_coupon)/100
      prc  <- as.numeric(input$plain_price)
      face <- as.numeric(input$plain_face)
      is_clean <- isTRUE(input$plain_is_clean)
      
      v <- list()
      v[["Datum konsistent"]] <- if (!is.na(s) && !is.na(m) && m > s) "OK" else "FEHLER: Fälligkeit ≤ Settlement"
      v[["Kupon sinnvoll"]]   <- if (!is.na(coup) && coup >= 0 && coup < 1) "OK" else "WARN: Kupon unplausibel"
      v[["Preis plausibel"]]  <- if (!is.na(prc)  && prc > 0 && prc < 400) "OK" else "FEHLER: Preis unplausibel"
      v[["Nominal > 0"]]      <- if (!is.na(face) && face > 0) "OK" else "FEHLER: Nominal ≤ 0"
      
      # Implizite YTM (Helfer aus deinem Projekt)
      ytm_est <- tryCatch({
        y <- ytm_from_price(settle = s, maturity = m,
                            coupon_rate = coup,
                            price_input = prc,
                            is_clean = is_clean,
                            freq = freq,
                            face = face,
                            conv = dcc)
        as.numeric(y)
      }, error = function(e) NA_real_)
      
      if (!is.na(ytm_est)) {
        v[["Implizite YTM (aus Preis)"]] <- sprintf("%.3f %%", 100*ytm_est)
        
        if (!is.na(input$plain_ytm_hint)) {
          diff_bp <- round((ytm_est - input$plain_ytm_hint/100)*10000)
          flag <- dplyr::case_when(
            abs(diff_bp) <= 10  ~ "OK (±10bp)",
            abs(diff_bp) <= 50  ~ "HINWEIS (±50bp)",
            TRUE                ~ "WARN (>±50bp)"
          )
          v[["YTM vs. Hinweis"]] <- sprintf("%s | Δ = %d bp", flag, diff_bp)
        }
        
        pr <- tryCatch({
          price_from_ytm(s, m, coup, ytm_est, freq, face, dcc)
        }, error = function(e) NULL)
        
        if (!is.null(pr)) {
          v[["Accrued Interest"]] <- sprintf("%.6f", pr$ai)
          v[["Dirty Price"]]      <- sprintf("%.6f", pr$dirty)
          if (is_clean) {
            v[["Clean Price (Input)"]] <- sprintf("%.6f", prc)
            v[["Dirty vs. Clean Plausi"]] <- if (abs((pr$dirty - (prc + pr$ai))) < 1e-4) "OK" else "HINWEIS: Dirty ≠ Clean+AI"
          }
        } else {
          v[["Accrued Interest"]] <- "n/a"
          v[["Dirty Price"]]      <- "n/a"
        }
      } else {
        v[["Implizite YTM (aus Preis)"]] <- "nicht berechenbar"
      }
      
      tibble(Check = names(v), Ergebnis = unname(unlist(v)))
    })
    
    output$plain_table_checks <- renderDT({
      req(plain_checks())
      datatable(
        plain_checks(),
        rownames = FALSE,
        options = list(dom = "t", pageLength = 100),
        escape = TRUE
      ) %>%
        formatStyle("Ergebnis", target = "cell",
                    backgroundColor = styleEqual(c("OK", "OK (±10bp)"), c("#e8ffee", "#e8ffee")))
    })
    
    #  RECHTS: CALLABLE (STEP 1: CSV + KPIs mit temporärem p_call) 
    observeEvent(input$call_settle, ignoreInit = TRUE, {
      if (is.null(input$call_maturity) || is.na(input$call_maturity)) {
        updateDateInput(session, "call_maturity", value = as.Date(input$call_settle) %m+% years(10))
      }
      if (is.null(input$first_call) || is.na(input$first_call)) {
        updateDateInput(session, "first_call", value = as.Date(input$call_settle) %m+% years(5))
      }
    })
    
    iv_call <- InputValidator$new()
    iv_call$add_rule("call_settle",  sv_required("Bitte Settlement angeben."))
    iv_call$add_rule("call_maturity", sv_required("Bitte Endfälligkeit angeben."))
    iv_call$add_rule("first_call", sv_required("Bitte ersten Kündigungstermin angeben."))
    iv_call$add_rule("call_maturity", function(value) {
      s <- input$call_settle
      if (!is.na(s) && !is.na(value) && as.Date(value) <= as.Date(s)) {
      }
    })
    iv_call$add_rule("first_call", function(value) {
      s <- input$call_settle
      if (!is.na(s) && !is.na(value) && as.Date(value) <= as.Date(s)) {
      }
    })
    iv_call$add_rule("funding_bp", sv_gte(0, "Funding Level muss ≥ 0 bp sein."))
    iv_call$enable()
    
    # Demo-Loader für Callable
    observeEvent(input$btn_call_demo, {
      today <- Sys.Date()
      updateDateInput(session, "call_settle", value = today)
      updateDateInput(session, "call_maturity", value = today %m+% years(15))
      updateDateInput(session, "first_call", value = today %m+% years(10)) # Beispiel: 15Y NC10
      updateNumericInput(session, "call_coupon", value = 3.00)
      updateSelectInput(session, "call_freq", selected = 1)
      updateNumericInput(session, "redemption", value = 100)
      updateNumericInput(session, "funding_bp", value = 30)
      updateSliderInput(session, "p_call_tmp", value = 0.3)
    }, ignoreInit = TRUE)
    
    # Einlesen SCD Spreads CSV (Semikolon + deutsches Dezimalkomma robust)
    spreads_df <- reactive({
      file <- input$scd_spreads_csv
      validate(need(!is.null(file), "Bitte SCD-Spreads CSV laden (date; value)."))
      df <- suppressWarnings(readr::read_delim(
        file$datapath, delim = ";",
        col_names = c("date_raw","value_raw"),
        locale = locale(decimal_mark = ","),
        trim_ws = TRUE,
        show_col_types = FALSE
      ))
      # Datum parsen: erlaubt dd.mm.yyyy oder yyyy-mm-dd
      df <- df |>
        mutate(date = suppressWarnings(lubridate::dmy(date_raw))) |>
        mutate(date = ifelse(is.na(date), as.Date(date_raw), date)) |>
        mutate(date = as.Date(date, origin = "1970-01-01")) |>
        arrange(date) |>
        filter(!is.na(date))
      
      # Wert → bp
      if (isTRUE(input$spreads_are_bp)) {
        df <- df |>
          mutate(spread_bp = as.numeric(str_replace_all(value_raw, ",", "."))) # bereits bp
      } else {
        # Werte in % (z.B. 0,30) → bp
        df <- df |>
          mutate(spread_pct = as.numeric(str_replace_all(value_raw, ",", ".")),
                 spread_bp  = spread_pct * 100) # 0.30% -> 30 bp
      }
      df |>
        select(date, spread_bp) |>
        distinct(date, .keep_all = TRUE) |>
        arrange(date)
    })
    
    output$scd_spreads_preview <- renderDT({
      req(spreads_df())
      datatable(spreads_df(), rownames = FALSE,
                options = list(pageLength = 8, lengthChange = FALSE))
    })
    
    # Callable KPIs (v1): Option 1/2, erwarteter Spread, Funding-Vergleich – temporär mit p_call_tmp
    callable_kpis <- eventReactive(input$btn_call_run, {
      req(iv_call$is_valid())
      req(spreads_df())
      
      s <- as.Date(input$call_settle)
      m <- as.Date(input$call_maturity)
      t_call <- as.Date(input$first_call)
      validate(
        need(t_call > s, "Call-Datum muss nach Settlement liegen."),
        need(m > t_call, "Endfälligkeit muss nach Call-Datum liegen.")
      )
      
      df <- spreads_df()
      
      # Option 1: bis Call
      opt1 <- df |>
        filter(date <= t_call)
      # Option 2: bis Maturity
      opt2 <- df |>
        filter(date <= m)
      
      validate(
        need(nrow(opt1) > 0, "Spreads decken den Zeitraum bis zum Call nicht ab."),
        need(nrow(opt2) > 0, "Spreads decken den Zeitraum bis zur Fälligkeit nicht ab.")
      )
      
      mean1 <- mean(opt1$spread_bp, na.rm = TRUE)
      mean2 <- mean(opt2$spread_bp, na.rm = TRUE)
      
      p_call  <- as.numeric(input$p_call_tmp)
      p_nocal <- 1 - p_call
      
      expected_bp <- p_call * mean1 + p_nocal * mean2
      funding_bp  <- as.numeric(input$funding_bp)
      advantage_bp <- expected_bp - funding_bp # <0 => Vorteil ggü. Funding
      
      tibble::tibble(
        Kennzahl = c(
          "p_call (TEMP)",
          "p_no_call (TEMP)",
          "Ø Spread Option 1 (bis Call, bp)",
          "Ø Spread Option 2 (bis Maturity, bp)",
          "Erwarteter Spread (bp)",
          "Funding Level (bp)",
          "Vorteil (+)/Nachteil (-) ggü. Funding (bp)"
        ),
        Wert = c(
          sprintf("%.1f %%", 100*p_call),
          sprintf("%.1f %%", 100*p_nocal),
          sprintf("%.2f", mean1),
          sprintf("%.2f", mean2),
          sprintf("%.2f", expected_bp),
          sprintf("%.2f", funding_bp),
          sprintf("%.2f", advantage_bp)
        )
      )
    })
    
    output$callable_kpis_table <- renderDT({
      req(callable_kpis())
      datatable(callable_kpis(), rownames = FALSE,
                options = list(dom = "t", pageLength = 20))
    })
    
    # Rückgabe (falls du extern auf Ergebnisse zugreifen willst)
    return(list(
      plain_checks   = plain_checks,
      spreads_df     = spreads_df,
      callable_kpis  = callable_kpis
    ))
  })
}
