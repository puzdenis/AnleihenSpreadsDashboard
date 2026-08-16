# ---------- module_plausi.R ----------
library(dplyr)
library(DT)
library(shiny)
library(lubridate)
library(shinyvalidate)

plausi_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class="neon-title", "Plausi-Check"),
    div(class="glass-box",
        fluidRow(
          column(3,
                 dateInput(ns("settle"), "Settlement", value = Sys.Date()),
                 dateInput(ns("maturity"), "Fälligkeit"),
                 selectInput(ns("freq"), "Frequenz", c("jährlich"=1,"halbjährlich"=2,"quartal"=4), selected=1),
                 selectInput(ns("dcc"), "Day Count", c("ACT/365","ACT/360","30E/360", "ICMA ACT/ACT"), selected="ICMA ACT/ACT")
          ),
          column(3,
                 numericInput(ns("coupon"), "Kupon p.a. (%)", value = 3.00, step = 0.01, min = 0, max = 100),
                 numericInput(ns("price"),  "Preis (clean)", value = 99.50, step = 0.01, min = 0.0001, max = 400),
                 checkboxInput(ns("is_clean"), "Eingabe ist Clean Price", TRUE)
          ),
          column(3,
                 numericInput(ns("ytm_hint"), "YTM-Hinweis (%) (optional)", value = NA, step = 0.01),
                 numericInput(ns("face"), "Nominal", value = 100, step = 1, min = 0.01)
          ),
          column(3,
                 actionButton(ns("run"), "Check jetzt", class="btn btn-primary")
          )
        ),
        hr(),
        DTOutput(ns("table_checks"))
    )
  )
}

plausi_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- Auto-Fälligkeit, wenn leer: Settlement + 5 Jahre
    observeEvent(input$settle, ignoreInit = TRUE, {
      if (is.null(input$maturity) || is.na(input$maturity)) {
        updateDateInput(session, "maturity", value = as.Date(input$settle) %m+% years(5))
      }
    })
    
    # --- shinyvalidate: Feldnahe Validierung
    iv <- InputValidator$new()
    iv$add_rule("settle", sv_required("Bitte Settlement-Datum angeben."))
    iv$add_rule("maturity", sv_required("Bitte Fälligkeitsdatum angeben."))
    iv$add_rule("maturity", function(value) {
      s <- input$settle
      if (!is.na(s) && !is.na(value) && as.Date(value) <= as.Date(s)) {
        "Fälligkeit muss nach Settlement liegen."
      }
    })
    iv$add_rule("coupon", sv_between(0, 100, inclusive = c(TRUE, FALSE), message_fmt = "Kupon muss in [0; 100) liegen."))
    iv$add_rule("price",  sv_between(0, 400, inclusive = c(FALSE, FALSE), message_fmt = "Preis muss in (0; 400) liegen."))
    iv$add_rule("face",   sv_gt(0, message_fmt = "Nominal muss > 0 sein."))
    iv$enable()
    
    # --- Kernberechnung: Event auf Button
    checks <- eventReactive(input$run, {
      req(iv$is_valid())
      
      s <- as.Date(input$settle); m <- as.Date(input$maturity)
      freq <- as.integer(input$freq); dcc <- input$dcc
      coup <- as.numeric(input$coupon)/100
      prc  <- as.numeric(input$price)
      face <- as.numeric(input$face)
      is_clean <- isTRUE(input$is_clean)
      
      # Basisvalidierungen (redundant aber sichtbar in Tabelle)
      v <- list()
      v[["Datum konsistent"]] <- if (!is.na(s) && !is.na(m) && m > s) "OK" else "FEHLER: Fälligkeit ≤ Settlement"
      v[["Kupon sinnvoll"]]   <- if (!is.na(coup) && coup >= 0 && coup < 1) "OK" else "WARN: Kupon unplausibel"
      v[["Preis plausibel"]]  <- if (!is.na(prc)  && prc > 0 && prc < 400) "OK" else "FEHLER: Preis unplausibel"
      v[["Nominal > 0"]]      <- if (!is.na(face) && face > 0) "OK" else "FEHLER: Nominal ≤ 0"
      
      # Implizite YTM
      ytm_est <- tryCatch({
        y <- ytm_from_price(
          settle=s, maturity=m,
          coupon_rate=coup,
          price_input=prc,
          is_clean=is_clean,
          freq=freq,
          face=face,
          conv=dcc
        )
        as.numeric(y)
      }, error = function(e) NA_real_)
      
      if (!is.na(ytm_est)) {
        v[["Implizite YTM (aus Preis)"]] <- sprintf("%.3f %%", 100*ytm_est)
        
        # Vergleich zu Hinweis
        if (!is.na(input$ytm_hint)) {
          diff_bp <- round((ytm_est - input$ytm_hint/100)*10000)
          flag <- dplyr::case_when(
            abs(diff_bp) <= 10  ~ "OK (±10bp)",
            abs(diff_bp) <= 50  ~ "HINWEIS (±50bp)",
            TRUE                ~ "WARN (>±50bp)"
          )
          v[["YTM vs. Hinweis"]] <- sprintf("%s | Δ = %d bp", flag, diff_bp)
        }
        
        # AI & Dirty via Helper
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
    
    # --- Tabelle rendern (Ampel breiter gefasst)
    output$table_checks <- renderDT({
      req(checks())
      datatable(
        checks(),
        rownames = FALSE,
        options = list(dom="t", pageLength=100),
        escape = TRUE
      ) %>%
        formatStyle(
          "Ergebnis", target = "cell",
          backgroundColor = styleEqual(
            c("OK", "OK (±10bp)"),
            c("#e8ffee", "#e8ffee")
          )
        ) %>%
        formatStyle(
          "Ergebnis", target = "cell",
          color = styleInterval(
            c(0), c("","") # Platzhalter
          )
        )
    })
    
    # --- (Neu) Rückgabe: Checks reaktiv, damit du sie z.B. exportieren kannst
    return(list(checks = checks))
  })
}