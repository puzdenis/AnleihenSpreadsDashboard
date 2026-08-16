

aktuelle_version <- "3.6"

library(shiny)
library(bslib)          
library(shinyWidgets)   
library(ggplot2)
library(ggthemes)
library(readxl)
library(dplyr)
library(DT)
library(plotly)
library(grid)
library(gridExtra)
library(stringr)
library(lubridate)
library(RQuantLib) 


# Explizit sourcen 
source("helpers_calendar.R", encoding = "UTF-8")
source("helpers_yield.R",    encoding = "UTF-8")
source("helpers_daycount.R", encoding = "UTF-8")  # falls genutzt
source("module_yield.R",     encoding = "UTF-8")  # erwartet: module_yield_ui / module_yield_server
source("module_mkk.R",       encoding = "UTF-8")  # erwartet: mkk_ui / mkk_server

# Falls du zusätzlich dein dynamisches Sourcen über base/files nutzen willst,
# definiere base & files und lass den Block aktiv — ansonsten kannst du ihn entfernen.
# Beispiel:
base  <- "G:/WP/Mitarbeiter/Puzikov/5 Marktkonformitätsthemen/5 R Rstudio/Spreadvergleich"
files <- c("helpers_calendar.R","helpers_yield.R","helpers_daycount.R", "module_yield.R","module_mkk.R", "global.R")
if (exists("base") && exists("files")) {
  invisible(lapply(file.path(base, files), function(p) {
    source(normalizePath(p, winslash = "/", mustWork = TRUE), encoding = "UTF-8")
  }))
}

options(shiny.maxRequestSize = 100 * 1024^2)  # 100 MB


# UI
ui <- page_navbar(
  title = paste("MKK Dashboard", aktuelle_version),
  theme = bs_theme(bootswatch = "cerulean"),
  bg = "#A3B7CA",
  
  # Startseite variiert je nach Umgebung
  if (testumgebung) {
    nav_panel(
      title = "Startseite",
      div(
        style = "padding: 24px;",
        tags$h1("TESTUMGEBUNG!", style = "color:red; font-size: 64px; font-weight: 800;"),
        tags$p("Dies ist die Testumgebung. Daten und Ergebnisse sind nicht produktiv."),
        tags$hr(),
        tags$p(HTML(paste0(
          "<b>Build:</b> ", aktuelle_version,
          " &nbsp; | &nbsp; <b>Modus:</b> TEST &nbsp; | &nbsp; ",
          format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )))
      )
    )
  } else {
    nav_panel(
      title = "Startseite",
      div(
        style = "padding: 24px;",
        tags$h2("Willkommen in der MKK WebApp"),
        tags$p("Hier kommt Startseitencontent, falls gewünscht."),
        tags$hr(),
        tags$p(HTML(paste0(
          "<b>Build:</b> ", aktuelle_version,
          " &nbsp; | &nbsp; <b>Modus:</b> PRODUKTIV &nbsp; | &nbsp; ",
          format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )))
      )
    )
  },
  
  # --- Reiter: Rendite-Plausi (Yield) ---
  # Erwartet ein UI-Funktionsnamen 'module_yield_ui'
  nav_panel(
    title = "MKK Rendite Plausi",
    yield_ui("Renditeplausi")
  ),
  
  # --- Reiter: MKK Bewertung / Plausi (Marktkonformität) ---
  # Erwartet ein UI-Funktionsnamen 'mkk_ui'
  nav_panel(
    title = "MKK Bewertung",
    mkk_ui("mkk")
  )
  
  # --- weitere Reiter einfach anhängen ---
  # , nav_panel(title = "Daycount-Rechner", daycount_ui("daycount"))  # falls vorhanden
)

# -------------------------
# SERVER
# -------------------------
server <- function(input, output, session) {
  
  # Umgebungshinweise in der Konsole
  if (isTRUE(testumgebung)) {
    message(">>> Starte im TEST-Modus (", aktuelle_version, ")")
  } else {
    message(">>> Starte im PRODUKTIV-Modus (", aktuelle_version, ")")
  }
  
  # Modul-Server anstöpseln
  # Rendite-Plausi
  # Erwartet Server-Funktion 'module_yield_server(id, ...optional args...)'
  yield_server("Renditeplausi")
  
  # MKK Bewertung
  # Erwartet Server-Funktion 'mkk_server(id, ...optional args...)'
  mkk_server("mkk")
  
  # Optional: globale Reaktionen / Theming / Session-Handling etc.
  # observeEvent(session$clientData$url_search, { ... })
}

# -------------------------
# App starten
# -------------------------
shinyApp(ui = ui, server = server)
