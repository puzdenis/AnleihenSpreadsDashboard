

#Hier ist hauptsächlich die GUI drin sowie das Reinladen der Daten
#
#
#
#

base <- "G:/WP/Mitarbeiter/Puzikov/5 Marktkonformitätsthemen/5 R Rstudio/Spreadvergleich"
to_source <- c(
  "packages.R",
  "helpers_calendar.R","helpers_yield.R","helpers_daycount.R",
  "module_yield.R","module_mkk.R",
  "main_dashboard.R"   # <- UI/Server kommt erst NACH den Modulen
)
invisible(lapply(file.path(base, to_source), function(p)
  source(normalizePath(p, winslash = "/", mustWork = TRUE), encoding = "UTF-8")
))

#

mkk_excel_path <- "G:/WP/Mitarbeiter/Puzikov/5 Marktkonformitätsthemen/5 R Rstudio/Spreadvergleich/Hilfdatei mkk-reiter R.xlsx"

mkk_raw <- readxl::read_excel(mkk_excel_path, sheet = 1, col_names = TRUE)

mkk_data <- mkk_raw %>%
  dplyr::mutate(
    # nur Alias-Spalten anlegen – Originale bleiben unverändert:
    Produktart = trimws(as.character(`Geschäftsart`)),
    Schritt    = trimws(as.character(`SCD-Vorgänge`)),
    # Reihenfolge aus führender Nummer extrahieren (falls vorhanden), sonst laufende Nummer je Produktart
    Reihenfolge = suppressWarnings(as.integer(stringr::str_extract(`SCD-Vorgänge`, "^\\s*(\\d+)")))
  ) %>%
  dplyr::filter(!is.na(Schritt) & Schritt != "") %>%
  dplyr::group_by(Produktart) %>%
  dplyr::mutate(Reihenfolge = dplyr::coalesce(Reihenfolge, dplyr::row_number())) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(Produktart, Reihenfolge)


# Markdaten reinladen


df_raw <- read_excel("G:/WP/Mitarbeiter/Puzikov/5 Marktkonformitätsthemen/5 R Rstudio/Spreadvergleich/MarktdatenSpreadsbereinigtmitRendite.xlsx")

df_raw <- df_raw %>%
  filter(!is.na(Zinsart)) %>%
  mutate(
    Zinsart = toupper(trimws(Zinsart)),
    `Laufzeit in Jahren` = round(`Laufzeit in Jahren`, 2)
  )

min_lz <- min(df_raw$`Laufzeit in Jahren`, na.rm = TRUE)
max_lz <- max(df_raw$`Laufzeit in Jahren`, na.rm = TRUE)
zinsarten <- sort(unique(df_raw$Zinsart))
emittenten <- sort(unique(df_raw$Emittent))

df_bandbreiten <- read_excel("G:/WP/Mitarbeiter/Puzikov/5 Marktkonformitätsthemen/5 R Rstudio/Spreadvergleich/BandbreitenMKK2.xlsx") %>%
  select(Kategorie, Segment, Wert)


# UI Frontend

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background: linear-gradient(270deg, #e5e5e5, #f0f2f6, #cccccc);
        background-size: 600% 600%;
        animation: gradientBG 2s ease forwards;
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      }
      @keyframes gradientBG {
        0% { background-position: 0% 50%; }
        100% { background-position: 100% 50%; }
      }
      .glass-box {
        background: linear-gradient(to bottom, #f0f8ff, #ddeeff);
        border: 1px solid #819cd6;
        border-radius: 10px;
        box-shadow: inset 0 1px 0 #ffffff, 0 2px 5px rgba(0, 0, 0, 0.2);
        padding: 20px;
        margin: 10px;
      }
      .neon-title {
        font-family: 'Arial Black', sans-serif;
        font-size: 32px;
        color: #1E90FF;
        text-shadow:
          -1px -1px 0 black,
           1px -1px 0 black,
          -1px 1px 0 black,
           1px 1px 0 black,
           0 0 4px #1E90FF;
        margin-bottom: 20px;
      }
      .form-control, .selectize-input, .dataTable, .shiny-input-container, .selectize-control {
        background-color: #ffffff !important;
        border-radius: 6px !important;
        border: 1px solid #7da2ce !important;
        box-shadow: 0 0 2px #b0cfff !important;
        padding: 5px;
      }
      .form-control:hover, .btn:hover, .selectize-input:hover, .selectize-input.focus, .dataTable:hover {
        box-shadow: 0 0 5px #b0cfff !important;
        border-color: #4686ff !important;
      }
      .btn {
        background: linear-gradient(to bottom, #e2efff, #b0d2ff);
        border: 1px solid #7da2ce;
        border-radius: 6px;
        color: #003366;
        font-weight: bold;
        text-shadow: 1px 1px #ffffff;
        box-shadow: 0 2px 4px rgba(0,0,0,0.2);
      }
      .dataTables_wrapper, table.dataTable td, table.dataTable th {
        color: #222 !important;
        background-color: #ffffff !important;
      }
      ::-webkit-scrollbar-thumb {
        background-color: #c0d3ff;
        border-radius: 10px;
      }
      #download_excel, #download_pdf {
  background: none;
  border: none;
  padding: 6px;
  margin-right: 10px;
      }
.mkk-panel .btn { margin-right:6px; }
.mkk-panel .checkbox { margin-bottom:6px; }
.mkk-panel .shiny-options-group { margin-top:6px; }

#download_excel::before {
  content: url('https://cdn-icons-png.flaticon.com/24/732/732220.png'); /* Excel-Icon */
}

#download_pdf::before {
  content: url('https://cdn-icons-png.flaticon.com/24/337/337946.png'); /* PDF-Icon */
}
    "))
  ),
  
  tags$div(class = "neon-title", "DZ HYP & Markt Betrachtung"),
 
      sidebarLayout(
        sidebarPanel(
          selectInput("zinsart", "Zinsart wählen:", choices = zinsarten, multiple = TRUE, selected = "H"),
          sliderInput("laufzeit", "Laufzeit (in Jahren):", min = min_lz, max = max_lz, value = c(min_lz, max_lz)),
          selectizeInput("emittenten", "Emittent(en) auswählen:", choices = emittenten, multiple = TRUE, selected = c("DZ HYP AG", "AAREAL BANK AG")),
          h4("Bandbreiten (BBSpreads)"),
          tags$div(class = "glass-box",
                   radioButtons("spread_type", "Spread-Typ wählen:",
                                choices = c("Asset Swap Spread" = "AssetSwapSpreadMid",
                                            "REFZS Spread (SCD)" = "REFZS_SPREAD_SCD"),
                                selected = "AssetSwapSpreadMid")
          ),
          selectInput("bb_kategorie", "Bandbreiten-Kategorie:", choices = unique(df_bandbreiten$Kategorie), multiple = TRUE),
          selectInput("bb_segment", "Segment wählen:", choices = unique(df_bandbreiten$Segment), multiple = TRUE),
          DTOutput("bandbreitenTabelle"),
          # MKK-Panel in der Sidebar, direkt unter "Segment wählen"
          tags$div(class = "glass-box mkk-panel",
                   h4("MKK-Leitfaden"),
                   selectInput(
                     "mkk_produktart", "Produktart wählen:",
                     choices  = sort(unique(mkk_data$Produktart)),
                     selected = sort(unique(mkk_data$Produktart))[1]
                   ),
                   div(class = "btn-row",
                       actionButton("mkk_select_all", "Alle markieren"),
                       actionButton("mkk_clear_all", "Zurücksetzen"),
                       downloadButton("download_mkk_excel", "Original-Excel")
                   ),
                   # schlanker Scroll-Container für die Liste
                   div(style = "max-height: 300px; overflow:auto; border:1px solid #cbd5e1; border-radius:6px; padding:8px; margin-top:8px;",
                       uiOutput("mkk_checklist_ui")
                   )
              )
          ),
        mainPanel(
          fluidRow(
            column(6, tags$div(class = "glass-box", plotlyOutput("spreadPlot"))),
            column(6, tags$div(class = "glass-box", plotlyOutput("renditePlot")))
          ),
          tags$div(class = "glass-box",
                   h4("Detaillierte Tabelle (inkl. Spread + Rendite)"),
                   downloadButton("download_excel", label = NULL),
                   downloadButton("download_pdf", label = NULL),
                   DTOutput("renditeTabelle")
          ),
          fluidRow(
            column(6, mkk_ui("mkk")),
            column(6, yield_ui("ycalc"))
          )
        )
    )
)

# SERVER

server <- function(input, output, session) {
  
  # Reactives 
  
  filteredData <- reactive({
    data <- df_raw %>%
      filter(
        Zinsart %in% input$zinsart,
        `Laufzeit in Jahren` >= input$laufzeit[1],
        `Laufzeit in Jahren` <= input$laufzeit[2]
      )
    if (!is.null(input$emittenten) && length(input$emittenten) > 0) {
      data <- data[data$Emittent %in% input$emittenten, ]
    }
    data
  })
  
  filteredBandbreiten <- reactive({
    req(input$bb_kategorie, input$bb_segment)
    df_bandbreiten %>% filter(Kategorie %in% input$bb_kategorie,
                              Segment %in% input$bb_segment)
  })
  
  filteredDZ  <- reactive({ filteredData() %>% filter(Emittent == "DZ HYP AG") })
  filteredMKT <- reactive({ filteredData() %>% filter(Emittent != "DZ HYP AG") })
  
  # --- Einheitliche Datengrundlage für Tabelle + Export ---
  df_detail <- reactive({
    filteredData()[, c("Emittent", "Laufzeit in Jahren",
                       "AssetSwapSpreadMid", "REFZS_SPREAD_SCD", "Rendite")]
  })
  
# Plots
  output$spreadPlot <- renderPlotly({
    selected_spread <- input$spread_type
    
    dz <- filteredData() %>%
      filter(Emittent == "DZ HYP AG") %>%
      mutate(Source = "DZ HYP", Spread = .data[[selected_spread]])
    
    mkt <- filteredData() %>%
      filter(Emittent != "DZ HYP AG") %>%
      mutate(Source = "Markt", Spread = .data[[selected_spread]])
    
    combined <- dplyr::bind_rows(dz, mkt)
    
    title_text <- if (selected_spread == "AssetSwapSpreadMid") {
      "Asset Swap Spread Vergleich"
    } else {
      "REFZS Spread Vergleich"
    }
    
    p <- ggplot(combined, aes(x = `Laufzeit in Jahren`, y = Spread, color = Source, shape = Source,
                              text = paste("Emittent:", Emittent,
                                           "<br>Laufzeit:", `Laufzeit in Jahren`,
                                           "<br>Spread:", round(Spread, 2)))) +
      geom_point(size = 3, alpha = 0.9) +
      scale_color_manual(values = c("DZ HYP" = "blue", "Markt" = "orange")) +
      scale_shape_manual(values = c("DZ HYP" = 17, "Markt" = 16)) +
      labs(title = title_text, y = "Spread (bps)", x = "Laufzeit (Jahre)") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$renditePlot <- renderPlotly({
    data <- filteredMKT()
    p <- ggplot(data, aes(x = `Laufzeit in Jahren`, y = Rendite,
                          text = paste("Emittent:", Emittent,
                                       "<br>Laufzeit:", `Laufzeit in Jahren`,
                                       "<br>Rendite:", round(Rendite, 3)))) +
      geom_point(color = "purple", size = 3) +
      labs(title = "Renditen externer Emittenten", y = "Rendite (%)", x = "Laufzeit (Jahre)") +
      theme_minimal()
    ggplotly(p, tooltip = "text")
  })
  
#Tabellen
  output$renditeTabelle <- DT::renderDT({
    DT::datatable(df_detail(), options = list(pageLength = 5, scrollX = TRUE))
  })
  
  output$bandbreitenTabelle <- DT::renderDT({
    DT::datatable(filteredBandbreiten(), options = list(dom = 't', pageLength = 5))
  })
  
  # Excel-Download
  output$download_excel <- downloadHandler(
    filename = function() paste0("Marktvergleich_", Sys.Date(), ".xlsx"),
    content = function(file) {
      openxlsx::write.xlsx(filteredMKT()[, c("Emittent","Laufzeit in Jahren",
                                             "AssetSwapSpreadMid","REFZS_SPREAD_SCD","Rendite")],
                           file, asTable = TRUE)
    }
  )
  
# PDF-Download
  output$download_pdf <- downloadHandler(
    filename    = function() paste0("DZ_HYP_Markt_Tabelle_", Sys.Date(), ".pdf"),
    contentType = "application/pdf",
    content = function(file) {
      
      dat <- filteredData()[, c("Emittent", "Laufzeit in Jahren",
                                "AssetSwapSpreadMid", "REFZS_SPREAD_SCD", "Rendite")]
      dat <- as.data.frame(dat)
  
      dat$Emittent <- stringr::str_wrap(dat$Emittent, width = 32)
      
      # Falls kein Ergebnis: leere PDF mit Hinweis
      if (nrow(dat) == 0) {
        pdf(file, width = 11.69, height = 8.27)  # A4 landscape (inches)
        grid::grid.newpage()
        grid::grid.text("Keine Daten für die gewählten Filter.", gp = grid::gpar(cex = 1.2))
        dev.off()
        return(invisible())
      }
      
      # Mehrseitig: in Blöcke aufteilen
      rows_per_page <- 30
      parts <- split(dat, (seq_len(nrow(dat)) - 1) %/% rows_per_page + 1L)
      
      # PDF starten (A4 landscape)
      pdf(file, width = 11.69, height = 8.27)  # 11.69x8.27 inches
      
      # dezentes Tabellen-Theme
      tt <- gridExtra::ttheme_minimal(
        base_size = 8,
        core = list(fg_params = list(cex = 0.8), padding = unit(c(3,3), "pt")),
        colhead = list(fg_params = list(fontface = "bold"))
      )
      
      # Seiten zeichnen
      for (i in seq_along(parts)) {
        grid::grid.newpage()
        
        title <- grid::textGrob(
          label = sprintf("DZ HYP vs. Markt – Detailtabelle (%s)   |   Seite %d/%d",
                          format(Sys.Date()), i, length(parts)),
          x = 0.5, y = 0.98, gp = grid::gpar(fontsize = 12, fontface = "bold")
        )
        
        tbl  <- gridExtra::tableGrob(parts[[i]], rows = NULL, theme = tt)
        
        page <- gridExtra::arrangeGrob(title, tbl, ncol = 1, heights = c(0.06, 0.94))
        grid::grid.draw(page)
      }
      
      dev.off()
    }
  )
  
  server_mkk_info <- function(input, output, session) {
    
    # Status je Produktart merken (damit beim Wechsel der Auswahl nichts verloren geht)
    checked_store <- reactiveValues()  # key = Produktart, value = character Vektor der abgehakten Schritte
    
    # gefilterte Schritte je Produktart
    mkk_steps <- reactive({
      req(input$mkk_produktart)
      mkk_data %>%
        filter(Produktart == input$mkk_produktart) %>%
        arrange(Reihenfolge)
    })
    
    # UI: Checkbox-Liste dynamisch rendern
    output$mkk_checklist_ui <- renderUI({
      steps <- mkk_steps()
      req(nrow(steps) > 0)
      
      sel <- isolate(checked_store[[input$mkk_produktart]])
      if (is.null(sel)) sel <- character(0)
      
      # Label = "1) Text", "2) Text", ...
      labels <- paste0(seq_len(nrow(steps)), ") ", steps$Schritt)
      
      tagList(
        tags$div(
          checkboxGroupInput(
            inputId = "mkk_checklist",
            label   = paste0("Schritte für: ", input$mkk_produktart),
            choices = setNames(steps$Schritt, labels),  # Werte = Schritt, Labels = nummeriert
            selected = sel,
            width = "100%"
          )
        ),
        tags$small(
          style = "color:#666;",
          sprintf("%d von %d Schritten abgehakt.",
                  length(sel), nrow(steps))
        )
      )
    })
    
    # beim Abhaken Status speichern
    observeEvent(input$mkk_checklist, {
      req(input$mkk_produktart)
      checked_store[[input$mkk_produktart]] <- input$mkk_checklist
    }, ignoreInit = TRUE)
    
    # Buttons: Alle markieren / Zurücksetzen
    observeEvent(input$mkk_select_all, {
      steps <- mkk_steps()
      updateCheckboxGroupInput(session, "mkk_checklist", selected = steps$Schritt)
      checked_store[[input$mkk_produktart]] <- steps$Schritt
    })
    
    observeEvent(input$mkk_clear_all, {
      updateCheckboxGroupInput(session, "mkk_checklist", selected = character(0))
      checked_store[[input$mkk_produktart]] <- character(0)
    })
    
    # Download Original-Excel
    output$download_mkk_excel <- downloadHandler(
      filename = function() "MKK_Erlaeuterung.xlsx",
      content = function(file) file.copy(mkk_excel_path, file, overwrite = TRUE)
    )
  }
  
  # Module-Server aufrufen
  server_mkk_info(input, output, session)
  mkk_server("mkk")
  yield_server("ycalc")
  
}
# Server

shinyApp(ui = ui, server = server)