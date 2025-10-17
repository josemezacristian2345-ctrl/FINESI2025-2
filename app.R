# Dynamic Time Warping Analyzer - Shiny App
# Instalar paquetes necesarios si no están instalados:
# install.packages(c("shiny", "shinydashboard", "ggplot2", "dtw", "plotly", "DT", "bslib", "scales"))

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dtw)
library(plotly)
library(DT)
library(scales)

# ============================================================================
# UI
# ============================================================================

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Dynamic Time Warping Analyzer", titleWidth = 350),
  
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      menuItem("Datos", tabName = "data", icon = icon("database")),
      menuItem("Parámetros DTW", tabName = "params", icon = icon("sliders")),
      menuItem("Resultados", tabName = "results", icon = icon("chart-line")),
      menuItem("Casos Especiales", tabName = "special", icon = icon("exclamation-triangle")),
      menuItem("Comparación Múltiple", tabName = "multiple", icon = icon("project-diagram"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #f4f6f9; }
        .box { box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
        .info-box { box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
      "))
    ),
    
    tabItems(
      # ===== TAB 1: DATOS =====
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            title = "Carga o Generación de Datos", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            
            radioButtons("data_source", "Fuente de datos:",
                         choices = c("Generar series sintéticas" = "synthetic",
                                     "Cargar CSV" = "upload"),
                         selected = "synthetic"),
            
            conditionalPanel(
              condition = "input.data_source == 'synthetic'",
              fluidRow(
                column(4, 
                       selectInput("series_type", "Tipo de serie:",
                                   choices = c("Sinusoide" = "sine",
                                               "Señal ruidosa" = "noisy",
                                               "Tendencia + Ruido" = "trend",
                                               "Serie financiera" = "financial"))
                ),
                column(4,
                       numericInput("n_points", "Número de puntos:", 
                                    value = 100, min = 20, max = 1000)
                ),
                column(4,
                       numericInput("n_series", "Número de series:", 
                                    value = 2, min = 2, max = 10)
                )
              ),
              actionButton("generate_data", "Generar Datos", 
                           icon = icon("magic"), 
                           class = "btn-success")
            ),
            
            conditionalPanel(
              condition = "input.data_source == 'upload'",
              fileInput("file_upload", "Cargar archivo CSV:",
                        accept = c(".csv", "text/csv")),
              helpText("El CSV debe tener series temporales en columnas")
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Previsualización de Series", 
            status = "info", 
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            plotlyOutput("preview_plot", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = "Selección de Series para DTW", 
            status = "warning", 
            solidHeader = TRUE,
            width = 6,
            selectInput("series1", "Serie 1 (Query):", choices = NULL),
            selectInput("series2", "Serie 2 (Reference):", choices = NULL)
          ),
          
          box(
            title = "Preprocesamiento", 
            status = "warning", 
            solidHeader = TRUE,
            width = 6,
            checkboxInput("normalize_data", "Normalizar series", value = FALSE),
            conditionalPanel(
              condition = "input.normalize_data == true",
              radioButtons("norm_method", "Método:",
                           choices = c("Z-score" = "zscore", 
                                       "Min-Max [0,1]" = "minmax"),
                           selected = "zscore")
            )
          )
        )
      ),
      
      # ===== TAB 2: PARÁMETROS DTW =====
      tabItem(
        tabName = "params",
        fluidRow(
          box(
            title = "Configuración de DTW", 
            status = "primary", 
            solidHeader = TRUE,
            width = 6,
            
            selectInput("distance_metric", "Métrica de distancia local:",
                        choices = c("Euclidiana" = "Euclidean",
                                    "Manhattan" = "Manhattan",
                                    "Chebyshev (máximo)" = "maximum",
                                    "Correlación" = "correlation",
                                    "Coseno" = "cosine"),
                        selected = "Euclidean"),
            
            checkboxInput("normalize_path", 
                          "Normalizar por longitud del camino", 
                          value = TRUE),
            
            checkboxInput("use_window", 
                          "Usar ventana de restricción (Sakoe-Chiba)", 
                          value = FALSE),
            
            conditionalPanel(
              condition = "input.use_window == true",
              sliderInput("window_size", "Tamaño de ventana:",
                          min = 1, max = 50, value = 10, step = 1)
            ),
            
            selectInput("step_pattern", "Patrón de paso:",
                        choices = c("Simétrico P2" = "symmetric2",
                                    "Simétrico P1" = "symmetric1",
                                    "Asimétrico" = "asymmetric"),
                        selected = "symmetric2")
          ),
          
          box(
            title = "Ejecutar Análisis", 
            status = "success", 
            solidHeader = TRUE,
            width = 6,
            
            actionButton("run_dtw", "Calcular DTW", 
                         icon = icon("play"), 
                         class = "btn-success btn-lg btn-block",
                         style = "margin-bottom: 20px;"),
            
            hr(),
            
            h4("Descripción de parámetros:"),
            p(strong("Métrica de distancia:"), 
              "Define cómo medir la diferencia entre puntos."),
            p(strong("Normalización:"), 
              "Ajusta la distancia por la longitud del camino de alineamiento."),
            p(strong("Ventana Sakoe-Chiba:"), 
              "Limita el warping a una banda diagonal, reduciendo complejidad."),
            p(strong("Patrón de paso:"), 
              "Define los movimientos permitidos en la matriz de costo.")
          )
        )
      ),
      
      # ===== TAB 3: RESULTADOS =====
      tabItem(
        tabName = "results",
        
        fluidRow(
          valueBoxOutput("dtw_distance_box", width = 4),
          valueBoxOutput("path_length_box", width = 4),
          valueBoxOutput("normalized_distance_box", width = 4)
        ),
        
        fluidRow(
          box(
            title = "Matriz de Costo Acumulado y Camino Óptimo", 
            status = "primary", 
            solidHeader = TRUE,
            width = 6,
            plotOutput("cost_matrix_plot", height = "450px")
          ),
          
          box(
            title = "Alineamiento Temporal", 
            status = "info", 
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("alignment_plot", height = "450px")
          )
        ),
        
        fluidRow(
          box(
            title = "Series Alineadas", 
            status = "success", 
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("aligned_series_plot", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = "Resumen de Resultados", 
            status = "warning", 
            solidHeader = TRUE,
            width = 12,
            DTOutput("results_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Exportar Resultados", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            downloadButton("download_results", "Descargar Resultados (CSV)"),
            downloadButton("download_plots", "Descargar Gráficos (PDF)")
          )
        )
      ),
      
      # ===== TAB 4: CASOS ESPECIALES =====
      tabItem(
        tabName = "special",
        
        fluidRow(
          box(
            title = "Diagnóstico de Datos", 
            status = "warning", 
            solidHeader = TRUE,
            width = 12,
            htmlOutput("data_diagnostics")
          )
        ),
        
        fluidRow(
          box(
            title = "Comparación de Métricas", 
            status = "info", 
            solidHeader = TRUE,
            width = 6,
            actionButton("compare_metrics", "Comparar Todas las Métricas", 
                         class = "btn-info"),
            br(), br(),
            plotlyOutput("metrics_comparison_plot", height = "350px")
          ),
          
          box(
            title = "Sensibilidad a Normalización", 
            status = "info", 
            solidHeader = TRUE,
            width = 6,
            actionButton("test_normalization", "Probar Normalizaciones", 
                         class = "btn-info"),
            br(), br(),
            plotlyOutput("normalization_comparison_plot", height = "350px")
          )
        ),
        
        fluidRow(
          box(
            title = "Efecto de la Ventana de Restricción", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            sliderInput("window_range", "Rango de ventanas a probar:",
                        min = 0, max = 100, value = c(0, 50), step = 5),
            actionButton("test_windows", "Analizar Efecto de Ventana", 
                         class = "btn-primary"),
            br(), br(),
            plotlyOutput("window_effect_plot", height = "350px")
          )
        )
      ),
      
      # ===== TAB 5: COMPARACIÓN MÚLTIPLE =====
      tabItem(
        tabName = "multiple",
        
        fluidRow(
          box(
            title = "Análisis de Múltiples Series", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            
            selectInput("reference_series", "Serie de referencia:", choices = NULL),
            actionButton("compare_all", "Comparar Todas vs Referencia", 
                         class = "btn-success btn-lg"),
            br(), br(),
            plotlyOutput("multiple_comparison_plot", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = "Matriz de Distancias DTW", 
            status = "info", 
            solidHeader = TRUE,
            width = 6,
            plotOutput("distance_matrix_plot", height = "450px")
          ),
          
          box(
            title = "Clustering Jerárquico", 
            status = "info", 
            solidHeader = TRUE,
            width = 6,
            plotOutput("dendrogram_plot", height = "450px")
          )
        ),
        
        fluidRow(
          box(
            title = "Tabla de Distancias", 
            status = "warning", 
            solidHeader = TRUE,
            width = 12,
            DTOutput("distance_table")
          )
        )
      )
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  
  # Reactive values
  rv <- reactiveValues(
    data = NULL,
    dtw_result = NULL,
    metrics_comparison = NULL,
    normalization_comparison = NULL,
    window_effect = NULL,
    multiple_comparison = NULL,
    distance_matrix = NULL
  )
  
  # ===== GENERACIÓN DE DATOS =====
  
  observeEvent(input$generate_data, {
    n <- input$n_points
    n_series <- input$n_series
    
    data_list <- lapply(1:n_series, function(i) {
      if (input$series_type == "sine") {
        freq <- runif(1, 0.5, 2)
        phase <- runif(1, 0, 2*pi)
        amp <- runif(1, 0.8, 1.2)
        sin(seq(0, 4*pi, length.out = n) * freq + phase) * amp
        
      } else if (input$series_type == "noisy") {
        signal <- sin(seq(0, 4*pi, length.out = n))
        signal + rnorm(n, 0, 0.3)
        
      } else if (input$series_type == "trend") {
        trend <- seq(0, 10, length.out = n)
        seasonal <- sin(seq(0, 4*pi, length.out = n)) * 2
        noise <- rnorm(n, 0, 0.5)
        trend + seasonal + noise
        
      } else { # financial
        cumsum(rnorm(n, 0.01, 1))
      }
    })
    
    rv$data <- as.data.frame(data_list)
    colnames(rv$data) <- paste0("Serie_", 1:n_series)
    
    updateSelectInput(session, "series1", choices = colnames(rv$data), selected = colnames(rv$data)[1])
    updateSelectInput(session, "series2", choices = colnames(rv$data), selected = colnames(rv$data)[2])
    updateSelectInput(session, "reference_series", choices = colnames(rv$data), selected = colnames(rv$data)[1])
  })
  
  # Cargar CSV
  observeEvent(input$file_upload, {
    req(input$file_upload)
    rv$data <- read.csv(input$file_upload$datapath)
    
    updateSelectInput(session, "series1", choices = colnames(rv$data), selected = colnames(rv$data)[1])
    updateSelectInput(session, "series2", choices = colnames(rv$data), selected = colnames(rv$data)[min(2, ncol(rv$data))])
    updateSelectInput(session, "reference_series", choices = colnames(rv$data), selected = colnames(rv$data)[1])
  })
  
  # ===== PREVIEW PLOT =====
  
  output$preview_plot <- renderPlotly({
    req(rv$data)
    
    data_long <- data.frame(
      Time = rep(1:nrow(rv$data), ncol(rv$data)),
      Value = unlist(rv$data),
      Series = rep(colnames(rv$data), each = nrow(rv$data))
    )
    
    p <- ggplot(data_long, aes(x = Time, y = Value, color = Series)) +
      geom_line(linewidth = 0.8) +
      theme_minimal() +
      labs(title = "Vista Previa de Todas las Series",
           x = "Tiempo", y = "Valor") +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  # ===== FUNCIONES AUXILIARES =====
  
  get_processed_series <- function(series_name) {
    req(rv$data, series_name)
    series <- rv$data[[series_name]]
    
    if (input$normalize_data) {
      if (input$norm_method == "zscore") {
        series <- scale(series)
      } else {
        series <- (series - min(series, na.rm = TRUE)) / 
          (max(series, na.rm = TRUE) - min(series, na.rm = TRUE))
      }
    }
    
    as.numeric(series)
  }
  
  get_distance_function <- function(metric) {
    switch(metric,
           "Euclidean" = function(x, y) sqrt(sum((x - y)^2)),
           "Manhattan" = function(x, y) sum(abs(x - y)),
           "maximum" = function(x, y) max(abs(x - y)),
           "correlation" = function(x, y) 1 - cor(x, y),
           "cosine" = function(x, y) 1 - sum(x*y) / (sqrt(sum(x^2)) * sqrt(sum(y^2))),
           function(x, y) sqrt(sum((x - y)^2)) # default
    )
  }
  
  # ===== CÁLCULO DTW =====
  
  observeEvent(input$run_dtw, {
    req(input$series1, input$series2)
    
    s1 <- get_processed_series(input$series1)
    s2 <- get_processed_series(input$series2)
    
    # Configurar parámetros
    step_pat <- switch(input$step_pattern,
                       "symmetric2" = symmetric2,
                       "symmetric1" = symmetric1,
                       "asymmetric" = asymmetric,
                       symmetric2)
    
    window_type <- if (input$use_window) "sakoechiba" else "none"
    window_size <- if (input$use_window) input$window_size else NULL
    
    # Calcular DTW
    rv$dtw_result <- dtw(s1, s2,
                         distance.only = FALSE,
                         step.pattern = step_pat,
                         window.type = window_type,
                         window.size = window_size,
                         dist.method = input$distance_metric)
  })
  
  # ===== VALUE BOXES =====
  
  output$dtw_distance_box <- renderValueBox({
    req(rv$dtw_result)
    
    dist <- if (input$normalize_path) {
      rv$dtw_result$normalizedDistance
    } else {
      rv$dtw_result$distance
    }
    
    valueBox(
      round(dist, 3),
      "Distancia DTW",
      icon = icon("ruler"),
      color = "blue"
    )
  })
  
  output$path_length_box <- renderValueBox({
    req(rv$dtw_result)
    
    valueBox(
      length(rv$dtw_result$index1),
      "Longitud del Camino",
      icon = icon("route"),
      color = "green"
    )
  })
  
  output$normalized_distance_box <- renderValueBox({
    req(rv$dtw_result)
    
    valueBox(
      round(rv$dtw_result$normalizedDistance, 3),
      "Distancia Normalizada",
      icon = icon("balance-scale"),
      color = "yellow"
    )
  })
  
  # ===== MATRIZ DE COSTO =====
  
  output$cost_matrix_plot <- renderPlot({
    req(rv$dtw_result)
    
    cost_matrix <- rv$dtw_result$costMatrix
    
    # Crear data frame para ggplot
    mat_df <- expand.grid(
      Query = 1:nrow(cost_matrix),
      Reference = 1:ncol(cost_matrix)
    )
    mat_df$Cost <- as.vector(cost_matrix)
    
    # Camino óptimo
    path_df <- data.frame(
      Query = rv$dtw_result$index1,
      Reference = rv$dtw_result$index2
    )
    
    ggplot(mat_df, aes(x = Reference, y = Query, fill = Cost)) +
      geom_tile() +
      geom_path(data = path_df, aes(x = Reference, y = Query), 
                color = "red", linewidth = 1, inherit.aes = FALSE) +
      geom_point(data = path_df, aes(x = Reference, y = Query), 
                 color = "red", size = 0.5, inherit.aes = FALSE) +
      scale_fill_gradient(low = "white", high = "darkblue") +
      theme_minimal() +
      labs(title = "Matriz de Costo Acumulado con Camino Óptimo",
           x = "Serie 2 (Reference)", y = "Serie 1 (Query)") +
      coord_fixed()
  })
  
  # ===== ALINEAMIENTO =====
  
  output$alignment_plot <- renderPlotly({
    req(rv$dtw_result)
    
    path_df <- data.frame(
      Index1 = rv$dtw_result$index1,
      Index2 = rv$dtw_result$index2
    )
    
    p <- ggplot(path_df, aes(x = Index2, y = Index1)) +
      geom_path(color = "steelblue", linewidth = 1) +
      geom_point(size = 1, alpha = 0.5) +
      theme_minimal() +
      labs(title = "Camino de Alineamiento Temporal",
           x = "Índice en Serie 2 (Reference)",
           y = "Índice en Serie 1 (Query)") +
      coord_fixed()
    
    ggplotly(p)
  })
  
  # ===== SERIES ALINEADAS =====
  
  output$aligned_series_plot <- renderPlotly({
    req(rv$dtw_result, input$series1, input$series2)
    
    s1 <- get_processed_series(input$series1)
    s2 <- get_processed_series(input$series2)
    
    # Crear índices comunes para el alineamiento
    aligned_time <- 1:length(rv$dtw_result$index1)
    
    aligned_df <- data.frame(
      Time = rep(aligned_time, 2),
      Value = c(s1[rv$dtw_result$index1], s2[rv$dtw_result$index2]),
      Series = rep(c(input$series1, input$series2), each = length(aligned_time))
    )
    
    p <- ggplot(aligned_df, aes(x = Time, y = Value, color = Series)) +
      geom_line(linewidth = 1) +
      theme_minimal() +
      labs(title = "Series Temporales Alineadas por DTW",
           x = "Tiempo Alineado", y = "Valor") +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  # ===== TABLA DE RESULTADOS =====
  
  output$results_table <- renderDT({
    req(rv$dtw_result)
    
    results_df <- data.frame(
      Parámetro = c("Distancia Total", "Distancia Normalizada", 
                    "Longitud del Camino", "Métrica", "Patrón de Paso",
                    "Ventana Usada", "Normalización de Datos"),
      Valor = c(
        round(rv$dtw_result$distance, 4),
        round(rv$dtw_result$normalizedDistance, 4),
        length(rv$dtw_result$index1),
        input$distance_metric,
        input$step_pattern,
        if (input$use_window) paste0("Sí (", input$window_size, ")") else "No",
        if (input$normalize_data) input$norm_method else "No"
      )
    )
    
    datatable(results_df, 
              options = list(dom = 't', pageLength = 10),
              rownames = FALSE)
  })
  
  # ===== DIAGNÓSTICOS =====
  
  output$data_diagnostics <- renderUI({
    req(rv$data, input$series1, input$series2)
    
    s1 <- rv$data[[input$series1]]
    s2 <- rv$data[[input$series2]]
    
    diagnostics <- list()
    
    # Longitud
    if (length(s1) != length(s2)) {
      diagnostics <- c(diagnostics, 
                       sprintf("<div class='alert alert-warning'><strong>⚠ Longitudes diferentes:</strong> Serie 1 tiene %d puntos, Serie 2 tiene %d puntos</div>", 
                               length(s1), length(s2)))
    } else {
      diagnostics <- c(diagnostics, 
                       sprintf("<div class='alert alert-success'><strong>✓ Longitudes iguales:</strong> Ambas series tienen %d puntos</div>", 
                               length(s1)))
    }
    
    # Valores NA
    na1 <- sum(is.na(s1))
    na2 <- sum(is.na(s2))
    if (na1 > 0 || na2 > 0) {
      diagnostics <- c(diagnostics,
                       sprintf("<div class='alert alert-danger'><strong>✗ Valores perdidos:</strong> Serie 1: %d NAs, Serie 2: %d NAs</div>",
                               na1, na2))
    } else {
      diagnostics <- c(diagnostics,
                       "<div class='alert alert-success'><strong>✓ Sin valores perdidos</strong></div>")
    }
    
    # Escalas
    range1 <- max(s1, na.rm = TRUE) - min(s1, na.rm = TRUE)
    range2 <- max(s2, na.rm = TRUE) - min(s2, na.rm = TRUE)
    ratio <- max(range1, range2) / min(range1, range2)
    
    if (ratio > 2) {
      diagnostics <- c(diagnostics,
                       sprintf("<div class='alert alert-warning'><strong>⚠ Escalas muy diferentes:</strong> Ratio de rangos = %.2f. Considere normalizar.</div>",
                               ratio))
    } else {
      diagnostics <- c(diagnostics,
                       "<div class='alert alert-success'><strong>✓ Escalas similares</strong></div>")
    }
    
    HTML(paste(diagnostics, collapse = ""))
  })
  
  # ===== COMPARACIÓN DE MÉTRICAS =====
  
  observeEvent(input$compare_metrics, {
    req(input$series1, input$series2)
    
    s1 <- get_processed_series(input$series1)
    s2 <- get_processed_series(input$series2)
    
    metrics <- c("Euclidean", "Manhattan", "maximum")
    
    results <- sapply(metrics, function(m) {
      d <- dtw(s1, s2, dist.method = m, distance.only = TRUE)
      d$normalizedDistance
    })
    
    rv$metrics_comparison <- data.frame(
      Metric = metrics,
      Distance = results
    )
  })
  
  output$metrics_comparison_plot <- renderPlotly({
    req(rv$metrics_comparison)
    
    p <- ggplot(rv$metrics_comparison, aes(x = Metric, y = Distance, fill = Metric)) +
      geom_col() +
      geom_text(aes(label = round(Distance, 3)), vjust = -0.5) +
      theme_minimal() +
      labs(title = "Comparación de Distancias DTW por Métrica",
           x = "Métrica", y = "Distancia Normalizada") +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  # ===== COMPARACIÓN NORMALIZACIÓN =====
  
  observeEvent(input$test_normalization, {
    req(input$series1, input$series2, rv$data)
    
    s1_raw <- rv$data[[input$series1]]
    s2_raw <- rv$data[[input$series2]]
    
    # Sin normalización
    d_none <- dtw(s1_raw, s2_raw, distance.only = TRUE)$normalizedDistance
    
    # Z-score
    s1_z <- scale(s1_raw)
    s2_z <- scale(s2_raw)
    d_zscore <- dtw(s1_z, s2_z, distance.only = TRUE)$normalizedDistance
    
    # Min-Max
    s1_mm <- (s1_raw - min(s1_raw)) / (max(s1_raw) - min(s1_raw))
    s2_mm <- (s2_raw - min(s2_raw)) / (max(s2_raw) - min(s2_raw))
    d_minmax <- dtw(s1_mm, s2_mm, distance.only = TRUE)$normalizedDistance
    
    rv$normalization_comparison <- data.frame(
      Method = c("Sin normalizar", "Z-score", "Min-Max"),
      Distance = c(d_none, d_zscore, d_minmax)
    )
  })
  
  output$normalization_comparison_plot <- renderPlotly({
    req(rv$normalization_comparison)
    
    p <- ggplot(rv$normalization_comparison, aes(x = Method, y = Distance, fill = Method)) +
      geom_col() +
      geom_text(aes(label = round(Distance, 3)), vjust = -0.5) +
      theme_minimal() +
      labs(title = "Efecto de la Normalización en DTW",
           x = "Método de Normalización", y = "Distancia Normalizada") +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  # ===== EFECTO VENTANA =====
  
  observeEvent(input$test_windows, {
    req(input$series1, input$series2)
    
    s1 <- get_processed_series(input$series1)
    s2 <- get_processed_series(input$series2)
    
    windows <- seq(input$window_range[1], input$window_range[2], by = 5)
    
    results <- sapply(windows, function(w) {
      if (w == 0) {
        d <- dtw(s1, s2, window.type = "none", distance.only = TRUE)
      } else {
        d <- dtw(s1, s2, window.type = "sakoechiba", 
                 window.size = w, distance.only = TRUE)
      }
      d$normalizedDistance
    })
    
    rv$window_effect <- data.frame(
      WindowSize = windows,
      Distance = results
    )
  })
  
  output$window_effect_plot <- renderPlotly({
    req(rv$window_effect)
    
    p <- ggplot(rv$window_effect, aes(x = WindowSize, y = Distance)) +
      geom_line(color = "steelblue", linewidth = 1) +
      geom_point(size = 2) +
      theme_minimal() +
      labs(title = "Efecto del Tamaño de Ventana en la Distancia DTW",
           x = "Tamaño de Ventana Sakoe-Chiba", 
           y = "Distancia Normalizada")
    
    ggplotly(p)
  })
  
  # ===== COMPARACIÓN MÚLTIPLE =====
  
  observeEvent(input$compare_all, {
    req(rv$data, input$reference_series)
    
    ref_series <- get_processed_series(input$reference_series)
    series_names <- colnames(rv$data)
    
    distances <- sapply(series_names, function(sname) {
      if (sname == input$reference_series) return(0)
      s <- get_processed_series(sname)
      d <- dtw(ref_series, s, distance.only = TRUE)
      d$normalizedDistance
    })
    
    rv$multiple_comparison <- data.frame(
      Series = series_names,
      Distance = distances
    )
    
    # Calcular matriz de distancias completa
    n_series <- ncol(rv$data)
    dist_matrix <- matrix(0, n_series, n_series)
    
    for (i in 1:(n_series-1)) {
      for (j in (i+1):n_series) {
        si <- get_processed_series(colnames(rv$data)[i])
        sj <- get_processed_series(colnames(rv$data)[j])
        d <- dtw(si, sj, distance.only = TRUE)$normalizedDistance
        dist_matrix[i, j] <- d
        dist_matrix[j, i] <- d
      }
    }
    
    rownames(dist_matrix) <- colnames(rv$data)
    colnames(dist_matrix) <- colnames(rv$data)
    rv$distance_matrix <- dist_matrix
  })
  
  output$multiple_comparison_plot <- renderPlotly({
    req(rv$multiple_comparison)
    
    # Ordenar por distancia
    df <- rv$multiple_comparison[order(rv$multiple_comparison$Distance), ]
    df$Series <- factor(df$Series, levels = df$Series)
    
    p <- ggplot(df, aes(x = Series, y = Distance, fill = Distance)) +
      geom_col() +
      scale_fill_gradient(low = "lightblue", high = "darkred") +
      theme_minimal() +
      labs(title = paste("Distancia DTW respecto a", input$reference_series),
           x = "Serie", y = "Distancia Normalizada") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # ===== MATRIZ DE DISTANCIAS =====
  
  output$distance_matrix_plot <- renderPlot({
    req(rv$distance_matrix)
    
    mat <- rv$distance_matrix
    mat_df <- expand.grid(
      Series1 = rownames(mat),
      Series2 = colnames(mat)
    )
    mat_df$Distance <- as.vector(mat)
    mat_df$Series1 <- factor(mat_df$Series1, levels = rownames(mat))
    mat_df$Series2 <- factor(mat_df$Series2, levels = colnames(mat))
    
    ggplot(mat_df, aes(x = Series2, y = Series1, fill = Distance)) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(Distance, 2)), size = 3) +
      scale_fill_gradient(low = "white", high = "darkblue") +
      theme_minimal() +
      labs(title = "Matriz de Distancias DTW",
           x = "", y = "") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      coord_fixed()
  })
  
  # ===== DENDROGRAMA =====
  
  output$dendrogram_plot <- renderPlot({
    req(rv$distance_matrix)
    
    # Convertir a objeto dist
    dist_obj <- as.dist(rv$distance_matrix)
    
    # Clustering jerárquico
    hc <- hclust(dist_obj, method = "complete")
    
    plot(hc, main = "Clustering Jerárquico basado en DTW",
         xlab = "Series Temporales", ylab = "Distancia DTW",
         sub = "", hang = -1)
    
    # Añadir rectángulos para grupos
    rect.hclust(hc, k = min(3, nrow(rv$distance_matrix)), 
                border = c("red", "blue", "green"))
  })
  
  # ===== TABLA DE DISTANCIAS =====
  
  output$distance_table <- renderDT({
    req(rv$distance_matrix)
    
    mat <- rv$distance_matrix
    df <- as.data.frame(mat)
    df <- round(df, 4)
    
    datatable(df, 
              options = list(pageLength = 10, scrollX = TRUE),
              caption = "Matriz de distancias DTW entre todas las series")
  })
  
  # ===== DESCARGAS =====
  
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("dtw_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(rv$dtw_result)
      
      results_df <- data.frame(
        Parameter = c("Total Distance", "Normalized Distance", 
                      "Path Length", "Distance Metric", "Step Pattern",
                      "Window Used", "Window Size", "Data Normalization"),
        Value = c(
          rv$dtw_result$distance,
          rv$dtw_result$normalizedDistance,
          length(rv$dtw_result$index1),
          input$distance_metric,
          input$step_pattern,
          input$use_window,
          ifelse(input$use_window, input$window_size, NA),
          ifelse(input$normalize_data, input$norm_method, "None")
        )
      )
      
      # Agregar el camino de alineamiento
      path_df <- data.frame(
        Index1 = rv$dtw_result$index1,
        Index2 = rv$dtw_result$index2
      )
      
      # Escribir ambas tablas al CSV
      write.csv(results_df, file, row.names = FALSE)
      write.table("\n\nAlignment Path:", file, append = TRUE, 
                  row.names = FALSE, col.names = FALSE, quote = FALSE)
      write.table(path_df, file, append = TRUE, 
                  row.names = FALSE, sep = ",")
    }
  )
  
  output$download_plots <- downloadHandler(
    filename = function() {
      paste0("dtw_plots_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      req(rv$dtw_result, input$series1, input$series2)
      
      pdf(file, width = 11, height = 8)
      
      # Plot 1: Series originales
      s1 <- get_processed_series(input$series1)
      s2 <- get_processed_series(input$series2)
      
      par(mfrow = c(2, 2))
      
      plot(s1, type = "l", col = "blue", lwd = 2,
           main = "Series Originales",
           xlab = "Tiempo", ylab = "Valor")
      lines(s2, col = "red", lwd = 2)
      legend("topright", legend = c(input$series1, input$series2),
             col = c("blue", "red"), lwd = 2)
      
      # Plot 2: Camino de alineamiento
      plot(rv$dtw_result$index2, rv$dtw_result$index1,
           type = "l", col = "steelblue", lwd = 2,
           main = "Camino de Alineamiento",
           xlab = paste("Índice en", input$series2),
           ylab = paste("Índice en", input$series1))
      
      # Plot 3: Matriz de costo
      cost_matrix <- rv$dtw_result$costMatrix
      image(t(cost_matrix[nrow(cost_matrix):1, ]), 
            col = heat.colors(100),
            main = "Matriz de Costo Acumulado",
            xlab = input$series2, ylab = input$series1)
      lines(rv$dtw_result$index2 / ncol(cost_matrix), 
            1 - rv$dtw_result$index1 / nrow(cost_matrix),
            col = "blue", lwd = 2)
      
      # Plot 4: Series alineadas
      aligned_time <- 1:length(rv$dtw_result$index1)
      plot(aligned_time, s1[rv$dtw_result$index1],
           type = "l", col = "blue", lwd = 2,
           main = "Series Alineadas",
           xlab = "Tiempo Alineado", ylab = "Valor",
           ylim = range(c(s1, s2)))
      lines(aligned_time, s2[rv$dtw_result$index2], 
            col = "red", lwd = 2)
      legend("topright", legend = c(input$series1, input$series2),
             col = c("blue", "red"), lwd = 2)
      
      dev.off()
    }
  )
}

# ============================================================================
# RUN APP
# ============================================================================

shinyApp(ui = ui, server = server)