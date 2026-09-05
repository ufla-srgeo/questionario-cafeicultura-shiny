library(shiny)
library(shinythemes)
library(leaflet)
library(sf)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(rlang)
library(stringr)
library(fmsb)       # radar chart
library(tidyr)
library(scales)

# ------------------------------
# Load and validate spatial data
# ------------------------------
rds_path <- "lavouras_completas.rds"

if (!file.exists(rds_path)) stop("RDS file not found.")

dados_sp <- readRDS(rds_path)

dados_sp <- dados_sp %>%
  mutate(
    Cultivar        = str_replace_all(Cultivar, '[\\"]', '') %>% str_trim(),
    Area_ha         = as.numeric(Area_ha),
    Ano_Implantacao = as.integer(Ano_Implantacao),
    Numero_Linhas   = as.integer(Numero_Linhas)
  )

if (!"Lavoura" %in% names(dados_sp)) dados_sp$Lavoura <- dados_sp$FID

# --- 1. Primeira validação e correção ---
if (!all(st_is_valid(dados_sp))) {
  message("Aplicando st_make_valid para corrigir geometrias inválidas...")
  dados_sp <- st_make_valid(dados_sp)
}

# Transformar para WGS84 (EPSG:4326)
if (is.na(st_crs(dados_sp)$epsg) || st_crs(dados_sp)$epsg != 4326) {
  dados_sp <- st_transform(dados_sp, 4326)
  # --- 2. Segunda validação após transformação ---
  dados_sp <- st_make_valid(dados_sp)
}

# (Opcional) Simplificar geometrias com tolerância muito pequena para remover micro-detalhes
# dados_sp <- st_simplify(dados_sp, dTolerance = 0.00001, preserveTopology = TRUE)

# --- 3. Cálculo robusto do centro médio ---
centro_medio_geo <- suppressWarnings({
  geom_valid <- st_make_valid(st_geometry(dados_sp))
  safe_centroid <- tryCatch(
    st_centroid(geom_valid),
    error = function(e) {
      # Se st_centroid falhar, tenta st_point_on_surface
      tryCatch(
        st_point_on_surface(geom_valid),
        error = function(e2) {
          # Fallback: usa o bounding box geral
          bbox <- st_bbox(geom_valid)
          st_sfc(st_point(c(mean(bbox[c("xmin","xmax")]), 
                            mean(bbox[c("ymin","ymax")]))), crs = 4326)
        }
      )
    }
  )
  # Se ainda assim for vazio, usa centro do bbox
  if (length(safe_centroid) == 0 || is.null(safe_centroid)) {
    bbox <- st_bbox(geom_valid)
    safe_centroid <- st_sfc(st_point(c(mean(bbox[c("xmin","xmax")]), 
                                       mean(bbox[c("ymin","ymax")]))), crs = 4326)
  }
  st_coordinates(safe_centroid) %>%
    as.data.frame() %>%
    summarise(X = mean(X), Y = mean(Y))
})

# Institutional coffee color palette
COR_PRIMARIA  <- "#4E342E"   # dark coffee
COR_SECUNDARIA <- "#A5D6A7"  # leaf green
COR_ACENTO    <- "#FF8F00"   # harvest amber
COR_FUNDO     <- "#FFF8F0"   # cream

# ------------------------------
# UI
# ------------------------------
# ------------------------------
# UI
# ------------------------------
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  tags$head(tags$style(HTML(paste0("
    body { background-color: ", COR_FUNDO, "; font-family: 'Segoe UI', sans-serif; }
    .navbar-default { background-color: ", COR_PRIMARIA, " !important; border: none; }
    .navbar-default .navbar-brand, .navbar-default .navbar-nav > li > a { color: #fff !important; }
    .navbar-default .navbar-nav > .active > a { background-color: ", COR_ACENTO, " !important; color: #fff !important; }
    .well { background-color: #fff; border: 1px solid #e0d5cc; border-radius: 8px; }
    .metric-box { background: #fff; border-left: 5px solid ", COR_ACENTO, ";
      border-radius: 6px; padding: 14px 18px; margin-bottom: 12px;
      box-shadow: 0 2px 6px rgba(0,0,0,0.07); }
    .metric-box .val { font-size: 2rem; font-weight: 700; color: ", COR_PRIMARIA, "; }
    .metric-box .lbl { font-size: 0.82rem; color: #888; text-transform: uppercase; letter-spacing: .05em; }
    h3.secao { color: ", COR_PRIMARIA, "; border-bottom: 2px solid ", COR_ACENTO, ";
      padding-bottom: 6px; margin-bottom: 16px; }
    .btn-primary { background-color: ", COR_PRIMARIA, " !important; border-color: ", COR_PRIMARIA, " !important; }
    .btn-primary:hover { background-color: ", COR_ACENTO, " !important; border-color: ", COR_ACENTO, " !important; }
  ")))),
  
  navbarPage(
    # Nome limpo para a aba do navegador
    windowTitle = "Geocomputation - Coffee Crops | UFLA",
    
    # Título estilizado para o cabeçalho da aplicação
    title = div(style = paste0("color: #fff; font-weight:700; font-size:1.2rem;"),
                "☕ Geocomputation — Coffee Crops | UFLA"),
  
    
    # ── TAB 1: INTERACTIVE MAP ──────────────────────────────────────────────
    tabPanel("🗺️ Interactive Map",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 wellPanel(
                   h4("Settings", style = paste0("color:", COR_PRIMARIA, ";")),
                   selectInput("var_color", "Color by:",
                               choices = c("Area (ha)" = "Area_ha",
                                           "Cultivar" = "Cultivar",
                                           "Planting Year" = "Ano_Implantacao",
                                           "Number of Lines" = "Numero_Linhas",
                                           "Age" = "Idade",
                                           "Spacing" = "Espacamento",
                                           "Responsible" = "Responsavel"),
                               selected = "Cultivar"),
                   selectInput("filtro_cat", "Filter by:",
                               choices = c("None" = "Nenhum",
                                           "Cultivar" = "Cultivar",
                                           "Spacing" = "Espacamento",
                                           "Responsible" = "Responsavel"),
                               selected = "Nenhum"),
                   uiOutput("filtro_valor"),
                   hr(),
                   selectInput("selecionar_lavoura", "Zoom to Crop:",
                               choices = sort(unique(dados_sp$Lavoura)), selected = NULL),
                   actionButton("zoom_lavoura", "📍 Center", class = "btn-primary", width = "100%")
                 ),
                 wellPanel(
                   h4("Metrics", style = paste0("color:", COR_PRIMARIA, ";")),
                   tableOutput("metricas_banco")
                 )
               ),
               mainPanel(
                 width = 9,
                 leafletOutput("mapa", height = "460px"),
                 br(),
                 fluidRow(
                   column(6, h4("Distribution of Selected Attribute"), plotOutput("grafico_dinamico", height = "280px")),
                   column(6, h4("Polygon Inspection (click on map)"), verbatimTextOutput("info_clique"))
                 )
               )
             )
    ),
    
    # ── TAB 2: COMPARATIVE PANEL ────────────────────────────────────────────
    tabPanel("📊 Comparative Panel",
             fluidPage(
               br(),
               fluidRow(
                 column(3,
                        wellPanel(
                          h4("Select Crops", style = paste0("color:", COR_PRIMARIA, ";")),
                          checkboxGroupInput("lav_comp",
                                             label = "Crops to compare:",
                                             choices  = sort(unique(dados_sp$Lavoura)),
                                             selected = head(sort(unique(dados_sp$Lavoura)), 4)
                          )
                        )
                 ),
                 column(9,
                        # Summary KPIs
                        h3("Summary of Selected Group", class = "secao"),
                        fluidRow(
                          column(3, uiOutput("kpi_n")),
                          column(3, uiOutput("kpi_area")),
                          column(3, uiOutput("kpi_cultivar")),
                          column(3, uiOutput("kpi_idade"))
                        ),
                        hr(),
                        
                        # Side-by-side plots
                        h3("Comparative Quantitative Attributes", class = "secao"),
                        fluidRow(
                          column(6, plotOutput("comp_area",    height = "280px")),
                          column(6, plotOutput("comp_linhas",  height = "280px"))
                        ),
                        fluidRow(
                          column(6, plotOutput("comp_idade",   height = "280px")),
                          column(6, plotOutput("comp_ano",     height = "280px"))
                        ),
                        hr(),
                        
                        # Cultivar and spacing composition
                        h3("Composition by Cultivar and Spacing", class = "secao"),
                        fluidRow(
                          column(6, plotOutput("comp_cultivar",    height = "300px")),
                          column(6, plotOutput("comp_espacamento", height = "300px"))
                        ),
                        hr(),
                        
                        # Temporal evolution
                        h3("Temporal Evolution of Planting", class = "secao"),
                        plotOutput("comp_temporal", height = "300px"),
                        hr(),
                        
                        # Radar chart
                        h3("Multivariate Profile of Crops (Radar)", class = "secao"),
                        plotOutput("comp_radar", height = "400px"),
                        hr(),
                        
                        # Comparative table
                        h3("Detailed Comparative Table", class = "secao"),
                        tableOutput("tabela_comp")
                 )
               )
             )
    ),
    
    # ── TAB 3: BY CULTIVAR ──────────────────────────────────────────────────
    tabPanel("🌿 By Cultivar",
             fluidPage(
               br(),
               h3("Aggregated Analysis by Cultivar", class = "secao"),
               fluidRow(
                 column(6, plotOutput("cult_area_total",  height = "300px")),
                 column(6, plotOutput("cult_n_talhoes",   height = "300px"))
               ),
               fluidRow(
                 column(6, plotOutput("cult_box_area",    height = "300px")),
                 column(6, plotOutput("cult_box_idade",   height = "300px"))
               ),
               hr(),
               h3("Summary Table by Cultivar", class = "secao"),
               tableOutput("tabela_cultivar")
             )
    ),
    
    # ── TAB 4: TEMPORAL ──────────────────────────────────────────────────────
    tabPanel("📅 Temporal",
             fluidPage(
               br(),
               h3("Plantings Over Time", class = "secao"),
               plotOutput("temp_implantacao", height = "320px"),
               hr(),
               h3("Cumulative Planted Area by Year", class = "secao"),
               plotOutput("temp_area_acum",   height = "320px"),
               hr(),
               h3("Area by Year and Cultivar", class = "secao"),
               plotOutput("temp_cultivar_ano", height = "360px")
             )
    )
  )
)

# ------------------------------
# SERVER
# ------------------------------
server <- function(input, output, session) {
  
  # ── Filtered data (map tab) ──────────────────────────────────────────────
  dados_filtrados <- reactive({
    df <- dados_sp
    if (input$filtro_cat != "Nenhum" &&
        !is.null(input$filtro_valor) &&
        input$filtro_valor != "Todos") {
      coluna <- sym(input$filtro_cat)
      df <- df %>% filter(!!coluna == input$filtro_valor)
    }
    df
  })
  
  # Data for comparative panel
  dados_comp <- reactive({
    req(input$lav_comp)
    dados_sp %>% filter(Lavoura %in% input$lav_comp)
  })
  
  # ── Dynamic filter UI ─────────────────────────────────────────────────────
  observeEvent(input$filtro_cat, {
    if (input$filtro_cat == "Nenhum") {
      output$filtro_valor <- renderUI(NULL)
    } else {
      vals <- sort(unique(dados_sp[[input$filtro_cat]]))
      vals <- vals[!is.na(vals)]
      output$filtro_valor <- renderUI(
        selectInput("filtro_valor",
                    paste("Value of", input$filtro_cat),
                    choices = c("All" = "Todos", vals), selected = "Todos")
      )
    }
  })
  
  # ── Metrics table ──────────────────────────────────────────────────────────
  output$metricas_banco <- renderTable({
    df <- dados_filtrados()
    data.frame(
      Indicator = c("Plots", "Total Area (ha)", "NAs"),
      Value = c(nrow(df),
                round(sum(df$Area_ha, na.rm = TRUE), 2),
                sum(is.na(df[[input$var_color]])))
    )
  }, colnames = FALSE, striped = TRUE, spacing = "s")
  
  # ── Color palette ──────────────────────────────────────────────────────────
  paleta_cores <- reactive({
    var    <- input$var_color
    dados  <- dados_filtrados()
    valores <- dados[[var]]
    if (is.numeric(valores)) {
      colorNumeric(palette = "YlOrRd", domain = valores, na.color = "gray")
    } else {
      niveis <- unique(valores[!is.na(valores)])
      n  <- length(niveis)
      if (n == 0) return(colorFactor("gray", domain = valores))
      pal <- if (n <= 2) c("#66C2A5","#FC8D62") else brewer.pal(min(n,9),"Set3")
      if (n > 9) pal <- colorRampPalette(pal)(n)
      colorFactor(palette = pal, domain = valores, na.color = "gray")
    }
  })
  
  # ── Map ────────────────────────────────────────────────────────────────────
  output$mapa <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite (Esri)") %>%
      setView(lng = centro_medio_geo$X, lat = centro_medio_geo$Y, zoom = 15) %>%
      addLayersControl(baseGroups = c("OpenStreetMap","Satellite (Esri)"),
                       options = layersControlOptions(collapsed = FALSE))
  })
  
  observe({
    req(dados_filtrados())
    pal <- paleta_cores()
    var <- input$var_color
    leafletProxy("mapa", data = dados_filtrados()) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~pal(get(var)), weight = 1.5, color = "black", fillOpacity = 0.6,
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
        label = ~paste("Crop:", Lavoura), layerId = ~Lavoura,
        popup = ~paste(
          "<b>ID:</b>", Lavoura, "<br>",
          "<b>Area (ha):</b>", round(Area_ha,3), "<br>",
          "<b>Cultivar:</b>", Cultivar, "<br>",
          "<b>Year:</b>", Ano_Implantacao, "<br>",
          "<b>Age:</b>", Idade, "<br>",
          "<b>Spacing:</b>", Espacamento, "<br>",
          "<b>Responsible:</b>", Responsavel)
      ) %>%
      clearControls() %>%
      addLegend("bottomright", pal = pal,
                values = dados_filtrados()[[var]],
                title = input$var_color, opacity = 0.7)
  })
  
  observeEvent(input$zoom_lavoura, {
    req(input$selecionar_lavoura)
    lav <- dados_sp[dados_sp$Lavoura == input$selecionar_lavoura, ]
    if (nrow(lav) > 0) {
      # Garantir geometria válida antes de st_point_on_surface
      geom <- st_geometry(lav)
      if (!st_is_valid(geom)) geom <- st_make_valid(geom)
      coords <- suppressWarnings(st_coordinates(st_point_on_surface(geom)))
      leafletProxy("mapa") %>% setView(lng = coords[1], lat = coords[2], zoom = 18)
    }
  })
  
  output$grafico_dinamico <- renderPlot({
    df  <- dados_filtrados()
    var <- input$var_color
    
    # 1. Validação de dados
    req(var, df)
    
    valores <- df[[var]]
    
    # 2. Mensagem personalizada para variáveis categóricas/texto (em inglês)
    validate(
      need(
        is.numeric(valores),
        paste0("ℹ️ The variable '", var, "' is categorical/qualitative.\n\n",
               "Please select a numeric variable (e.g., Area, Planting Year, Number of Lines, or Age) ",
               "to view the distribution plot in this section.")
      )
    )
    
    # 3. Gráfico exibido Apenas quando for numérico
    ggplot(df, aes(x = factor(Lavoura), y = .data[[var]], fill = .data[[var]])) +
      geom_bar(stat = "identity", color = "black", width = 0.7) +
      scale_fill_viridis_c(name = var) +
      labs(x = "Crop / Plot", y = var) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  output$info_clique <- renderPrint({
    ev <- input$mapa_shape_click
    if (is.null(ev)) return(cat("Click a polygon to inspect."))
    lav <- dados_sp[dados_sp$Lavoura == ev$id, ]
    print(glimpse(st_drop_geometry(lav)))
  })
  
  # ═══════════════════════════════════════════════════════════════════════════
  # COMPARATIVE TAB
  # ═══════════════════════════════════════════════════════════════════════════
  
  # KPIs
  kpi_box <- function(val, lbl)
    div(class = "metric-box",
        div(class = "val", val),
        div(class = "lbl", lbl))
  
  output$kpi_n        <- renderUI(kpi_box(nrow(dados_comp()), "Selected Plots"))
  output$kpi_area     <- renderUI(kpi_box(
    paste0(round(sum(dados_comp()$Area_ha, na.rm=TRUE),2)," ha"), "Total Area"))
  output$kpi_cultivar <- renderUI(kpi_box(
    length(unique(dados_comp()$Cultivar)), "Cultivars"))
  output$kpi_idade    <- renderUI({
    id <- suppressWarnings(as.numeric(dados_comp()$Idade))
    kpi_box(paste0(round(mean(id, na.rm=TRUE),1)," years"), "Average Age")
  })
  
  # Helper bar plot
  gg_barra <- function(df, col, titulo, ylab, fill_col = COR_PRIMARIA) {
    ggplot(df, aes(x = factor(Lavoura), y = .data[[col]], fill = factor(Lavoura))) +
      geom_bar(stat = "identity", color = "white", width = 0.7) +
      geom_text(aes(label = round(.data[[col]], 2)),
                vjust = -0.4, size = 3, fontface = "bold") +
      scale_fill_brewer(palette = "Set3") +
      labs(title = titulo, x = "Crop", y = ylab) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none",
            plot.title = element_text(color = COR_PRIMARIA, face = "bold"),
            axis.text.x = element_text(angle = 30, hjust = 1))
  }
  
  output$comp_area   <- renderPlot({
    df <- st_drop_geometry(dados_comp())
    gg_barra(df, "Area_ha", "Area per Crop", "Area (ha)")
  })
  
  output$comp_linhas <- renderPlot({
    df <- st_drop_geometry(dados_comp())
    gg_barra(df, "Numero_Linhas", "Number of Lines", "Lines")
  })
  
  output$comp_idade  <- renderPlot({
    df <- st_drop_geometry(dados_comp()) %>%
      mutate(Idade_num = suppressWarnings(as.numeric(Idade)))
    gg_barra(df, "Idade_num", "Age of Crops", "Years")
  })
  
  output$comp_ano    <- renderPlot({
    df <- st_drop_geometry(dados_comp())
    gg_barra(df, "Ano_Implantacao", "Planting Year", "Year")
  })
  
  output$comp_cultivar <- renderPlot({
    df <- st_drop_geometry(dados_comp())
    ggplot(df, aes(x = Cultivar, fill = factor(Lavoura))) +
      geom_bar(color = "white", position = "dodge") +
      scale_fill_brewer(palette = "Set3", name = "Crop") +
      labs(title = "Cultivar Distribution", x = "Cultivar", y = "Number of Plots") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(color = COR_PRIMARIA, face = "bold"),
            axis.text.x = element_text(angle = 35, hjust = 1))
  })
  
  output$comp_espacamento <- renderPlot({
    df <- st_drop_geometry(dados_comp())
    ggplot(df, aes(x = Espacamento, fill = Espacamento)) +
      geom_bar(color = "white") +
      scale_fill_brewer(palette = "Pastel1") +
      labs(title = "Spacing Adopted", x = "Spacing", y = "Number of Plots") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none",
            plot.title = element_text(color = COR_PRIMARIA, face = "bold"),
            axis.text.x = element_text(angle = 30, hjust = 1))
  })
  
  output$comp_temporal <- renderPlot({
    df <- st_drop_geometry(dados_comp()) %>%
      group_by(Ano_Implantacao) %>%
      summarise(n = n(), Area = sum(Area_ha, na.rm=TRUE), .groups = "drop")
    ggplot(df, aes(x = Ano_Implantacao)) +
      geom_col(aes(y = Area), fill = COR_ACENTO, alpha = 0.7, color = "white") +
      geom_line(aes(y = n * max(df$Area) / max(df$n)), color = COR_PRIMARIA,
                linewidth = 1.2) +
      geom_point(aes(y = n * max(df$Area) / max(df$n)), color = COR_PRIMARIA, size = 3) +
      scale_y_continuous(
        name = "Planted Area (ha)",
        sec.axis = sec_axis(~ . * max(df$n) / max(df$Area), name = "Number of Plots")
      ) +
      labs(title = "Area and Plots by Planting Year", x = "Year") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(color = COR_PRIMARIA, face = "bold"))
  })
  
  # Radar chart
  output$comp_radar <- renderPlot({
    df <- st_drop_geometry(dados_comp()) %>%
      mutate(Idade_num = suppressWarnings(as.numeric(Idade))) %>%
      select(Lavoura, Area_ha, Numero_Linhas, Idade_num, Ano_Implantacao) %>%
      group_by(Lavoura) %>%
      summarise(across(everything(), ~mean(.x, na.rm=TRUE)), .groups="drop")
    
    vars <- c("Area_ha","Numero_Linhas","Idade_num","Ano_Implantacao")
    radar_df <- df %>% select(all_of(vars))
    
    max_vals <- sapply(radar_df, max, na.rm=TRUE)
    min_vals <- sapply(radar_df, min, na.rm=TRUE)
    
    radar_plot <- rbind(max_vals, min_vals, radar_df)
    colnames(radar_plot) <- c("Area (ha)","Lines","Age","Planting Year")
    
    n_lav <- nrow(df)
    cores  <- brewer.pal(max(3, n_lav), "Set2")[seq_len(n_lav)]
    
    par(mar = c(1, 1, 2, 1))
    fmsb::radarchart(
      as.data.frame(radar_plot),
      axistype  = 1,
      pcol      = cores,
      pfcol     = adjustcolor(cores, alpha.f = 0.25),
      plwd      = 2.5,
      cglcol    = "grey70", cglty = 1, cglwd = 0.8,
      axislabcol = "grey40",
      vlcex     = 0.9,
      title     = "Multivariate Profile of Selected Crops"
    )
    legend("topright", legend = df$Lavoura, col = cores,
           lty = 1, lwd = 2.5, bty = "n", cex = 0.85)
  })
  
  # Comparative table
  output$tabela_comp <- renderTable({
    st_drop_geometry(dados_comp()) %>%
      select(Lavoura, Cultivar, Area_ha, Numero_Linhas,
             Ano_Implantacao, Idade, Espacamento, Responsavel) %>%
      arrange(Lavoura)
  }, striped = TRUE, hover = TRUE, spacing = "s", digits = 3)
  
  # ═══════════════════════════════════════════════════════════════════════════
  # BY CULTIVAR TAB
  # ═══════════════════════════════════════════════════════════════════════════
  
  df_cultivar <- reactive({
    st_drop_geometry(dados_sp) %>%
      group_by(Cultivar) %>%
      summarise(
        N_Plots     = n(),
        Total_Area  = sum(Area_ha, na.rm=TRUE),
        Mean_Area   = mean(Area_ha, na.rm=TRUE),
        Mean_Age    = mean(suppressWarnings(as.numeric(Idade)), na.rm=TRUE),
        .groups = "drop"
      ) %>% arrange(desc(Total_Area))
  })
  
  output$cult_area_total <- renderPlot({
    ggplot(df_cultivar(),
           aes(x = reorder(Cultivar, Total_Area), y = Total_Area, fill = Cultivar)) +
      geom_bar(stat="identity", color="white") +
      coord_flip() +
      scale_fill_brewer(palette="Set3") +
      labs(title="Total Area by Cultivar", x="Cultivar", y="Area (ha)") +
      theme_minimal(base_size=12) +
      theme(legend.position="none",
            plot.title=element_text(color=COR_PRIMARIA, face="bold"))
  })
  
  output$cult_n_talhoes <- renderPlot({
    ggplot(df_cultivar(),
           aes(x = reorder(Cultivar, N_Plots), y = N_Plots, fill = Cultivar)) +
      geom_bar(stat="identity", color="white") +
      geom_text(aes(label=N_Plots), hjust=-0.2, size=3.5) +
      coord_flip() +
      scale_fill_brewer(palette="Pastel1") +
      labs(title="Number of Plots by Cultivar", x="Cultivar", y="Plots") +
      theme_minimal(base_size=12) +
      theme(legend.position="none",
            plot.title=element_text(color=COR_PRIMARIA, face="bold"))
  })
  
  output$cult_box_area <- renderPlot({
    df <- st_drop_geometry(dados_sp)
    ggplot(df, aes(x=reorder(Cultivar, Area_ha, median), y=Area_ha, fill=Cultivar)) +
      geom_boxplot(outlier.colour=COR_ACENTO, outlier.size=2) +
      coord_flip() +
      scale_fill_brewer(palette="Set3") +
      labs(title="Distribution of Area by Cultivar", x="Cultivar", y="Area (ha)") +
      theme_minimal(base_size=12) +
      theme(legend.position="none",
            plot.title=element_text(color=COR_PRIMARIA, face="bold"))
  })
  
  output$cult_box_idade <- renderPlot({
    df <- st_drop_geometry(dados_sp) %>%
      mutate(Idade_num = suppressWarnings(as.numeric(Idade)))
    ggplot(df, aes(x=reorder(Cultivar, Idade_num, median, na.rm=TRUE),
                   y=Idade_num, fill=Cultivar)) +
      geom_boxplot(outlier.colour=COR_ACENTO, outlier.size=2) +
      coord_flip() +
      scale_fill_brewer(palette="Pastel2") +
      labs(title="Distribution of Age by Cultivar", x="Cultivar", y="Age (years)") +
      theme_minimal(base_size=12) +
      theme(legend.position="none",
            plot.title=element_text(color=COR_PRIMARIA, face="bold"))
  })
  
  output$tabela_cultivar <- renderTable({
    df_cultivar() %>%
      rename(`N Plots` = N_Plots,
             `Total Area` = Total_Area,
             `Mean Area` = Mean_Area,
             `Mean Age` = Mean_Age) %>%
      mutate(across(where(is.numeric), ~round(.x, 2)))
  }, striped=TRUE, hover=TRUE, spacing="s")
  
  # ═══════════════════════════════════════════════════════════════════════════
  # TEMPORAL TAB
  # ═══════════════════════════════════════════════════════════════════════════
  
  output$temp_implantacao <- renderPlot({
    df <- st_drop_geometry(dados_sp) %>%
      group_by(Ano_Implantacao) %>%
      summarise(n=n(), .groups="drop") %>%
      filter(!is.na(Ano_Implantacao))
    ggplot(df, aes(x=Ano_Implantacao, y=n)) +
      geom_col(fill=COR_PRIMARIA, color="white", alpha=0.85) +
      geom_line(color=COR_ACENTO, linewidth=1.2) +
      geom_point(color=COR_ACENTO, size=3) +
      labs(title="Plots Planted per Year", x="Year", y="Number of Plots") +
      theme_minimal(base_size=13) +
      theme(plot.title=element_text(color=COR_PRIMARIA, face="bold"))
  })
  
  output$temp_area_acum <- renderPlot({
    df <- st_drop_geometry(dados_sp) %>%
      filter(!is.na(Ano_Implantacao)) %>%
      group_by(Ano_Implantacao) %>%
      summarise(Area=sum(Area_ha, na.rm=TRUE), .groups="drop") %>%
      arrange(Ano_Implantacao) %>%
      mutate(Area_Acum = cumsum(Area))
    ggplot(df, aes(x=Ano_Implantacao, y=Area_Acum)) +
      geom_area(fill=COR_SECUNDARIA, alpha=0.6) +
      geom_line(color=COR_PRIMARIA, linewidth=1.3) +
      geom_point(color=COR_PRIMARIA, size=3) +
      labs(title="Cumulative Planted Area (ha)", x="Year", y="Cumulative Area (ha)") +
      theme_minimal(base_size=13) +
      theme(plot.title=element_text(color=COR_PRIMARIA, face="bold"))
  })
  
  output$temp_cultivar_ano <- renderPlot({
    df <- st_drop_geometry(dados_sp) %>%
      filter(!is.na(Ano_Implantacao), !is.na(Cultivar)) %>%
      group_by(Ano_Implantacao, Cultivar) %>%
      summarise(Area=sum(Area_ha, na.rm=TRUE), .groups="drop")
    ggplot(df, aes(x=Ano_Implantacao, y=Area, fill=Cultivar)) +
      geom_area(position="stack", alpha=0.85, color="white") +
      scale_fill_brewer(palette="Set3") +
      labs(title="Planted Area by Cultivar Over Time",
           x="Year", y="Area (ha)", fill="Cultivar") +
      theme_minimal(base_size=13) +
      theme(plot.title=element_text(color=COR_PRIMARIA, face="bold"))
  })
}

shinyApp(ui, server)
