# ============================================================
# Shiny App - Seguimiento temporal de índices vegetativos
# Finca Agrikova (Piña)
# Versión 5 - estilo profesional
# ============================================================
# Cómo usar:
# - Estructura esperada del proyecto:
#   /app.R
#   /data/*.gpkg
#   /www/Logo_GXL.jpg
# - El app leerá automáticamente todos los archivos .gpkg de /data
# - El logo se leerá desde /www/Logo_GXL.jpg
# - Ejecuta: shiny::runApp()
#
# Paquetes requeridos:
# install.packages(c(
#   "shiny", "sf", "dplyr", "tidyr", "ggplot2", "plotly",
#   "leaflet", "DT", "lubridate", "scales", "htmltools", "writexl"
# ))
# ============================================================

library(shiny)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(DT)
library(lubridate)
library(scales)
library(htmltools)
library(writexl)

# ------------------------------------------------------------
# 1) CONFIGURACIÓN DE ARCHIVOS
# ------------------------------------------------------------

data_dir <- "data"
www_dir <- "www"
gpkg_files <- list.files(data_dir, pattern = "\\.gpkg$", full.names = TRUE, ignore.case = TRUE)
gpkg_files <- sort(gpkg_files)

if (!dir.exists(data_dir)) {
  stop("No se encontró la carpeta 'data'. Crea la carpeta data y coloca allí los archivos .gpkg.")
}

if (length(gpkg_files) == 0) {
  stop("No se encontraron archivos .gpkg dentro de la carpeta data.")
}

logo_file <- file.path(www_dir, "Logo_GXL.jpg")

# ------------------------------------------------------------
# 2) FUNCIONES AUXILIARES
# ------------------------------------------------------------

parse_fecha_segura <- function(x) {
  x <- as.character(x)

  y <- suppressWarnings(as.Date(x, format = "%d %b %Y"))

  idx <- is.na(y)
  if (any(idx)) y[idx] <- suppressWarnings(as.Date(x[idx], format = "%Y-%m-%d"))

  idx <- is.na(y)
  if (any(idx)) y[idx] <- suppressWarnings(as.Date(x[idx], format = "%d/%m/%Y"))

  idx <- is.na(y)
  if (any(idx)) {
    y[idx] <- suppressWarnings(as.Date(
      parse_date_time(x[idx], orders = c("d b Y", "d B Y", "Y-m-d", "d/m/Y"), locale = "C")
    ))
  }

  y
}

extraer_numero_bloque <- function(x) {
  x_chr <- as.character(x)
  suppressWarnings(as.numeric(gsub("[^0-9.-]", "", x_chr)))
}

ordenar_bloques <- function(x) {
  x_chr <- unique(as.character(x))
  nums <- extraer_numero_bloque(x_chr)
  x_chr[order(is.na(nums), nums, x_chr)]
}

paleta_indice <- function(indice) {
  switch(
    indice,
    "Biomasa"   = c("#FFFFFF", "#006d2c"),
    "Estres"    = c("#d73027", "#1a9850"),
    "Clorofila" = c("#d73027", "#2166ac"),
    "Nitrogeno" = c("#2166ac", "#ffd92f"),
    c("#FFFFFF", "#006d2c")
  )
}

color_principal_indice <- function(indice) {
  switch(
    indice,
    "Biomasa"   = "#0b6e3b",
    "Estres"    = "#2e8b57",
    "Clorofila" = "#2166ac",
    "Nitrogeno" = "#d4a200",
    "#0b6e3b"
  )
}

tema_grafico <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", colour = "#153b2e", size = base_size + 2),
      plot.subtitle = element_text(colour = "#5b6b65"),
      axis.title = element_text(face = "bold", colour = "#28453a"),
      axis.text = element_text(colour = "#33413b"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA)
    )
}

leer_gpkg <- function(path_gpkg) {
  capa <- st_layers(path_gpkg)$name[1]
  x <- st_read(path_gpkg, layer = capa, quiet = TRUE, stringsAsFactors = FALSE)

  if (!"Name" %in% names(x)) x$Name <- NA_character_
  if (!"Fecha_Monitoreo" %in% names(x)) x$Fecha_Monitoreo <- NA_character_
  if (!"Biomasa" %in% names(x)) x$Biomasa <- NA_real_
  if (!"Clorofila" %in% names(x)) x$Clorofila <- NA_real_
  if (!"Estres" %in% names(x)) x$Estres <- NA_real_
  if (!"Nitrogeno" %in% names(x)) x$Nitrogeno <- NA_real_
  if (!"Area" %in% names(x)) x$Area <- NA_real_
  if (!"Area_Efectiva" %in% names(x)) x$Area_Efectiva <- NA_real_
  if (!"Densidad" %in% names(x)) x$Densidad <- NA_real_

  col_numpoints <- names(x)[tolower(names(x)) == "numpoints"]
  if (length(col_numpoints) == 0) {
    x$NUMPOINTS <- NA_real_
  } else if (col_numpoints[1] != "NUMPOINTS") {
    x$NUMPOINTS <- x[[col_numpoints[1]]]
  }

  x %>%
    st_make_valid() %>%
    mutate(
      Name = as.character(Name),
      Bloque = as.character(Name),
      Fecha_Monitoreo = parse_fecha_segura(Fecha_Monitoreo),
      Biomasa = suppressWarnings(as.numeric(Biomasa)),
      Clorofila = suppressWarnings(as.numeric(Clorofila)),
      Estres = suppressWarnings(as.numeric(Estres)),
      Nitrogeno = suppressWarnings(as.numeric(Nitrogeno)),
      Area = suppressWarnings(as.numeric(Area)),
      Area_Efectiva = suppressWarnings(as.numeric(Area_Efectiva)),
      Densidad = suppressWarnings(as.numeric(Densidad)),
      NUMPOINTS = suppressWarnings(as.numeric(NUMPOINTS))
    )
}

valor_inicial_final_global <- function(df, campo, fecha_inicial, fecha_final) {
  dat <- df %>%
    st_drop_geometry() %>%
    filter(Fecha_Monitoreo %in% c(fecha_inicial, fecha_final))

  v_inicial <- dat %>%
    filter(Fecha_Monitoreo == fecha_inicial) %>%
    summarise(valor = dplyr::first(.data[[campo]])) %>%
    pull(valor)

  v_final <- dat %>%
    filter(Fecha_Monitoreo == fecha_final) %>%
    summarise(valor = dplyr::first(.data[[campo]])) %>%
    pull(valor)

  if (length(v_inicial) == 0) v_inicial <- NA_real_
  if (length(v_final) == 0) v_final <- NA_real_

  delta_abs <- ifelse(is.na(v_inicial) | is.na(v_final), NA_real_, v_final - v_inicial)
  delta_pct <- ifelse(is.na(v_inicial) | is.na(v_final) | v_inicial == 0, NA_real_, (delta_abs / v_inicial) * 100)

  list(
    inicial = v_inicial,
    final = v_final,
    delta_abs = delta_abs,
    delta_pct = delta_pct
  )
}

fmt_num <- function(x, digits = 4) {
  ifelse(is.na(x), "NA", format(round(x, digits), nsmall = digits, trim = TRUE))
}

obtener_rango_fijo <- function(x) {
  x_num <- suppressWarnings(as.numeric(x))
  x_num <- x_num[is.finite(x_num)]

  if (length(x_num) == 0) return(c(0, 1))

  r <- range(x_num, na.rm = TRUE)
  if (!is.finite(r[1]) || !is.finite(r[2])) return(c(0, 1))

  if (r[1] == r[2]) {
    ajuste <- ifelse(r[1] == 0, 1, abs(r[1]) * 0.01)
    return(c(r[1] - ajuste, r[2] + ajuste))
  }

  r
}

# ------------------------------------------------------------
# 3) CARGA Y PREPARACIÓN DE DATOS
# ------------------------------------------------------------

lista_sf <- lapply(gpkg_files, leer_gpkg)
datos <- bind_rows(lista_sf)

campos_requeridos <- c("Bloque", "Fecha_Monitoreo", "Biomasa", "Clorofila", "Estres", "Nitrogeno")
faltantes <- setdiff(campos_requeridos, names(datos))
if (length(faltantes) > 0) {
  stop(paste("Faltan campos requeridos:", paste(faltantes, collapse = ", ")))
}

datos <- datos %>%
  filter(!is.na(Fecha_Monitoreo))

if (nrow(datos) == 0) {
  stop("No hay registros válidos con Fecha_Monitoreo.")
}

if (is.na(st_crs(datos))) {
  stop("Los datos no tienen CRS definido.")
}

datos_wgs84 <- st_transform(datos, 4326)

fechas_disponibles <- sort(unique(datos$Fecha_Monitoreo))
fecha_inicial_global <- min(fechas_disponibles, na.rm = TRUE)
fecha_final_global <- max(fechas_disponibles, na.rm = TRUE)

rangos_indices <- list(
  Biomasa = obtener_rango_fijo(datos$Biomasa),
  Clorofila = obtener_rango_fijo(datos$Clorofila),
  Estres = obtener_rango_fijo(datos$Estres),
  Nitrogeno = obtener_rango_fijo(datos$Nitrogeno)
)

bloques_disponibles <- ordenar_bloques(unique(datos$Bloque))

bbox <- st_bbox(datos_wgs84)
centro_lng <- mean(c(bbox["xmin"], bbox["xmax"]))
centro_lat <- mean(c(bbox["ymin"], bbox["ymax"]))

finca_union_geom <- st_union(datos_wgs84)
finca_union_sf <- st_as_sf(data.frame(nombre = "Finca Agrikova", geometry = st_sfc(finca_union_geom, crs = 4326)))
finca_centroid_sf <- suppressWarnings(st_centroid(finca_union_sf))

# ------------------------------------------------------------
# 4) UI
# ------------------------------------------------------------

logo_ui <- if (file.exists(logo_file)) {
  tags$img(src = "Logo_GXL.jpg", class = "brand-logo")
} else {
  NULL
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML(" 
      :root {
        --gxl-green: #0d5c3f;
        --gxl-green-dark: #083726;
        --gxl-green-soft: #edf6f1;
        --gxl-border: #d7e4dd;
        --gxl-text: #173228;
        --gxl-muted: #61756c;
        --gxl-white: #ffffff;
        --gxl-shadow: 0 10px 26px rgba(13, 92, 63, 0.08);
        --gxl-radius: 18px;
      }

      body {
        background: linear-gradient(180deg, #f5faf7 0%, #eef4f1 100%);
        color: var(--gxl-text);
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      }

      .container-fluid {
        max-width: 1750px;
        padding-left: 22px;
        padding-right: 22px;
      }

      .hero {
        background: linear-gradient(135deg, var(--gxl-green-dark) 0%, var(--gxl-green) 100%);
        color: white;
        border-radius: 22px;
        padding: 20px 24px;
        margin-top: 10px;
        margin-bottom: 18px;
        box-shadow: 0 14px 30px rgba(8, 55, 38, 0.16);
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 18px;
      }

      .hero-title {
        margin: 0;
        font-size: 32px;
        font-weight: 800;
        letter-spacing: 0.2px;
      }

      .hero-subtitle {
        margin-top: 6px;
        margin-bottom: 0;
        color: rgba(255,255,255,0.85);
        font-size: 14px;
      }

      .brand-logo {
        height: 86px;
        width: auto;
        border-radius: 14px;
        background: rgba(255,255,255,0.94);
        padding: 6px;
      }

      .controls-grid {
        display: grid;
        grid-template-columns: repeat(4, minmax(220px, 1fr));
        gap: 14px;
        margin-bottom: 18px;
      }

      .control-card, .panel-card, .stats-card {
        background: var(--gxl-white);
        border: 1px solid var(--gxl-border);
        border-radius: var(--gxl-radius);
        box-shadow: var(--gxl-shadow);
      }

      .control-card {
        padding: 14px 16px 8px 16px;
      }

      .control-card .form-group {
        margin-bottom: 6px;
      }

      .control-note {
        color: var(--gxl-muted);
        font-size: 12px;
        line-height: 1.45;
        margin-top: 4px;
      }

      .summary-chip {
        font-size: 13px;
        color: var(--gxl-text);
        line-height: 1.65;
        padding-top: 6px;
      }

      .nav-tabs {
        border-bottom: none;
        margin-bottom: 10px;
      }

      .nav-tabs > li > a {
        border: none !important;
        border-radius: 14px !important;
        margin-right: 10px;
        background: #e7f0eb;
        color: var(--gxl-green-dark);
        font-weight: 700;
        padding: 12px 18px;
      }

      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        background: linear-gradient(135deg, var(--gxl-green-dark) 0%, var(--gxl-green) 100%) !important;
        color: white !important;
        box-shadow: 0 10px 24px rgba(13, 92, 63, 0.20);
      }

      .tab-content {
        padding-top: 6px;
      }

      .map-wrap {
        padding: 14px;
      }

      .sidebar-card {
        position: sticky;
        top: 12px;
        padding: 18px;
      }

      .section-title {
        font-size: 20px;
        font-weight: 800;
        margin-top: 0;
        margin-bottom: 14px;
        color: var(--gxl-green-dark);
      }

      .section-subtitle {
        font-size: 13px;
        color: var(--gxl-muted);
        margin-bottom: 14px;
      }

      .stats-card {
        padding: 16px 18px;
        min-height: 120px;
      }

      .stats-label {
        color: var(--gxl-muted);
        font-size: 13px;
        margin-bottom: 8px;
        font-weight: 600;
      }

      .stats-value {
        color: var(--gxl-green-dark);
        font-size: 30px;
        font-weight: 800;
        line-height: 1.15;
      }

      .stats-note {
        color: var(--gxl-muted);
        font-size: 12px;
        margin-top: 8px;
      }

      .panel-card {
        padding: 18px 18px 12px 18px;
        margin-bottom: 16px;
      }

      .panel-title {
        font-size: 18px;
        font-weight: 800;
        color: var(--gxl-green-dark);
        margin-top: 0;
        margin-bottom: 6px;
      }

      .panel-desc {
        font-size: 12px;
        color: var(--gxl-muted);
        margin-bottom: 8px;
      }

      .data-header {
        display: flex;
        justify-content: space-between;
        align-items: center;
        gap: 12px;
        margin-bottom: 10px;
      }

      .btn-default, .btn-primary {
        border-radius: 10px;
        font-weight: 700;
      }

      .leaflet-container {
        border-radius: 16px;
      }

      .selectize-input, .form-control {
        border-radius: 10px !important;
        border-color: #c9d9d1 !important;
        box-shadow: none !important;
      }

      .selectize-input.focus, .form-control:focus {
        border-color: var(--gxl-green) !important;
      }

      .help-text-custom {
        color: var(--gxl-muted);
        font-size: 12px;
        line-height: 1.5;
      }

      @media (max-width: 1199px) {
        .controls-grid {
          grid-template-columns: repeat(2, minmax(220px, 1fr));
        }
      }

      @media (max-width: 767px) {
        .controls-grid {
          grid-template-columns: 1fr;
        }
        .hero {
          flex-direction: column;
          align-items: flex-start;
        }
        .brand-logo {
          height: 70px;
        }
      }
    "))
  ),

  div(
    class = "hero",
    div(
      tags$h1(class = "hero-title", "Finca Agrikova"),
      tags$p(class = "hero-subtitle", "Visualización espacial, análisis temporal y comparación entre bloques de monitoreo.")
    ),
    logo_ui
  ),

  div(
    class = "controls-grid",
    div(
      class = "control-card",
      selectInput(
        inputId = "indice",
        label = "Índice a visualizar",
        choices = c("Biomasa", "Clorofila", "Estres", "Nitrogeno"),
        selected = "Biomasa"
      )
    ),
    div(
      class = "control-card",
      selectInput(
        inputId = "fecha_mapa",
        label = "Fecha de análisis",
        choices = setNames(as.character(fechas_disponibles), format(fechas_disponibles, "%d-%m-%Y")),
        selected = as.character(fecha_inicial_global)
      )
    ),
    div(
      class = "control-card",
      checkboxInput(
        inputId = "mostrar_etiquetas",
        label = "Mostrar etiquetas de bloques",
        value = FALSE
      ),
      div(class = "control-note", "El mapa y la tabla muestran únicamente los bloques existentes en la fecha seleccionada.")
    ),
    div(
      class = "control-card",
      div(class = "summary-chip", htmlOutput("resumen_fechas_globales"))
    )
  ),

  tabsetPanel(
    id = "tabs_principales",

    tabPanel(
      "Mapa de la finca",
      div(
        class = "panel-card map-wrap",
        leafletOutput("mapa_finca", height = "78vh")
      )
    ),

    tabPanel(
      "Análisis gráfico",
      br(),
      fluidRow(
        column(
          3,
          div(
            class = "panel-card sidebar-card",
            h3(class = "section-title", "Controles de análisis"),
            p(class = "section-subtitle", "Selecciona un bloque y compara su comportamiento en el tiempo."),
            selectInput(
              inputId = "bloque",
              label = "Bloque para seguimiento temporal",
              choices = bloques_disponibles,
              selected = bloques_disponibles[1]
            ),
            selectizeInput(
              inputId = "bloques_comparar",
              label = "Bloques para comparar en la fecha seleccionada (máx. 10)",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = list(maxItems = 10, placeholder = 'Seleccione hasta 10 bloques')
            ),
            tags$hr(),
            div(class = "help-text-custom", "Puedes hacer clic sobre un bloque en el mapa para seleccionarlo automáticamente."),
            div(class = "help-text-custom", style = "margin-top:8px;", "El valor inicial corresponde a la primera fecha disponible y el valor final a la última fecha disponible.")
          )
        ),
        column(
          9,
          fluidRow(
            column(3, uiOutput("card_inicial")),
            column(3, uiOutput("card_final")),
            column(3, uiOutput("card_delta_abs")),
            column(3, uiOutput("card_delta_pct"))
          ),
          div(
            class = "panel-card",
            h4(class = "panel-title", "Seguimiento temporal del bloque"),
            div(class = "panel-desc", "Valores del índice seleccionado para el bloque a través del tiempo."),
            plotlyOutput("grafico_bloque_tiempo", height = 340),
            DTOutput("tabla_bloque")
          ),
          div(
            class = "panel-card",
            h4(class = "panel-title", "Cambio temporal del índice en todas las fechas"),
            div(class = "panel-desc", "Barras por fecha para el bloque seleccionado, mostrando además la variación frente a la fecha anterior."),
            plotlyOutput("grafico_cambio_temporal", height = 360)
          ),
          div(
            class = "panel-card",
            h4(class = "panel-title", "Comparación entre bloques en una fecha"),
            div(class = "panel-desc", "Comparación directa entre los bloques seleccionados para la fecha activa."),
            plotlyOutput("grafico_barras_fecha", height = 420)
          )
        )
      )
    ),


    tabPanel(
      "Cantidad de plantas",
      br(),
      fluidRow(
        column(
          3,
          div(
            class = "panel-card sidebar-card",
            h3(class = "section-title", "Cantidad de plantas sembradas"),
            p(class = "section-subtitle", "Selecciona un bloque para visualizar la cantidad de plantas registradas en cada fecha de monitoreo."),
            selectInput(
              inputId = "bloque_plantas",
              label = "Bloque a graficar",
              choices = bloques_disponibles,
              selected = bloques_disponibles[1]
            ),
            div(class = "help-text-custom")
          )
        ),
        column(
          9,
          div(
            class = "panel-card",
            h4(class = "panel-title", "Cantidad de plantas por bloque"),
            div(class = "panel-desc", "Evolución temporal de la cantidad de plantas sembradas para el bloque seleccionado."),
            plotlyOutput("grafico_cantidad_plantas", height = 420),
            DTOutput("tabla_cantidad_plantas")
          )
        )
      )
    ),

    tabPanel(
      "Datos filtrados",
      br(),
      div(
        class = "panel-card",
        div(
          class = "data-header",
          div(
            tags$h4(class = "panel-title", "Datos filtrados por fecha"),
            div(class = "panel-desc", "Tabla exportable correspondiente a la fecha seleccionada en la parte superior.")
          ),
          div(
            downloadButton("descargar_csv", "Descargar CSV"),
            tags$span(" "),
            downloadButton("descargar_xlsx", "Descargar Excel")
          )
        ),
        DTOutput("tabla_datos")
      )
    )
  )
)

# ------------------------------------------------------------
# 5) SERVER
# ------------------------------------------------------------

server <- function(input, output, session) {

  output$resumen_fechas_globales <- renderUI({
    HTML(paste0(
      "<strong>Fecha inicial:</strong> ", format(fecha_inicial_global, "%d-%m-%Y"), "<br/>",
      "<strong>Fecha final:</strong> ", format(fecha_final_global, "%d-%m-%Y")
    ))
  })

  observeEvent(input$mapa_finca_shape_click, {
    click <- input$mapa_finca_shape_click
    if (!is.null(click$id) && nzchar(click$id)) {
      updateSelectInput(session, "bloque", selected = click$id)
      updateSelectInput(session, "bloque_plantas", selected = click$id)
      updateTabsetPanel(session, "tabs_principales", selected = "Análisis gráfico")
    }
  })

  datos_fecha_mapa <- reactive({
    req(input$fecha_mapa)
    fecha_sel <- as.Date(input$fecha_mapa)

    datos_wgs84 %>%
      filter(Fecha_Monitoreo == fecha_sel)
  })

  datos_bloque <- reactive({
    req(input$bloque)

    datos %>%
      filter(Bloque == input$bloque) %>%
      arrange(Fecha_Monitoreo)
  })

  datos_bloque_plantas <- reactive({
    req(input$bloque_plantas)

    datos %>%
      filter(Bloque == input$bloque_plantas) %>%
      arrange(Fecha_Monitoreo)
  })

  bloques_fecha_actual <- reactive({
    req(input$fecha_mapa)
    fecha_sel <- as.Date(input$fecha_mapa)

    datos %>%
      st_drop_geometry() %>%
      filter(Fecha_Monitoreo == fecha_sel) %>%
      pull(Bloque) %>%
      unique() %>%
      ordenar_bloques()
  })

  datos_comparacion_fecha <- reactive({
    req(input$fecha_mapa)
    fecha_sel <- as.Date(input$fecha_mapa)

    dat <- datos %>%
      st_drop_geometry() %>%
      filter(Fecha_Monitoreo == fecha_sel)

    if (!is.null(input$bloques_comparar) && length(input$bloques_comparar) > 0) {
      dat <- dat %>% filter(Bloque %in% input$bloques_comparar)
    } else {
      dat <- dat %>% slice(0)
    }

    dat %>% arrange(match(Bloque, ordenar_bloques(Bloque)))
  })

  datos_fecha_tabla <- reactive({
    req(input$fecha_mapa)
    fecha_sel <- as.Date(input$fecha_mapa)

    datos %>%
      st_drop_geometry() %>%
      filter(Fecha_Monitoreo == fecha_sel) %>%
      mutate(Bloque_num = extraer_numero_bloque(Bloque)) %>%
      arrange(Bloque_num, Bloque) %>%
      select(-Bloque_num)
  })

  observeEvent(input$fecha_mapa, {
    bloques_fecha <- bloques_fecha_actual()
    seleccion_actual <- isolate(input$bloques_comparar)
    seleccion_actual <- seleccion_actual[seleccion_actual %in% bloques_fecha]

    if (length(seleccion_actual) == 0) {
      seleccion_actual <- head(bloques_fecha, 10)
    }

    updateSelectizeInput(
      session,
      inputId = "bloques_comparar",
      choices = bloques_fecha,
      selected = seleccion_actual,
      server = TRUE
    )

    bloque_actual <- isolate(input$bloque)
    if (!is.null(bloque_actual) && !(bloque_actual %in% bloques_fecha)) {
      if (length(bloques_fecha) > 0) {
        updateSelectInput(session, "bloque", selected = bloques_fecha[1])
      }
    }

    bloque_plantas_actual <- isolate(input$bloque_plantas)
    if (is.null(bloque_plantas_actual) || !(bloque_plantas_actual %in% bloques_disponibles)) {
      if (length(bloques_disponibles) > 0) {
        updateSelectInput(session, "bloque_plantas", selected = bloques_disponibles[1])
      }
    }
  }, ignoreInit = FALSE)

  output$mapa_finca <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satélite") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Base clara") %>%
      setView(lng = centro_lng, lat = centro_lat, zoom = 17)
  })

  observe({
    req(input$indice, input$fecha_mapa)

    dat_map <- datos_fecha_mapa()
    indice_sel <- input$indice

    proxy <- leafletProxy("mapa_finca") %>%
      clearShapes() %>%
      clearMarkers() %>%
      clearControls()

    proxy <- proxy %>%
      addMarkers(
        data = finca_centroid_sf,
        popup = "Finca Agrikova",
        group = "Ubicación"
      )

    if (nrow(dat_map) == 0) {
      proxy %>%
        addLayersControl(
          baseGroups = c("Satélite", "Base clara"),
          overlayGroups = c("Ubicación"),
          options = layersControlOptions(collapsed = FALSE)
        )
      return()
    }

    rango_fijo <- rangos_indices[[indice_sel]]

    pal <- colorNumeric(
      palette = paleta_indice(indice_sel),
      domain = rango_fijo,
      na.color = "#bdbdbd"
    )

    etiquetas <- sprintf(
      "<strong>Bloque:</strong> %s<br/>
       <strong>Fecha:</strong> %s<br/>
       <strong>%s:</strong> %s<br/>
       <strong>Biomasa:</strong> %s<br/>
       <strong>Clorofila:</strong> %s<br/>
       <strong>Estrés:</strong> %s<br/>
       <strong>Nitrógeno:</strong> %s",
      dat_map$Bloque,
      format(dat_map$Fecha_Monitoreo, "%d-%m-%Y"),
      indice_sel,
      fmt_num(dat_map[[indice_sel]], 4),
      fmt_num(dat_map$Biomasa, 4),
      fmt_num(dat_map$Clorofila, 4),
      fmt_num(dat_map$Estres, 4),
      fmt_num(dat_map$Nitrogeno, 4)
    ) %>% lapply(HTML)

    proxy %>%
      addPolygons(
        data = dat_map,
        fillColor = pal(dat_map[[indice_sel]]),
        fillOpacity = 0.82,
        color = "#20352d",
        weight = 1,
        layerId = ~Bloque,
        popup = etiquetas,
        label = ~Bloque,
        labelOptions = labelOptions(
          textsize = "11px",
          direction = "center",
          noHide = isTRUE(input$mostrar_etiquetas),
          style = list(
            "font-weight" = "bold",
            "color" = "#111111",
            "text-shadow" = "1px 1px 2px white"
          )
        ),
        group = "Bloques"
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = rango_fijo,
        title = paste0(
          indice_sel, "<br>", format(as.Date(input$fecha_mapa), "%d-%m-%Y"),
          "<br><span style='font-size:11px;'>Escala fija global</span>"
        ),
        opacity = 0.95
      ) %>%
      addLayersControl(
        baseGroups = c("Satélite", "Base clara"),
        overlayGroups = c("Bloques", "Ubicación"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })

  output$card_inicial <- renderUI({
    dat <- datos_bloque()
    delta <- valor_inicial_final_global(dat, input$indice, fecha_inicial_global, fecha_final_global)

    div(class = "stats-card",
        div(class = "stats-label", "Valor inicial"),
        div(class = "stats-value", fmt_num(delta$inicial, 4)),
        div(class = "stats-note", paste("Fecha:", format(fecha_inicial_global, "%d-%m-%Y")))
    )
  })

  output$card_final <- renderUI({
    dat <- datos_bloque()
    delta <- valor_inicial_final_global(dat, input$indice, fecha_inicial_global, fecha_final_global)

    div(class = "stats-card",
        div(class = "stats-label", "Valor final"),
        div(class = "stats-value", fmt_num(delta$final, 4)),
        div(class = "stats-note", paste("Fecha:", format(fecha_final_global, "%d-%m-%Y")))
    )
  })

  output$card_delta_abs <- renderUI({
    dat <- datos_bloque()
    delta <- valor_inicial_final_global(dat, input$indice, fecha_inicial_global, fecha_final_global)

    div(class = "stats-card",
        div(class = "stats-label", "Cambio absoluto"),
        div(class = "stats-value", fmt_num(delta$delta_abs, 4)),
        div(class = "stats-note", "Valor final - valor inicial")
    )
  })

  output$card_delta_pct <- renderUI({
    dat <- datos_bloque()
    delta <- valor_inicial_final_global(dat, input$indice, fecha_inicial_global, fecha_final_global)
    pct_txt <- ifelse(is.na(delta$delta_pct), "NA", paste0(round(delta$delta_pct, 2), "%"))

    div(class = "stats-card",
        div(class = "stats-label", "Cambio porcentual"),
        div(class = "stats-value", pct_txt),
        div(class = "stats-note", "Respecto al valor inicial")
    )
  })

  output$grafico_bloque_tiempo <- renderPlotly({
    dat <- datos_bloque() %>% st_drop_geometry()
    validate(
      need(nrow(dat) > 0, "No hay datos para el bloque seleccionado.")
    )

    dat <- dat %>%
      arrange(Fecha_Monitoreo) %>%
      mutate(
        Fecha_label = format(Fecha_Monitoreo, "%d-%m-%Y"),
        Fecha_factor = factor(Fecha_label, levels = Fecha_label)
      )

    col_barra <- color_principal_indice(input$indice)

    p <- ggplot(
      dat,
      aes(
        x = Fecha_factor,
        y = .data[[input$indice]],
        text = paste0(
          "Bloque: ", Bloque,
          "<br>Fecha: ", Fecha_label,
          "<br>", input$indice, ": ", fmt_num(.data[[input$indice]], 4)
        )
      )
    ) +
      geom_col(fill = col_barra, width = 0.72) +
      labs(
        title = paste("Seguimiento temporal del bloque", input$bloque),
        x = "Fecha de monitoreo",
        y = input$indice
      ) +
      tema_grafico(13)

    ggplotly(p, tooltip = "text")
  })

  output$tabla_bloque <- renderDT({
    dat <- datos_bloque() %>%
      st_drop_geometry() %>%
      mutate(Bloque_num = extraer_numero_bloque(Bloque)) %>%
      arrange(Fecha_Monitoreo, Bloque_num, Bloque) %>%
      transmute(
        Bloque = Bloque,
        `Fecha de Monitoreo` = format(Fecha_Monitoreo, "%d-%m-%Y"),
        Biomasa = Biomasa,
        Clorofila = Clorofila,
        Estrés = Estres,
        Nitrógeno = Nitrogeno,
        Área = Area,
        `Área Efectiva` = Area_Efectiva,
        Densidad = Densidad
      )

    datatable(
      dat,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE, dom = "tip")
    )
  })

  output$grafico_cambio_temporal <- renderPlotly({
    dat <- datos_bloque() %>% st_drop_geometry()
    validate(
      need(nrow(dat) > 0, "No hay datos para el bloque seleccionado.")
    )

    dat <- dat %>%
      arrange(Fecha_Monitoreo) %>%
      mutate(
        Fecha_label = format(Fecha_Monitoreo, "%d-%m-%Y"),
        Fecha_factor = factor(Fecha_label, levels = Fecha_label),
        valor = .data[[input$indice]],
        cambio_abs = valor - lag(valor),
        cambio_pct = ifelse(is.na(lag(valor)) | lag(valor) == 0, NA_real_, (cambio_abs / lag(valor)) * 100)
      )

    col_barra <- color_principal_indice(input$indice)

    p <- ggplot(
      dat,
      aes(
        x = Fecha_factor,
        y = valor,
        text = paste0(
          "Bloque: ", input$bloque,
          "<br>Fecha: ", Fecha_label,
          "<br>", input$indice, ": ", fmt_num(valor, 4),
          "<br>Cambio vs fecha anterior: ", ifelse(is.na(cambio_abs), "NA", fmt_num(cambio_abs, 4)),
          "<br>Cambio % vs fecha anterior: ", ifelse(is.na(cambio_pct), "NA", paste0(round(cambio_pct, 2), "%"))
        )
      )
    ) +
      geom_col(fill = col_barra, width = 0.72, alpha = 0.9) +
      labs(
        title = paste("Cambio temporal del bloque", input$bloque),
        x = "Fecha de monitoreo",
        y = input$indice
      ) +
      tema_grafico(13)

    ggplotly(p, tooltip = "text")
  })

  output$grafico_barras_fecha <- renderPlotly({
    dat <- datos_comparacion_fecha()
    validate(
      need(length(input$bloques_comparar) > 0, "Selecciona entre 1 y 10 bloques para comparar."),
      need(nrow(dat) > 0, "No hay datos para los bloques seleccionados en la fecha elegida.")
    )

    orden_bloques <- ordenar_bloques(dat$Bloque)
    dat <- dat %>%
      mutate(
        valor = .data[[input$indice]],
        Bloque = factor(Bloque, levels = orden_bloques)
      ) %>%
      arrange(Bloque)

    col_barra <- color_principal_indice(input$indice)

    p <- ggplot(
      dat,
      aes(
        x = Bloque,
        y = valor,
        text = paste0(
          "Bloque: ", Bloque,
          "<br>Fecha: ", format(Fecha_Monitoreo, "%d-%m-%Y"),
          "<br>", input$indice, ": ", fmt_num(valor, 4)
        )
      )
    ) +
      geom_col(fill = col_barra, width = 0.72) +
      labs(
        title = paste("Comparación de bloques -", format(as.Date(input$fecha_mapa), "%d-%m-%Y")),
        x = "Bloque",
        y = input$indice
      ) +
      tema_grafico(13)

    ggplotly(p, tooltip = "text")
  })



  output$grafico_cantidad_plantas <- renderPlotly({
    dat <- datos_bloque_plantas() %>% st_drop_geometry()
    validate(
      need(nrow(dat) > 0, "No hay datos para el bloque seleccionado."),
      need(any(!is.na(dat$NUMPOINTS)), "No hay valores disponibles de cantidad de plantas para el bloque seleccionado.")
    )

    dat <- dat %>%
      arrange(Fecha_Monitoreo) %>%
      mutate(
        Fecha_label = format(Fecha_Monitoreo, "%d-%m-%Y"),
        Fecha_factor = factor(Fecha_label, levels = Fecha_label)
      )

    p <- ggplot(
      dat,
      aes(
        x = Fecha_factor,
        y = NUMPOINTS,
        text = paste0(
          "Bloque: ", Bloque,
          "<br>Fecha: ", Fecha_label,
          "<br>Cantidad de Plantas: ", scales::comma(NUMPOINTS, accuracy = 1)
        )
      )
    ) +
      geom_col(fill = "#4f8a5b", width = 0.72) +
      labs(
        title = paste("Cantidad de plantas del bloque", input$bloque_plantas),
        x = "Fecha de monitoreo",
        y = "Cantidad de Plantas"
      ) +
      tema_grafico(13)

    ggplotly(p, tooltip = "text")
  })

  output$tabla_cantidad_plantas <- renderDT({
    dat <- datos_bloque_plantas() %>%
      st_drop_geometry() %>%
      mutate(Bloque_num = extraer_numero_bloque(Bloque)) %>%
      arrange(Fecha_Monitoreo, Bloque_num, Bloque) %>%
      transmute(
        Bloque = Bloque,
        `Fecha de Monitoreo` = format(Fecha_Monitoreo, "%d-%m-%Y"),
        `Cantidad de Plantas` = NUMPOINTS
      )

    datatable(
      dat,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE, dom = "tip")
    )
  })


  output$tabla_datos <- renderDT({    dat <- datos_fecha_tabla() %>%
      transmute(
        Bloque = Bloque,
        `Fecha de Monitoreo` = format(Fecha_Monitoreo, "%d-%m-%Y"),
        Biomasa = Biomasa,
        Clorofila = Clorofila,
        Estrés = Estres,
        Nitrógeno = Nitrogeno,
        Área = Area,
        `Área Efectiva` = Area_Efectiva,
        Densidad = Densidad,
        `Cantidad de Plantas` = NUMPOINTS
      )

    datatable(
      dat,
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 15, scrollX = TRUE)
    )
  })

  datos_descarga <- reactive({
    datos_fecha_tabla() %>%
      transmute(
        Bloque = Bloque,
        `Fecha de Monitoreo` = format(Fecha_Monitoreo, "%d-%m-%Y"),
        Biomasa = Biomasa,
        Clorofila = Clorofila,
        Estrés = Estres,
        Nitrógeno = Nitrogeno,
        Área = Area,
        `Área Efectiva` = Area_Efectiva,
        Densidad = Densidad,
        `Cantidad de Plantas` = NUMPOINTS
      )
  })

  output$descargar_csv <- downloadHandler(
    filename = function() {
      paste0("Agrikova_datos_filtrados_", format(as.Date(input$fecha_mapa), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write.csv(datos_descarga(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  output$descargar_xlsx <- downloadHandler(
    filename = function() {
      paste0("Agrikova_datos_filtrados_", format(as.Date(input$fecha_mapa), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(datos_descarga(), path = file)
    }
  )
}

# ------------------------------------------------------------
# 6) EJECUTAR APP
# ------------------------------------------------------------

shinyApp(ui, server)
