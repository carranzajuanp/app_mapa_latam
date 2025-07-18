# app.R

# Cargar librerías necesarias
library(shiny)
library(leaflet) # Para mapas interactivos
library(RSQLite) # Para la base de datos SQLite
library(uuid)    # Para generar IDs únicos
library(dplyr)   # Para manipulación de datos (opcional, pero buena práctica)
library(htmltools) # Asegurarse de que esté disponible para tagAppendAttributes
library(htmlwidgets) # Necesario para htmlwidgets::onRender

# --- Configuración de la base de datos ---
db_path <- "data.sqlite"
table_name <- "puntos_mapa"

# Función para inicializar la base de datos
init_db <- function() {
  db <- dbConnect(SQLite(), dbname = db_path)
  if (!dbExistsTable(db, table_name)) {
    dbCreateTable(db, table_name, fields = c(
      ID = "TEXT",
      Latitud = "REAL",
      Longitud = "REAL",
      Valor = "REAL",
      Fecha = "TEXT", # Guardar como texto 'YYYY-MM-DD'
      Fuente = "TEXT",
      Servicios = "TEXT",
      Superficie = "REAL"
    ))
  }
  dbDisconnect(db)
}

# Inicializar la base de datos al inicio de la aplicación
init_db()

# --- UI (Interfaz de Usuario) ---
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      html, body {
        height: 100%;
        margin: 0;
        padding: 0;
        overflow: hidden;
      }
      .container-fluid {
        height: 100%;
        display: flex;
        flex-direction: column;
      }
      .shiny-title-panel {
        flex-shrink: 0;
      }
      /* Estilos para el contenedor del control Leaflet que contiene el combobox */
      /* Estos márgenes lo separan de los bordes del mapa */
      .leaflet-top.leaflet-right .info.legend.leaflet-control {
          margin-top: 10px; /* Margen superior */
          margin-right: 10px; /* Margen derecho */
      }
      /* Estilos para el combobox dentro del control */
      .leaflet-control-select-input .form-group {
        margin-bottom: 0; /* Elimina margen inferior extra */
      }
      .leaflet-control-select-input label {
        display: block; /* Asegura que la etiqueta esté en su propia línea */
        margin-bottom: 3px; /* Pequeño margen debajo de la etiqueta */
        font-weight: bold;
        color: #333;
      }
      /* Estilo para que el fondo del control sea semitransparente */
      .info.legend {
        background: rgba(255,255,255,0.8);
        padding: 6px 8px;
        font: 14px/16px Arial, Helvetica, sans-serif;
        box-shadow: 0 0 15px rgba(0,0,0,0.2);
        border-radius: 5px;
      }
      /* --- NUEVOS ESTILOS PARA EL FOOTER PERSONALIZADO --- */
      .app-footer-personalizado {
        width: 100%;
        text-align: center;
        padding: 10px 0;
        background-color: #f0f0f0; /* Un color de fondo claro */
        border-top: 1px solid #e0e0e0; /* Un borde superior sutil */
        flex-shrink: 0; /* Evita que el footer se encoja */
        box-sizing: border-box; /* Asegura que padding y border se incluyan en el width/height */
        font-size: 0.9em;
        color: #555;
      }
      .app-footer-personalizado a {
        color: #007bff; /* Color azul para el enlace */
        text-decoration: none; /* Sin subrayado por defecto */
      }
      .app-footer-personalizado a:hover {
        text-decoration: underline; /* Subrayado al pasar el mouse */
      }
    "))
  ),

  titlePanel("Carga de datos - Mapa de Valores del Suelo de América Latina"),

  leafletOutput("map", height = "100%"), # El mapa toma el espacio restante

  # --- NUEVO: Pie de página con el texto y enlace ---
  tags$footer(class = "app-footer-personalizado",
              HTML("Desarrollado por Juan Pablo Carranza (<a href='https://github.com/carranzajuanp/app_mapa_latam' target='_blank'>https://github.com/carranzajuanp/app_mapa_latam</a>)")
  )
)

# --- SERVER (Lógica del Servidor) ---
server <- function(input, output, session) {

  output$map <- renderLeaflet({
    # Conectar a la base de datos y cargar los datos existentes
    db <- dbConnect(SQLite(), dbname = db_path)
    existing_data <- dbReadTable(db, table_name)
    dbDisconnect(db)

    # Crear el mapa base
    m <- leaflet() %>%
      addProviderTiles("OpenStreetMap", group = "base_map_tiles") %>% # Por defecto inicia con OpenStreetMap
      setView(lng = -60, lat = -20, zoom = 4) %>%
      addMiniMap() %>%
      # --- Agregar el selectInput directamente en addControl ---
      addControl(
        html = tags$div(
          # Le damos un ID específico al contenedor del control para apuntar con JS
          id = "base_map_control_container",
          class = "info legend leaflet-control-select-input", # Clases para estilo y agrupación
          selectInput("base_map_selector", "Mapa Base:",
                      choices = c(
                        "OpenStreetMap" = "OpenStreetMap",
                        "Satélite" = "Esri.WorldImagery",
                        "CartoDB Positron (Claro)" = "CartoDB.Positron",
                        "OpenTopoMap (Topográfico)" = "OpenTopoMap"
                      ),
                      selected = "OpenStreetMap",
                      width = "180px" # Ancho fijo para el combobox dentro del mapa
          )
        ),
        position = "topright" # Posiciona el control en la esquina superior derecha del mapa
      ) %>%
      # --- Usar htmlwidgets::onRender para JavaScript ---
      htmlwidgets::onRender("
        function(el, x) {
          // 'el' es el elemento DOM contenedor del mapa
          // Buscamos nuestro control por el ID que le asignamos
          var controlContainer = document.getElementById('base_map_control_container');

          if (controlContainer) {
            // L es el objeto global de Leaflet, que tiene L.DomEvent
            L.DomEvent.disableClickPropagation(controlContainer);
            L.DomEvent.disableScrollPropagation(controlContainer); // También evita el zoom involuntario con la rueda del ratón
          }
        }
      ")

    # Si hay datos existentes, añadir los marcadores al mapa inicial
    if (nrow(existing_data) > 0) {
      m <- m %>%
        addMarkers(
          data = existing_data,
          lng = ~Longitud,
          lat = ~Latitud,
          popup = ~paste0("<b>Valor:</b> $", format(Valor, big.mark = ".", decimal.mark = ","), "<br>",
                          "<b>Superficie:</b> ", Superficie, " m²<br>",
                          "<b>Fuente:</b> ", Fuente, "<br>",
                          "<b>Servicios:</b> ", Servicios)
        )
    }

    return(m) # Devolver el objeto mapa
  })

  # Observar cambios en el selector de mapa base
  # Este observador se mantiene igual, ya que el selectInput tiene el mismo ID
  observeEvent(input$base_map_selector, {
    leafletProxy("map") %>%
      clearGroup("base_map_tiles") %>% # Eliminar las capas de mosaicos del grupo "base_map_tiles"
      addProviderTiles(input$base_map_selector, group = "base_map_tiles") # Añadir las nuevas capas de mosaicos
  })

  # Observar los clics en el mapa
  observeEvent(input$map_click, {
    click <- input$map_click

    # Esta verificación es una capa extra de seguridad,
    # pero el JS de disableClickPropagation es la solución principal.
    if (!is.null(click$id) && startsWith(click$id, "Leaflet.Control")) {
      return() # Ignorar clics en controles Leaflet si tuvieran IDs específicos
    }

    new_id <- UUIDgenerate()
    current_date <- Sys.Date()

    showModal(modalDialog(
      title = "Ingresar Datos del Punto",
      fluidRow(
        column(12,
               numericInput("form_valor", "Valor:", value = NA, min = 0),
               selectInput("form_fuente", "Fuente:",
                           choices = c("", "Oferta publicada", "Tasación", "Venta"),
                           selected = ""),
               checkboxGroupInput("form_servicios", "Servicios:",
                                  choices = c("Agua", "Luz", "Gas de red", "Cloacas", "Asfalto")),
               numericInput("form_superficie", "Superficie (m²):", value = NA, min = 0)
        )
      ),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("save_data", "Guardar Datos")
      )
    ))

    rv_temp_data$current_click_data <- list(
      id = new_id,
      lat = click$lat,
      lng = click$lng,
      date = current_date
    )
  })

  rv_temp_data <- reactiveValues(current_click_data = NULL)

  observeEvent(input$save_data, {
    req(input$form_valor, input$form_superficie, rv_temp_data$current_click_data)

    click_data <- rv_temp_data$current_click_data

    new_row <- data.frame(
      ID = click_data$id,
      Latitud = click_data$lat,
      Longitud = click_data$lng,
      Valor = input$form_valor,
      Fecha = format(click_data$date, "%Y-%m-%d"),
      Fuente = input$form_fuente,
      Servicios = paste(input$form_servicios, collapse = ", "),
      Superficie = input$form_superficie,
      stringsAsFactors = FALSE
    )

    db <- dbConnect(SQLite(), dbname = db_path)
    dbAppendTable(db, table_name, new_row)
    dbDisconnect(db)

    leafletProxy("map") %>%
      addMarkers(
        lng = click_data$lng,
        lat = click_data$lat,
        popup = paste0("<b>Valor:</b> $", format(input$form_valor, big.mark = ".", decimal.mark = ","), "<br>",
                       "<b>Superficie:</b> ", input$form_superficie, " m²<br>",
                       "<b>Fuente:</b> ", input$form_fuente, "<br>",
                       "<b>Servicios:</b> ", paste(input$form_servicios, collapse = ", "))
      )

    removeModal()
    rv_temp_data$current_click_data <- NULL
  })

}

# --- Lanzar la aplicación Shiny ---
shinyApp(ui = ui, server = server)


# library(shiny)
# library(leaflet)
# library(DBI)
# library(RSQLite)
# library(dplyr)
# library(shinymanager)
# library(sodium) # Necesario para password_store y password_verify
# library(htmlwidgets)
# 
# # Inicializar base de datos SQLite
# init_db <- function(db_path) {
#   db <- dbConnect(SQLite(), dbname = db_path)
#   if (!dbExistsTable(db, "puntos")) {
#     dbExecute(db, "CREATE TABLE puntos (id INTEGER PRIMARY KEY, lat REAL, lng REAL)")
#   }
#   if (!dbExistsTable(db, "usuarios")) {
#     dbExecute(db, "CREATE TABLE usuarios (user TEXT PRIMARY KEY, password TEXT, admin INTEGER, comment TEXT)")
#     # Insertar usuario administrador por defecto solo si la tabla está vacía
#     if (dbGetQuery(db, "SELECT COUNT(*) FROM usuarios")[1,1] == 0) {
#       dbExecute(db, "INSERT INTO usuarios (user, password, admin, comment) VALUES (?, ?, ?, ?)",
#                 params = list("admin", password_store("admin123"), 1, "Usuario administrador"))
#     }
#   }
#   dbDisconnect(db)
# }
# 
# db_path <- "puntos.db"
# init_db(db_path)
# 
# # Leer credenciales desde SQLite
# load_credentials <- function(db_path) {
#   db <- dbConnect(SQLite(), dbname = db_path)
#   usuarios <- dbReadTable(db, "usuarios")
#   dbDisconnect(db)
#   # Asegurarse de que el password sea carácter para sodium y admin sea lógico para shinymanager
#   usuarios$password <- as.character(usuarios$password)
#   usuarios$admin <- as.logical(usuarios$admin)
#   return(usuarios)
# }
# 
# # Función personalizada para autenticación
# # Función personalizada para autenticación
# check_credentials_sqlite <- function(user, password) {
#   print(paste0("Intentando autenticar usuario: ", user)) # Diagnóstico
#   db <- dbConnect(SQLite(), dbname = db_path)
#   usuarios <- dbReadTable(db, "usuarios")
#   dbDisconnect(db)
#   
#   row <- usuarios[usuarios$user == user, ]
#   if (nrow(row) == 1) {
#     stored_password_hash <- row$password
#     print(paste0("Contraseña almacenada para ", user, ": ", stored_password_hash)) # Diagnóstico
#     print(paste0("Contraseña ingresada (sin hashear para verificación): ", password)) # Diagnóstico
#     
#     if (password_verify(stored_password_hash, password)) {
#       print("¡Autenticación exitosa!") # Diagnóstico
#       return(list(
#         user = user,
#         admin = as.logical(row$admin), # Asegurarse de que sea lógico
#         comment = row$comment
#       ))
#     } else {
#       print("Contraseña no coincide.") # Diagnóstico
#       return(NULL)
#     }
#   } else {
#     print("Usuario no encontrado.") # Diagnóstico
#     return(NULL)
#   }
# }
# 
# # UI principal sin login
# ui_main <- fluidPage(
#   tags$style(HTML("
#     html, body, .container-fluid {
#       height: 100%;
#       margin: 0;
#       padding: 0;
#       overflow: hidden;
#     }
#     .leaflet, #map {
#       height: calc(100vh - 110px); /* Ajustar la altura del mapa para dejar espacio al footer */
#     }
#     .app-footer-personalizado {
#       width: 100%;
#       text-align: center;
#       padding: 10px 0;
#       background-color: #f0f0f0;
#       border-top: 1px solid #e0e0e0;
#       flex-shrink: 0;
#       font-size: 0.9em;
#       color: #555;
#       position: absolute; /* Posicionamiento absoluto para el footer */
#       bottom: 0;
#       left: 0;
#       z-index: 1000; /* Asegurar que esté por encima del mapa */
#     }
#     .app-footer-personalizado a {
#       color: #007bff;
#       text-decoration: none;
#     }
#     .app-footer-personalizado a:hover {
#       text-decoration: underline;
#     }
#   ")),
#   
#   leafletOutput("map", height = "100%"), # El mapa usará el 100% del espacio disponible, ajustado por CSS
#   
#   # Panel condicional para la gestión de usuarios (solo visible para admin)
#   conditionalPanel(
#     condition = "output.user_is_admin",
#     absolutePanel(
#       top = 10, right = 10, width = 300,
#       draggable = TRUE,
#       wellPanel(
#         h4("Gestión de Usuarios"),
#         textInput("new_user", "Nuevo usuario"),
#         passwordInput("new_pass", "Contraseña"),
#         checkboxInput("new_admin", "Administrador", FALSE),
#         textInput("new_comment", "Comentario"),
#         actionButton("add_user", "Agregar usuario"),
#         hr(),
#         DT::dataTableOutput("tabla_usuarios")
#       ),
#       style = "opacity: 0.92; z-index: 100;"
#     )
#   ),
#   
#   tags$footer(class = "app-footer-personalizado",
#               HTML("Desarrollado por Juan Pablo Carranza | <a href='https://github.com/carranzajuanp' target='_blank'>GitHub</a>")
#   )
# )
# 
# ui <- secure_app(ui_main)
# 
# server <- function(input, output, session) {
#   
#   # Autenticación: corregido para pasar la función directamente
#   res_auth <- secure_server(
#     check_credentials = check_credentials_sqlite
#   )
#   
#   # Reactivo para determinar si el usuario logueado es admin
#   output$user_is_admin <- reactive({
#     # res_auth$result contiene el resultado de la autenticación
#     # Es una lista si la autenticación fue exitosa, o NULL
#     if (!is.null(res_auth$result)) {
#       return(res_auth$result$admin)
#     } else {
#       return(FALSE)
#     }
#   })
#   outputOptions(output, "user_is_admin", suspendWhenHidden = FALSE)
#   
#   
#   # Cargar puntos existentes desde la base
#   puntos_reactivos <- reactiveVal()
#   
#   cargar_puntos <- function() {
#     db <- dbConnect(SQLite(), dbname = db_path)
#     puntos <- dbReadTable(db, "puntos")
#     dbDisconnect(db)
#     puntos_reactivos(puntos)
#   }
#   
#   cargar_puntos()
#   
#   # Render del mapa
#   output$map <- renderLeaflet({
#     leaflet() %>%
#       addTiles(group = "OpenStreetMap") %>%
#       addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") %>%
#       addProviderTiles(providers$Esri.WorldImagery, group = "Esri Satelital") %>%
#       addLayersControl(
#         baseGroups = c("OpenStreetMap", "CartoDB Positron", "Esri Satelital"),
#         options = layersControlOptions(collapsed = TRUE)
#       ) %>%
#       setView(lng = -58.4173, lat = -34.6118, zoom = 5) %>%
#       htmlwidgets::onRender(
#         "function(el, x) {
#           var controlContainer = document.getElementById('base_map_control_container');
#           if (controlContainer) {
#             L.DomEvent.disableClickPropagation(controlContainer);
#             L.DomEvent.disableScrollPropagation(controlContainer);
#           }
#         }"
#       )
#   })
#   
#   # Agregar marcadores desde la base
#   observe({
#     puntos <- puntos_reactivos()
#     leafletProxy("map") %>% clearMarkers()
#     if (nrow(puntos) > 0) {
#       leafletProxy("map") %>% addMarkers(data = puntos, lng = ~lng, lat = ~lat)
#     }
#   })
#   
#   # Al hacer clic en el mapa, guardar coordenadas (solo si el usuario está autenticado)
#   observeEvent(input$map_click, {
#     # Solo permite agregar puntos si el usuario está logueado
#     req(res_auth$user)
#     click <- input$map_click
#     db <- dbConnect(SQLite(), dbname = db_path)
#     dbExecute(db, "INSERT INTO puntos (lat, lng) VALUES (?, ?)", params = list(click$lat, click$lng))
#     dbDisconnect(db)
#     cargar_puntos()
#   })
#   
#   # Gestión de usuarios
#   observeEvent(input$add_user, {
#     # Solo permite agregar usuarios si el usuario logueado es admin
#     req(output$user_is_admin())
#     req(input$new_user, input$new_pass)
#     
#     db <- dbConnect(SQLite(), dbname = db_path)
#     dbExecute(db, "INSERT OR REPLACE INTO usuarios (user, password, admin, comment) VALUES (?, ?, ?, ?)",
#               params = list(input$new_user, password_store(input$new_pass), as.integer(input$new_admin), input$new_comment))
#     dbDisconnect(db)
#     # Actualizar la tabla de usuarios después de añadir uno nuevo
#     output$tabla_usuarios <- DT::renderDataTable({
#       load_credentials(db_path)
#     })
#     # Limpiar campos después de agregar
#     updateTextInput(session, "new_user", value = "")
#     updatePasswordInput(session, "new_pass", value = "")
#     updateCheckboxInput(session, "new_admin", value = FALSE)
#     updateTextInput(session, "new_comment", value = "")
#     
#     showNotification("Usuario agregado/actualizado exitosamente.", type = "message")
#   })
#   
#   # Tabla de usuarios
#   output$tabla_usuarios <- DT::renderDataTable({
#     # Esto se renderizará automáticamente cuando se cargue la app o se añada un usuario
#     load_credentials(db_path)
#   })
# }
# 
# shinyApp(ui = ui, server = server)
