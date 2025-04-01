# app.R
library(shiny)
library(sf)
library(leaflet)
library(dplyr)
library(stringr)
library(htmltools)
library(RColorBrewer)
library(mapview) # Pour exporter en PDF
library(webshot2) # Pour l'export PDF
library(rsconnect)
library(mapview)

# Chemins des dossiers et fichiers
path_indicateurs_saisonniers <- "Data/INDICATEURS_SAISONNIERS_ETE/"
path_indicateurs_annuels <- "Data/INDICATEURS_ANNUELS_HORIZONS"
path_feux_indicateurs <- "Data/FEUX_INDICATEURS_ANNUELS_HORIZONS"
path_agri_indicateurs <- "Data/AGRI_INDICATEURS_ANNUELS_HORIZONS"
path_descriptions <- "Data/noms_variables.txt"
path_communes <- "Data/Communes/codes_postaux_region.shp" # Chemin vers le fichier des communes

# Définition des périodes des horizons
horizon_periods <- list(
  "REF" = "Référence",
  "H1" = "2021-2050",
  "H2" = "2041-2070",
  "H3" = "2071-2100"
)

# Lecture des descriptions de variables
read_descriptions <- function(file_path) {
  # Vérification si le fichier existe
  if (!file.exists(file_path)) {
    warning("Fichier de descriptions non trouvé: ", file_path)
    return(list())
  }
  
  lines <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
  desc_list <- list()
  
  for (line in lines) {
    # Recherche de patterns comme [CODE]: Description
    pattern <- "\\[(.*?)\\]:\\s*(.*)"
    if (grepl(pattern, line)) {
      var_code <- gsub(pattern, "\\1", line)
      var_desc <- gsub(pattern, "\\2", line)
      desc_list[[var_code]] <- var_desc
    }
  }
  
  return(desc_list)
}

# Fonction pour obtenir les fichiers gpkg d'un dossier
get_gpkg_files <- function(folder_path) {
  if (!dir.exists(folder_path)) {
    warning("Dossier non trouvé: ", folder_path)
    return(character(0))
  }
  
  files <- list.files(folder_path, pattern = "\\.gpkg$", full.names = TRUE)
  return(files)
}

# Fonction pour extraire le scénario du nom de fichier
extract_scenario <- function(file_path) {
  file_name <- basename(file_path)
  
  # Correspondance de motifs pour différents formats de noms de fichiers
  if (grepl("REFERENCE", file_name, ignore.case = TRUE)) {
    return("REFERENCE")
  } else if (grepl("_2_6_", file_name)) {
    return("Scénario 2.6")
  } else if (grepl("_4_5_", file_name)) {
    return("Scénario 4.5")
  } else if (grepl("_8_5_", file_name)) {
    return("Scénario 8.5")
  } else {
    return("Inconnu")
  }
}

# Fonction pour extraire les horizons disponibles à partir des colonnes
extract_horizons <- function(data) {
  col_names <- colnames(data)
  # Recherche les colonnes se terminant par _REF, _H1, _H2, _H3
  horizons <- unique(c(
    if(any(grepl("_REF$", col_names))) "REF",
    if(any(grepl("_H1$", col_names))) "H1", 
    if(any(grepl("_H2$", col_names))) "H2",
    if(any(grepl("_H3$", col_names))) "H3"
  ))
  return(horizons)
}

# Fonction pour obtenir les variables disponibles pour un horizon donné
get_variables_for_horizon <- function(data, horizon) {
  col_names <- colnames(data)
  # Recherche les colonnes se terminant par l'horizon spécifié
  vars <- col_names[grepl(paste0("_", horizon, "$"), col_names)]
  # Extraction des noms de variables sans le suffixe _Hn
  vars <- gsub(paste0("_", horizon, "$"), "", vars)
  # Exclure les colonnes non-variables (geom, index_original, etc.)
  vars <- vars[!vars %in% c("geom", "index_original")]
  return(vars)
}

# Charger le shapefile des communes et préparer le spatial join
load_communes <- function(path_communes) {
  if (!file.exists(path_communes)) {
    warning("Fichier de communes non trouvé: ", path_communes)
    return(NULL)
  }
  
  tryCatch({
    # Lire le shapefile avec st_read en supprimant les NA
    communes <- st_read(path_communes, quiet = TRUE, stringsAsFactors = FALSE, options = "ENCODING=UTF-8")
    
    # Vérifier si les données sont vides
    if (nrow(communes) == 0) {
      warning("Le fichier des communes est vide")
      return(NULL)
    }
    
    # S'assurer que toutes les géométries sont valides, avec gestion d'erreur
    communes <- suppressWarnings(st_make_valid(communes))
    
    # Ajouter un index corrigé pour la jointure
    communes$index_corrected <- seq_len(nrow(communes))
    
    # Transformer en WGS84 pour Leaflet avec gestion d'erreur
    if (!is.na(st_crs(communes)$wkt) && st_crs(communes)$epsg != 4326) {
      communes <- suppressWarnings(st_transform(communes, 4326))
    } else if (is.na(st_crs(communes)$wkt)) {
      communes <- suppressWarnings(st_set_crs(communes, 2154))
      communes <- suppressWarnings(st_transform(communes, 4326))
    }
    
    return(communes)
  }, error = function(e) {
    warning("Erreur lors de la lecture du fichier des communes: ", e$message)
    return(NULL)
  })
}

# Définir l'interface utilisateur - Suppression de la fonctionnalité BoxZoom
ui <- fluidPage(
  titlePanel("Visualisation des Données DRIAS"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("theme", "Thème:", 
                  choices = c("DRIAS - Indicateurs Saisonniers" = "INDICATEURS_SAISONNIERS_ETE",
                              "DRIAS - Indicateurs Annuels" = "INDICATEURS_ANNUELS_HORIZONS",
                              "DRIAS FEUX - Indicateurs Annuels" = "FEUX_INDICATEURS_ANNUELS_HORIZONS",
                              "DRIAS AGRI - Indicateurs Annuels" = "AGRI_INDICATEURS_ANNUELS_HORIZONS")),
      
      selectInput("scenario", "Scénario:", choices = NULL),
      
      selectInput("horizon", "Horizon:", choices = NULL),
      
      selectInput("variable", "Variable:", choices = NULL),
      
      # Bouton pour confirmer les sélections et charger la carte
      actionButton("confirmChoices", "Confirmer et charger la carte", 
                   style = "margin-top: 15px; margin-bottom: 15px; width: 100%; background-color: #4CAF50; color: white; font-weight: bold;"),
      
      # Bouton pour télécharger la carte en PDF
      downloadButton("downloadPDF", "Télécharger la carte (PDF)"),
      
      width = 3
    ),
    
    mainPanel(
      leafletOutput("map", height = "800px"),
      width = 9
    )
  )
)

# Définir le serveur - Suppression des popups et BoxZoom
server <- function(input, output, session) {
  
  # Charger les descriptions de variables dès le démarrage
  var_descriptions <- reactiveVal(read_descriptions(path_descriptions))
  
  # Obtenir le chemin du dossier en fonction du thème sélectionné
  selected_folder_path <- reactive({
    theme_folders <- list(
      "INDICATEURS_SAISONNIERS_ETE" = path_indicateurs_saisonniers,
      "INDICATEURS_ANNUELS_HORIZONS" = path_indicateurs_annuels,
      "FEUX_INDICATEURS_ANNUELS_HORIZONS" = path_feux_indicateurs, 
      "AGRI_INDICATEURS_ANNUELS_HORIZONS" = path_agri_indicateurs
    )
    return(theme_folders[[input$theme]])
  })
  
  # Initialiser les scénarios dès le démarrage
  observe({
    folder_path <- selected_folder_path()
    gpkg_files <- get_gpkg_files(folder_path)
    
    if (length(gpkg_files) > 0) {
      scenarios <- sapply(gpkg_files, extract_scenario)
      names(scenarios) <- gpkg_files
      updateSelectInput(session, "scenario", choices = unique(scenarios))
    } else {
      updateSelectInput(session, "scenario", choices = character(0))
    }
  }, priority = 1)
  
  # Charger les données en fonction du thème et du scénario
  raw_data <- reactive({
    req(input$scenario)
    folder_path <- selected_folder_path()
    gpkg_files <- get_gpkg_files(folder_path)
    
    # Aucun fichier trouvé
    if (length(gpkg_files) == 0) {
      return(NULL)
    }
    
    # Trouver le fichier correspondant au scénario sélectionné
    scenario_files <- names(which(sapply(gpkg_files, extract_scenario) == input$scenario))
    
    if (length(scenario_files) > 0) {
      # Charger les données du premier fichier correspondant
      tryCatch({
        # Lecture avec transformation EPSG:4326 (WGS84) pour Leaflet
        data <- st_read(scenario_files[1], quiet = TRUE)
        
        # Ajouter un index pour la jointure
        data$index_original <- seq_len(nrow(data))
        
        # Vérifier et transformer la projection si nécessaire
        if (!is.na(st_crs(data)$wkt) && st_crs(data)$epsg != 4326) {
          data <- st_transform(data, 4326)
        } else if (is.na(st_crs(data)$wkt)) {
          # Si la projection n'est pas définie, assigner une projection (souvent Lambert-93 pour la France)
          data <- st_set_crs(data, 2154)
          data <- st_transform(data, 4326)
        }
        
        return(data)
      }, error = function(e) {
        warning("Erreur lors de la lecture du fichier: ", e$message)
        return(NULL)
      })
    } else {
      return(NULL)
    }
  })
  
  # Données sélectionnées qui ne seront actualisées que lors de la confirmation
  selected_data <- reactiveVal(NULL)
  
  # Mettre à jour les horizons dès que les données sont disponibles
  observe({
    data <- raw_data()
    if (!is.null(data)) {
      horizons <- extract_horizons(data)
      updateSelectInput(session, "horizon", choices = horizons)
    } else {
      updateSelectInput(session, "horizon", choices = character(0))
    }
  }, priority = 2)
  
  # Mettre à jour les variables disponibles dès que l'horizon est sélectionné
  observe({
    data <- raw_data()
    req(input$horizon)
    
    if (!is.null(data)) {
      variables <- get_variables_for_horizon(data, input$horizon)
      updateSelectInput(session, "variable", choices = variables)
    } else {
      updateSelectInput(session, "variable", choices = character(0))
    }
  }, priority = 3)
  
  # Initialiser la carte avec une vue sur la France plus zoomée
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      setView(lng = 4, lat = 47, zoom = 6) %>%
      addControl(
        html = tags$div(
          style = "padding: 6px 8px; background: white; border-radius: 5px; box-shadow: 0 0 15px rgba(0,0,0,0.2);",
          tags$h3("Sélectionnez les paramètres et cliquez sur 'Confirmer et charger la carte'", 
                  style = "margin: 0; text-align: center; font-weight: bold;")
        ),
        position = "topright"
      )
  })
  
  # État réactif pour la carte actuelle
  current_map <- reactiveVal(NULL)
  
  # Observer pour le bouton de confirmation
  observeEvent(input$confirmChoices, {
    # Mettre à jour les données sélectionnées
    selected_data(raw_data())
    
    # Afficher un message de chargement
    showNotification("Chargement de la carte...", type = "message", duration = 1)
    
    # Mettre à jour la carte avec les paramètres choisis
    data <- selected_data()
    req(input$horizon, input$variable)
    
    if (is.null(data)) {
      leafletProxy("map") %>%
        clearShapes() %>%
        clearControls() %>%
        addControl(
          html = tags$div(
            style = "padding: 6px 8px; background: white; border-radius: 5px; box-shadow: 0 0 15px rgba(0,0,0,0.2);",
            tags$h3("Aucune donnée disponible", style = "margin: 0; text-align: center; font-weight: bold;")
          ),
          position = "topright"
        )
      current_map(NULL)
      return()
    }
    
    # Construire le nom de colonne complet
    col_name <- paste0(input$variable, "_", input$horizon)
    
    # Vérifier si la colonne existe
    if (!(col_name %in% colnames(data))) {
      leafletProxy("map") %>%
        clearShapes() %>%
        clearControls() %>%
        addControl(
          html = tags$div(
            style = "padding: 6px 8px; background: white; border-radius: 5px; box-shadow: 0 0 15px rgba(0,0,0,0.2);",
            tags$h3("Variable non disponible pour cet horizon", style = "margin: 0; text-align: center; font-weight: bold;")
          ),
          position = "topright"
        )
      current_map(NULL)
      return()
    }
    
    # Obtenir les données de la variable sélectionnée
    values <- data[[col_name]]
    
    # Retirer les valeurs NA pour la légende
    values_for_legend <- values[!is.na(values)]
    
    # Définir la palette de couleurs en fonction du type de variable
    if (grepl("^(NORT|AT).*AV$", input$variable)) {
      # Palette pour les températures
      pal <- colorNumeric(palette = "RdYlBu", domain = values, reverse = TRUE, na.color = "transparent")
    } else if (grepl("^(NORP|AP)", input$variable)) {
      # Palette pour les précipitations
      pal <- colorNumeric(palette = "Blues", domain = values, na.color = "transparent")
    } else {
      # Palette par défaut pour les autres variables
      pal <- colorNumeric(palette = "Spectral", domain = values, reverse = FALSE, na.color = "transparent")
    }
    
    # Obtenir la description de la variable
    descriptions <- var_descriptions()
    var_desc <- descriptions[[input$variable]]
    if (is.null(var_desc) || var_desc == "") {
      var_desc <- "Description non disponible"
    }
    
    # Créer le titre avec l'horizon et sa période
    horizon_period <- horizon_periods[[input$horizon]]
    title <- paste0(
      input$variable, " - ", var_desc, "<br>",
      "<span style='font-size: 0.9em;'>", input$scenario, " - Horizon: ", input$horizon, 
      if(!is.null(horizon_period)) paste0(" (", horizon_period, ")") else "", "</span>"
    )
    
    # Mettre à jour la carte sans redessiner complètement
    leafletProxy("map", data = data) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        fillColor = ~pal(data[[col_name]]),
        fillOpacity = 1.0,
        color = "#444444",
        weight = 0.5,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        # Ajout des popups qui s'affichent au clic
        popup = ~paste0(
          "<strong>Valeur:</strong> ", 
          ifelse(is.na(data[[col_name]]), "Non disponible", round(data[[col_name]], 2)),
          "<br><strong>Variable:</strong> ", input$variable
        ),
        # Ajout des labels qui s'affichent au survol
        label = ~paste0(
          "Valeur: ", 
          ifelse(is.na(data[[col_name]]), "Non disponible", round(data[[col_name]], 2))
        )
      ) %>%
      addLegend(
        position = "bottomleft",
        pal = pal,
        values = values_for_legend,
        title = input$variable,
        opacity = 1.0
      ) %>%
      addControl(
        html = tags$div(
          style = "padding: 8px 12px; background: white; border-radius: 5px; box-shadow: 0 0 15px rgba(0,0,0,0.2); min-width: 200px; max-width: 600px; margin: 0 auto; position: relative; left: 50%; transform: translateX(-50%);",
          HTML(paste0("<h3 style='margin: 0; text-align: center; font-weight: bold;'>", title, "</h3>"))
        ),
        position = "topright"
      )
    
    # Stocker la carte mise à jour
    map_data <- list(
      data = data,
      col_name = col_name,
      pal = pal,
      title = title,
      values = values_for_legend
    )
    current_map(map_data)
  })
  
  # Téléchargement de la carte en PDF
  output$downloadPDF <- downloadHandler(
    filename = function() {
      paste0("carte-", input$theme, "-", input$scenario, "-", input$horizon, "-", input$variable, ".pdf")
    },
    content = function(file) {
      # Vérifier si une carte valide est disponible
      if (is.null(current_map())) {
        # Créer un PDF avec un message d'erreur si aucune carte n'est disponible
        pdf(file, width = 11, height = 8.5)
        plot.new()
        text(0.5, 0.5, "Aucune carte disponible à exporter", cex = 1.5)
        dev.off()
        return()
      }
      
      # Récupérer les données de la carte actuelle
      map_data <- current_map()
      
      # Créer une nouvelle carte pour l'export
      export_map <- leaflet(map_data$data) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~map_data$pal(map_data$data[[map_data$col_name]]),
          fillOpacity = 1.0,
          color = "#444444", 
          weight = 0.5
        ) %>%
        addLegend(
          position = "bottomleft",
          pal = map_data$pal,
          values = map_data$values,
          title = input$variable,
          opacity = 1.0
        ) %>%
        addControl(
          html = tags$div(
            style = "padding: 8px 12px; background: white; border-radius: 5px; box-shadow: 0 0 15px rgba(0,0,0,0.2);",
            HTML(paste0("<h3 style='margin: 0; text-align: center; font-weight: bold;'>", map_data$title, "</h3>"))
          ),
          position = "topright"
        ) %>%
        setView(lng = 2.2137, lat = 46.2276, zoom = 6)
      
      # Exporter en PDF avec mapview et webshot2
      mapview::mapshot(export_map, file = file, selfcontained = FALSE)
    }
  )
}

# Lancer l'application
shinyApp(ui = ui, server = server)