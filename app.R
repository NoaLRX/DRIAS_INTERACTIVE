# FINAL APP
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
library(remotes)
library(raster)
library(shinydashboard)  # Pour les onglets et l'interface plus élaborée
library(openxlsx)  # Pour la gestion des fichiers Excel
library(htmlwidgets)  # Pour l'export des cartes au format HTML
library(httr)  # Pour les requêtes API
library(jsonlite)  # Pour parser les réponses JSON
library(ggplot2)  # Pour les graphiques de diagnostic
library(tidyr)  # Pour la manipulation des données
library(cowplot)  # Pour la mise en page des graphiques
library(readxl)  # Pour lire les fichiers Excel
library(base64enc)  # Pour encoder les images en base64
library(gridExtra)  # Pour combiner les graphiques

# Charger la fonction pour générer le diagnostic PDF
source("wrapper.R")

# Chemins des dossiers et fichiers
path_indicateurs_saisonniers <- "Data/INDICATEURS_SAISONNIERS_ETE/Resultats/"
path_indicateurs_annuels <- "Data/INDICATEURS_ANNUELS_HORIZONS/Resultats/"
path_feux_indicateurs <- "Data/FEUX_INDICATEURS_ANNUELS_HORIZONS/Resultats/"
path_agri_indicateurs <- "Data/AGRI_INDICATEURS_ANNUELS_HORIZONS/Resultats/"
path_descriptions <- "Data/noms_variables.txt"
path_communes <- "Data/Communes/codes_postaux_region.shp"
# Dossier pour stocker les fichiers de cache
path_cache <- "Data/cache"

# Créer le dossier de cache s'il n'existe pas
if (!dir.exists(path_cache)) {
  dir.create(path_cache, recursive = TRUE)
}

# Définition des périodes des horizons avec noms complets
horizon_periods <- list(
  "REF" = "Référence",
  "H1" = "2021-2050",
  "H2" = "2041-2070",
  "H3" = "2071-2100"
)

# Définition des noms complets des horizons
horizon_full_names <- list(
  "REF" = "REF : Période de Référence",
  "H1" = "H1 : Horizon proche [2021-2050]",
  "H2" = "H2 : Horizon Moyen [2041-2070]",
  "H3" = "H3 : Horizon Lointain [2071-2100]"
)

# Définition des noms complets des scénarios
scenario_full_names <- list(
  "REFERENCE" = "REFERENCE",
  "Scénario RCP 2.6: Émissions maitrisées " = "RCP 2.6",
  "Scénario RCP 4.5: Émissions modérées" = "RCP 4.5",
  "Scénario RCP 8.5: Émissions non réduites" = "RCP 8.5",
  "Inconnu" = "Inconnu"
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

# Fonction pour extraire le scénario du nom de fichier
extract_scenario <- function(file_path) {
  file_name <- basename(file_path)
  
  # Correspondance de motifs pour différents formats de noms de fichiers
  if (grepl("REFERENCE", file_name, ignore.case = TRUE)) {
    return("REFERENCE")
  } else if (grepl("_2_6_", file_name)) {
    return("Scénario RCP 2.6: Émissions maitrisées ")
  } else if (grepl("_4_5_", file_name)) {
    return("Scénario RCP 4.5: Émissions modérées")
  } else if (grepl("_8_5_", file_name)) {
    return("Scénario RCP 8.5: Émissions non réduites")
  } else {
    return("Inconnu")
  }
}

# Fonction pour obtenir les fichiers gpkg d'un dossier avec filtre sur le format spatial
get_gpkg_files <- function(folder_path, use_departments = FALSE) {
  if (!dir.exists(folder_path)) {
    warning("Dossier non trouvé: ", folder_path)
    return(character(0))
  }
  
  # Filtre pour les fichiers selon le format spatial choisi
  spatial_pattern <- if(use_departments) "_DEPARTEMENTS\\.gpkg$" else "_COMMUNES\\.gpkg$"
  files <- list.files(folder_path, pattern = spatial_pattern, full.names = TRUE)
  
  # Si aucun fichier trouvé avec le pattern spécifique, retourner une liste vide
  # au lieu d'essayer sans le pattern
  return(files)
}

# Nouvelle fonction pour gérer le cache des données géospatiales
get_cached_data <- function(file_path, transform_to_4326 = TRUE) {
  # Créer un nom de fichier unique pour le cache basé sur le chemin du fichier original
  cache_file_name <- gsub("[^a-zA-Z0-9]", "_", basename(file_path))
  cache_file_path <- file.path(path_cache, paste0(cache_file_name, ".rds"))
  
  # Vérifier si le fichier cache existe
  if (file.exists(cache_file_path)) {
    # Vérifier si le fichier cache est plus récent que le fichier original
    if (file.info(cache_file_path)$mtime > file.info(file_path)$mtime) {
      message("Chargement depuis le cache: ", basename(cache_file_path))
      return(readRDS(cache_file_path))
    }
  }
  
  # Si pas de cache valide, charger et traiter les données
  message("Chargement et traitement du fichier: ", basename(file_path))
  tryCatch({
    # Lecture avec transformation EPSG:4326 (WGS84) pour Leaflet
    data <- st_read(file_path, quiet = TRUE)
    
    # Ajouter un index pour la jointure
    data$index_original <- seq_len(nrow(data))
    
    # Vérifier et transformer la projection si nécessaire et demandé
    if (transform_to_4326) {
      if (!is.na(st_crs(data)$wkt) && st_crs(data)$epsg != 4326) {
        data <- st_transform(data, 4326)
      } else if (is.na(st_crs(data)$wkt)) {
        # Si la projection n'est pas définie, assigner une projection (souvent Lambert-93 pour la France)
        data <- st_set_crs(data, 2154)
        data <- st_transform(data, 4326)
      }
    }
    
    # Sauvegarder dans le cache
    saveRDS(data, cache_file_path)
    message("Données sauvegardées dans le cache: ", basename(cache_file_path))
    
    return(data)
  }, error = function(e) {
    warning("Erreur lors de la lecture du fichier: ", e$message)
    return(NULL)
  })
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
get_variables_for_horizon <- function(data, horizon, var_descriptions) {
  col_names <- colnames(data)
  # Recherche les colonnes se terminant par l'horizon spécifié
  vars <- col_names[grepl(paste0("_", horizon, "$"), col_names)]
  # Extraction des noms de variables sans le suffixe _Hn
  vars <- gsub(paste0("_", horizon, "$"), "", vars)
  # Exclure les colonnes non-variables (geom, index_original, etc.)
  vars <- vars[!vars %in% c("geom", "index_original")]
  
  # Créer un vecteur nommé pour le menu déroulant avec codes et descriptions
  vars_named <- vars
  names(vars_named) <- sapply(vars, function(var) {
    desc <- var_descriptions[[var]]
    if (!is.null(desc) && desc != "") {
      paste0(var, " - ", desc)
    } else {
      var
    }
  })
  
  return(vars_named)
}

# Charger le shapefile des communes et préparer le spatial join
load_communes <- function(path_communes) {
  if (!file.exists(path_communes)) {
    warning("Fichier de communes non trouvé: ", path_communes)
    return(NULL)
  }
  
  print(paste("Chargement du shapefile des communes:", path_communes))
  
  tryCatch({
    # Lire le shapefile avec st_read en supprimant les NA
    communes <- st_read(path_communes, quiet = TRUE, stringsAsFactors = FALSE, options = "ENCODING=UTF-8")
    
    # Informations sur les communes chargées
    print(paste("Nombre de communes chargées:", nrow(communes)))
    print(paste("Colonnes disponibles:", paste(colnames(communes), collapse = ", ")))
    print(paste("CRS original:", st_crs(communes)$epsg))
    
    # Vérifier si les données sont vides
    if (nrow(communes) == 0) {
      warning("Le fichier des communes est vide")
      return(NULL)
    }
    
    # S'assurer que toutes les géométries sont valides, avec gestion d'erreur
    print("Validation des géométries...")
    communes <- suppressWarnings(st_make_valid(communes))
    
    # Ajouter un index corrigé pour la jointure
    communes$index_corrected <- seq_len(nrow(communes))
    
    # Transformer en WGS84 pour Leaflet avec gestion d'erreur
    print("Transformation en WGS84 (EPSG:4326)...")
    if (!is.na(st_crs(communes)$wkt) && st_crs(communes)$epsg != 4326) {
      communes <- suppressWarnings(st_transform(communes, 4326))
    } else if (is.na(st_crs(communes)$wkt)) {
      print("CRS non défini, assignation de EPSG:2154 (Lambert-93)...")
      communes <- suppressWarnings(st_set_crs(communes, 2154))
      communes <- suppressWarnings(st_transform(communes, 4326))
    }
    
    print(paste("CRS final:", st_crs(communes)$epsg))
    
    # Vérifier si le shapefile contient des informations essentielles
    has_code <- any(c("CODE_INSEE", "INSEE_COM", "CODE_C") %in% colnames(communes))
    has_name <- any(c("NOM_COMMUNE", "NOM_COM", "LIB") %in% colnames(communes))
    
    if (!has_code || !has_name) {
      warning("Le shapefile ne contient pas les colonnes nécessaires pour les codes ou noms de communes")
      print(paste("Colonnes manquantes - Code:", !has_code, "Nom:", !has_name))
    }
    
    return(communes)
  }, error = function(e) {
    warning("Erreur lors de la lecture du fichier des communes: ", e$message)
    return(NULL)
  })
}

# Fonction pour détecter la commune à partir des coordonnées GPS avec un fichier GPKG
detect_commune_from_gpkg <- function(lon, lat, gpkg_file) {
  print(paste("Détection de commune dans", gpkg_file, "pour:", lon, lat))
  
  # Vérifier que les coordonnées sont dans des limites raisonnables pour la France
  if (is.na(lon) || is.na(lat) || lon < -5.5 || lon > 10 || lat < 41 || lat > 52) {
    print("Coordonnées hors des limites de la France métropolitaine")
    return(NULL)
  }
  
  # Nom du fichier de cache
  gpkg_basename <- basename(gpkg_file)
  cache_filename <- paste0("communes_", gsub("[^a-zA-Z0-9]", "_", gpkg_basename), ".rds")
  cache_filepath <- file.path(path_cache, cache_filename)
  
  # Essayer de charger depuis le cache
  commune_sf <- NULL
  if (file.exists(cache_filepath)) {
    print("Chargement des communes depuis le cache...")
    tryCatch({
      commune_sf <- readRDS(cache_filepath)
      print(paste("Chargé", nrow(commune_sf), "communes depuis le cache"))
    }, error = function(e) {
      print(paste("Erreur lors du chargement du cache:", e$message))
      commune_sf <- NULL
    })
  }
  
  # Si pas de cache, charger depuis le fichier GPKG
  if (is.null(commune_sf)) {
    print("Chargement des communes depuis le fichier GPKG...")
    tryCatch({
      commune_sf <- sf::st_read(gpkg_file, quiet = TRUE)
      print(paste("Chargé", nrow(commune_sf), "communes depuis GPKG"))
      
      # Trouver les colonnes de code et nom commune
      code_column <- NULL
      name_column <- NULL
      
      # Rechercher des colonnes possibles pour le code
      for (col_name in c("CODE_C", "INSEE_COM", "CODE_INSEE", "ID", "CODE")) {
        if (col_name %in% colnames(commune_sf)) {
          code_column <- col_name
          print(paste("Colonne de code commune trouvée:", code_column))
          break
        }
      }
      
      # Si aucune colonne de code trouvée, créer une colonne CODE_C vide
      if (is.null(code_column)) {
        print("Aucune colonne de code commune trouvée, création d'une colonne CODE_C")
        commune_sf$CODE_C <- NA
        code_column <- "CODE_C"
      }
      
      # Rechercher des colonnes possibles pour le nom
      for (col_name in c("LIB", "NOM_COM", "NOM", "COMMUNE", "LIBELLE")) {
        if (col_name %in% colnames(commune_sf)) {
          name_column <- col_name
          print(paste("Colonne de nom commune trouvée:", name_column))
          break
        }
      }
      
      # Si aucune colonne de nom trouvée, créer une colonne LIB vide
      if (is.null(name_column)) {
        print("Aucune colonne de nom commune trouvée, création d'une colonne LIB")
        commune_sf$LIB <- NA
        name_column <- "LIB"
      }
      
      # Si la colonne s'appelle différemment de CODE_C ou LIB, créer des alias
      if (code_column != "CODE_C") {
        commune_sf$CODE_C <- commune_sf[[code_column]]
      }
      
      if (name_column != "LIB") {
        commune_sf$LIB <- commune_sf[[name_column]]
      }
      
      # S'assurer que la géométrie est valide
      print("Validation des géométries...")
      commune_sf <- sf::st_make_valid(commune_sf)
      
      # Vérifier et transformer en WGS84 si nécessaire
      print(paste("CRS original:", sf::st_crs(commune_sf)$epsg))
      if (sf::st_crs(commune_sf)$epsg != 4326) {
        print("Transformation en WGS84 (EPSG:4326)...")
        commune_sf <- sf::st_transform(commune_sf, 4326)
      }
      
      # Sauvegarder dans le cache pour utilisation future
      print("Sauvegarde des communes dans le cache...")
      dir.create(path_cache, showWarnings = FALSE, recursive = TRUE)
      saveRDS(commune_sf, cache_filepath)
      print("Communes sauvegardées dans le cache")
      
    }, error = function(e) {
      print(paste("Erreur lors du chargement du fichier GPKG:", e$message))
      return(NULL)
    })
  }
  
  if (is.null(commune_sf) || nrow(commune_sf) == 0) {
    print("Aucune donnée de commune disponible")
    return(NULL)
  }
  
  # Créer un point à partir des coordonnées (en WGS84)
  point <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)
  
  # Trouver la commune qui contient le point
  print("Recherche de la commune contenant le point...")
  commune_found <- NULL
  
  tryCatch({
    # Utiliser st_intersects pour trouver quelle commune contient le point
    intersects <- sf::st_intersects(point, commune_sf)
    
    if (length(intersects[[1]]) > 0) {
      # Récupérer la première commune qui contient le point
      commune_idx <- intersects[[1]][1]
      commune_found <- commune_sf[commune_idx, ]
      
      # Extraire les informations de la commune
      code_commune <- as.character(commune_found$CODE_C)
      commune_name <- as.character(commune_found$LIB)
      
      print(paste("Commune trouvée par intersection spatiale:", commune_name, "Code:", code_commune))
      
      return(list(
        code = code_commune,
        name = commune_name
      ))
    } else {
      print("Aucune commune ne contient ce point. Recherche de la commune la plus proche...")
      
      # Comme alternative, trouver la commune la plus proche
      dists <- sf::st_distance(point, commune_sf)
      nearest_idx <- which.min(dists)
      
      nearest_commune <- commune_sf[nearest_idx, ]
      nearest_code <- as.character(nearest_commune$CODE_C)
      nearest_name <- as.character(nearest_commune$LIB)
      
      # Calculer la distance en mètres
      min_dist <- min(dists)
      print(paste("Commune la plus proche:", nearest_name, "Code:", nearest_code, 
                 "Distance:", round(min_dist), "mètres"))
      
      # Ne retourner la commune la plus proche que si elle est à moins de 5km
      if (min_dist < 5000) {
        return(list(
          code = nearest_code,
          name = nearest_name,
          approx = TRUE,
          distance = round(min_dist)
        ))
      } else {
        print("La commune la plus proche est trop éloignée (>5km)")
        return(NULL)
      }
    }
  }, error = function(e) {
    print(paste("Erreur lors de la recherche spatiale:", e$message))
    return(NULL)
  })
  
  return(NULL)
}

# Fonction pour détecter une commune à partir de coordonnées GPS en cherchant dans tous les fichiers GPKG disponibles
find_commune_by_gps <- function(lon, lat) {
  print(paste("Recherche de commune pour les coordonnées:", lon, lat))
  
  # Chercher tous les fichiers GPKG de communes
  all_gpkg_files <- c()
  
  # Chercher dans le dossier des indicateurs saisonniers
  saisonniers_files <- list.files(path_indicateurs_saisonniers, 
                                 pattern = ".*COMMUNES.*\\.gpkg$", 
                                 recursive = TRUE, 
                                 full.names = TRUE)
  all_gpkg_files <- c(all_gpkg_files, saisonniers_files)
  
  # Chercher dans le dossier des indicateurs annuels
  annuels_files <- list.files(path_indicateurs_annuels, 
                            pattern = ".*COMMUNES.*\\.gpkg$", 
                            recursive = TRUE, 
                            full.names = TRUE)
  all_gpkg_files <- c(all_gpkg_files, annuels_files)
  
  # Chercher dans le dossier des feux
  feux_files <- list.files(path_feux_indicateurs, 
                         pattern = ".*COMMUNES.*\\.gpkg$", 
                         recursive = TRUE, 
                         full.names = TRUE)
  all_gpkg_files <- c(all_gpkg_files, feux_files)
  
  # Chercher dans le dossier agricole
  agri_files <- list.files(path_agri_indicateurs, 
                         pattern = ".*COMMUNES.*\\.gpkg$", 
                         recursive = TRUE, 
                         full.names = TRUE)
  all_gpkg_files <- c(all_gpkg_files, agri_files)
  
  # Supprimer les doublons
  all_gpkg_files <- unique(all_gpkg_files)
  
  print(paste("Nombre total de fichiers GPKG trouvés:", length(all_gpkg_files)))
  
  if (length(all_gpkg_files) == 0) {
    print("Aucun fichier GPKG de communes trouvé!")
    return(NULL)
  }
  
  # Essayer chaque fichier GPKG jusqu'à ce qu'on trouve une commune
  for (gpkg_file in all_gpkg_files) {
    print(paste("Essai avec le fichier:", gpkg_file))
    commune_info <- detect_commune_from_gpkg(lon, lat, gpkg_file)
    
    if (!is.null(commune_info)) {
      print("Commune trouvée!")
      return(commune_info)
    }
  }
  
  print("Aucune commune trouvée dans tous les fichiers GPKG testés")
  return(NULL)
}

# Définir l'interface utilisateur - Ajout de l'onglet explicatif
ui <- navbarPage(
  title = "Visualisation des Données DRIAS",
  id = "navbarPage",  # Ajout d'un ID pour permettre la navigation programmatique
  
  # Premier onglet - Carte interactive
  tabPanel(
    title = "Carte interactive 🗺️",
    
    # Ajouter un JavaScript personnalisé pour gérer la recherche d'adresse
    tags$head(
      tags$script("
        $(document).ready(function() {
          console.log('Document ready, initializing address search handlers');
          
          // Gestionnaire pour le bouton de recherche
          $(document).on('click', '#searchBtn', function(e) {
            e.preventDefault(); // Empêcher le comportement par défaut
            console.log('Search button clicked');
            var address = $('#addressInput').val() || '';
            console.log('Search address: ' + address);
            Shiny.setInputValue('searchBtnClicked', {
              address: address,
              timestamp: new Date().getTime()
            });
          });
          
          // Gestionnaire pour la touche Entrée dans le champ de recherche
          $(document).on('keyup', '#addressInput', function(e) {
            if (e.key === 'Enter') {
              e.preventDefault(); // Empêcher le comportement par défaut
              console.log('Enter key pressed in address input');
              var address = $(this).val() || '';
              console.log('Search address: ' + address);
              Shiny.setInputValue('searchBtnClicked', {
                address: address,
                timestamp: new Date().getTime()
              });
            }
          });
          
          // Gestionnaire pour les résultats de recherche
          $(document).on('click', '.address-result', function(e) {
            e.preventDefault(); // Empêcher le comportement par défaut
            console.log('Address result clicked');
            var index = $(this).index() + 1;
            console.log('Selected index: ' + index);
            Shiny.setInputValue('selectedAddress', index, {priority: 'event'});
          });
          
          // Récepteur de message personnalisé pour mettre à jour les résultats
          Shiny.addCustomMessageHandler('updateSearchResults', function(message) {
            console.log('Updating search results');
            if ($('#searchResults').length) {
              $('#searchResults').html(message);
            } else {
              console.error('searchResults element not found');
            }
          });
        });
      ")
    ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("theme", "Thème:", 
                  choices = c("DRIAS - Indicateurs Saisonniers" = "INDICATEURS_SAISONNIERS_ETE",
                              "DRIAS - Indicateurs Annuels" = "INDICATEURS_ANNUELS_HORIZONS",
                              "🔥 DRIAS FEUX - Indicateurs Annuels" = "FEUX_INDICATEURS_ANNUELS_HORIZONS",
                              "🌱 DRIAS AGRI - Indicateurs Annuels" = "AGRI_INDICATEURS_ANNUELS_HORIZONS")),
      
        checkboxInput("use_departments", "Passer la carte au format départements", value = FALSE),
      
      selectInput("scenario", "Scénario:", choices = NULL),
      
      selectInput("horizon", "Horizon:", choices = NULL),
      
      selectInput("variable", "Variable:", choices = NULL),
      
      # Bouton pour confirmer les sélections et charger la carte
        actionButton("confirmChoices", "Confirmer et charger la carte ✅", 
                   style = "margin-top: 15px; margin-bottom: 15px; width: 100%; background-color: #4CAF50; color: white; font-weight: bold;"),
      
        # Boutons de téléchargement dans un conteneur div avec style
        tags$div(
          style = "margin-top: 15px; display: flex; flex-direction: column; gap: 10px;",
      # Bouton pour télécharger la carte en PDF
          downloadButton("downloadPDF", "Télécharger la carte (PDF) 📄", 
                         style = "width: 100%;"),
          
          # Bouton pour télécharger les données en Excel
          downloadButton("downloadExcel", "Télécharger les données (Excel) 📊", 
                        style = "width: 100%; background-color: #5cb85c;")
        ),
      
      width = 3
    ),
    
    mainPanel(
        # Barre de recherche d'adresse au-dessus de la carte
        tags$div(
          style = "margin-bottom: 10px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
          tags$div(
            style = "display: flex; flex-direction: column; gap: 5px;",
            tags$h4("Rechercher une adresse", style = "margin-top: 0; margin-bottom: 5px;"),
            tags$div(
              style = "display: flex; gap: 5px;",
              tags$input(id = "addressInput", type = "text", placeholder = "Entrez une adresse...", 
                        style = "flex-grow: 1; padding: 8px; border: 1px solid #ccc; border-radius: 3px;"),
              tags$button(id = "searchBtn", type = "button", "🔍 Rechercher", 
                         style = "padding: 8px 15px; background-color: #4CAF50; color: white; border: none; border-radius: 3px; cursor: pointer;")
            ),
            tags$div(id = "searchResults", style = "margin-top: 8px; max-height: 200px; overflow-y: auto;"),
            # Bouton de diagnostic conditionnel
            conditionalPanel(
              condition = "output.hasSelectedAddress == true",
              div(
                style = "margin-top: 10px; text-align: right;",
                actionButton("goDiagnostic", "📊 Voir le diagnostic climatique", 
                            icon = icon("chart-line"),
                            style = "padding: 6px 12px; background-color: #5bc0de; color: white; border: none; border-radius: 3px; cursor: pointer;")
              )
            )
          )
        ),
        
        # Carte
        leafletOutput("map", height = "700px"),
      width = 9
    )
  )
  ),
  
  # Deuxième onglet - Explications des indicateurs
  tabPanel(
    title = "Explications des indicateurs 🧭",
    fluidRow(
      column(width = 12,
             h2("Guide des indicateurs DRIAS", style = "text-align: center; margin-bottom: 30px;"),
             p("Cette section fournit des explications sur les différents indicateurs disponibles dans l'application DRIAS. 
               Ces indicateurs permettent de comprendre l'évolution du climat et ses impacts potentiels sur différents secteurs.",
               style = "font-size: 16px; margin-bottom: 20px;")
      )
    ),
    
    # Onglets internes pour les différentes catégories d'indicateurs
    tabsetPanel(
      # Onglet Tous les indicateurs (liste complète)
      tabPanel(
        title = "Liste complète des indicateurs",
        fluidRow(
          column(width = 12,
                 h3("Indicateurs de température", style = "color: #d9534f; border-bottom: 1px solid #d9534f; padding-bottom: 5px;"),
                 tags$div(
                   tags$b("NORTAV"), " - Température moyenne de l'air sur une période donnée, exprimée en degrés Celsius.", tags$br(),
                   tags$b("NORSTM0"), " - Somme de température 'en base 0°C' : Accumulation des températures journalières au-dessus de 0°C d'octobre à juillet, utilisée pour suivre le développement des cultures.", tags$br(),
                   tags$b("NORTXAV"), " - Température maximale : Valeur moyenne des températures les plus élevées enregistrées quotidiennement.", tags$br(),
                   tags$b("ATAV"), " - Écart de température moyenne : Différence entre la température moyenne observée et une valeur de référence.", tags$br(),
                   tags$b("ATXAV"), " - Écart de température maximale : Différence entre la température maximale observée et une valeur de référence.", tags$br(),
                   tags$b("NORSD"), " - Nombre de journées d'été : Nombre de jours où la température dépasse un seuil estival (souvent 25°C).", tags$br(),
                   tags$b("NORTX35"), " - Nombre de jours de forte chaleur : Nombre de jours où la température maximale atteint ou dépasse 35°C.", tags$br(),
                   tags$b("NORTXHWD"), " - Nombre de jours de vague de chaleur : Nombre de jours consécutifs où la température reste élevée, caractérisant une canicule.", tags$br(),
                   tags$b("NORTR"), " - Nombre de nuits tropicales : Nombre de nuits où la température ne descend pas en dessous de 20°C.", tags$br(),
                   tags$b("NORSDA"), " - Nombre de jours d'été d'avril à juin : Nombre de jours où la température dépasse un seuil estival sur cette période spécifique.", tags$br(),
                   tags$b("NORTNFD"), " - Nombre de jours de gel : Nombre de jours où la température descend sous 0°C.", tags$br(),
                   tags$b("NORTNCWD"), " - Nombre de jours de vague de froid : Nombre de jours consécutifs avec des températures très basses, caractérisant une période de froid intense.", tags$br(),
                   tags$b("ASDA"), " - Écart du nombre de jours d'été d'avril à juin : Différence entre le nombre de jours d'été sur cette période et une valeur de référence.", tags$br(),
                   tags$b("ASD"), " - Écart du nombre de journées d'été : Différence dans le nombre total de journées d'été par rapport à une période historique.", tags$br(),
                   tags$b("ATX35"), " - Écart du nombre de jours de forte chaleur : Différence entre le nombre de jours de forte chaleur observé et une moyenne historique.", tags$br(),
                   tags$b("ATXHWD"), " - Écart du nombre de jours de vague de chaleur : Variation du nombre de jours de canicule par rapport à une période de référence.", tags$br(),
                   tags$b("ATR"), " - Écart du nombre de nuits tropicales : Différence dans le nombre de nuits où la température reste élevée par rapport à une période donnée.", tags$br(),
                   tags$b("ATNFD"), " - Écart du nombre de jours de gel : Différence dans le nombre de jours de gel comparé à une période historique.", tags$br(),
                   tags$b("ATNCWD"), " - Écart du nombre de jours de vague de froid : Variation du nombre de jours de froid extrême par rapport à une moyenne de référence."
                 ),
                 
                 h3("Indicateurs de précipitations", style = "color: #5bc0de; border-bottom: 1px solid #5bc0de; padding-bottom: 5px; margin-top: 20px;"),
                 tags$div(
                   tags$b("NORRRA"), " - Cumul de précipitations d'avril à octobre : Total des précipitations enregistrées sur cette période.", tags$br(),
                   tags$b("NORRR"), " - Cumul de précipitations : Total des précipitations tombées sur une période donnée.", tags$br(),
                   tags$b("NORPQ90"), " - Précipitations quotidiennes intenses : Quantité de pluie tombée lors des jours où les précipitations sont dans les 10% les plus fortes.", tags$br(),
                   tags$b("NORPQ99"), " - Précipitations quotidiennes extrêmes : Quantité de pluie tombée lors des jours où les précipitations sont dans le 1% le plus extrême.", tags$br(),
                   tags$b("NORPFL90"), " - Pourcentage des précipitations intenses : Part des précipitations tombées lors des jours les plus pluvieux.", tags$br(),
                   tags$b("NORTPSPI"), " - Temps passé en sécheresse météorologique : Durée des périodes où il y a un déficit important de précipitations.", tags$br(),
                   tags$b("ARR"), " - Écart du cumul de précipitations : Différence entre la quantité de précipitations observée et une moyenne historique.", tags$br(),
                   tags$b("APQ90"), " - Écart de précipitations quotidiennes intenses : Différence dans les précipitations des jours les plus pluvieux par rapport à une période de référence.", tags$br(),
                   tags$b("APQ99"), " - Écart de précipitations quotidiennes extrêmes : Différence dans les précipitations des jours les plus pluvieux extrêmes par rapport à une période donnée.", tags$br(),
                   tags$b("APFL90"), " - Écart du pourcentage des précipitations intenses : Variation de la part des précipitations tombées lors des jours les plus pluvieux.", tags$br(),
                   tags$b("NORRR1MM"), " - Nombre de jours de pluie : Nombre de jours où il est tombé au moins 1 mm de pluie.", tags$br(),
                   tags$b("ARR1MM"), " - Écart du nombre de jours de pluie : Différence dans le nombre de jours de pluie par rapport à une période historique."
                 ),
                 
                 h3("Indicateurs de risques d'incendie", style = "color: #f0ad4e; border-bottom: 1px solid #f0ad4e; padding-bottom: 5px; margin-top: 20px;"),
                 tags$div(
                   tags$b("NORIFM40"), " - Sensibilité Feu Météo Élevée : Nombre de jours où l'indice de risque d'incendie (IFM12) dépasse 40, indiquant un risque important de départ de feu.", tags$br(),
                   tags$b("NORIFMxAV"), " - IFMx moyen : Valeur moyenne d'un indicateur météorologique de risque d'incendie.", tags$br(),
                   tags$b("NORIFMx50"), " - Danger Feu Météo Végétation Vivante Élevé : Nombre de jours où l'indice de risque d'incendie dépasse 50, signalant un danger critique.", tags$br(),
                   tags$b("AIFM40"), " - Écart de Sensibilité Feu Météo Élevée : Différence dans le nombre de jours où l'indice IFM12 dépasse 40 par rapport à une période de référence.", tags$br(),
                   tags$b("AIFMxAV"), " - Écart de IFMx moyen : Différence entre l'IFMx moyen observé et une valeur de référence.", tags$br(),
                   tags$b("AIFMx50"), " - Écart de Danger Feu Météo Végétation Vivante Élevé : Différence dans le nombre de jours où l'IFMx dépasse 50 par rapport à une période de référence."
                 ),
                 
                 h3("Indicateurs agricoles et de végétation", style = "color: #5cb85c; border-bottom: 1px solid #5cb85c; padding-bottom: 5px; margin-top: 20px;"),
                 tags$div(
                   tags$b("NORDATEVEG"), " - Date de la reprise de la végétation : Jour de l'année où la prairie commence à repousser après l'hiver.", tags$br(),
                   tags$b("NORDATEPG"), " - Date de la première gelée : Premier jour après le 1er juillet où la température descend sous 0°C.", tags$br(),
                   tags$b("NORDATEDG"), " - Date de la dernière gelée : Dernier jour après le 1er juillet où la température passe sous 0°C.", tags$br(),
                   tags$b("ADATEVEG"), " - Écart de la date de la reprise de la végétation : Différence entre la date effective de reprise de la végétation et une date moyenne de référence.", tags$br(),
                   tags$b("ADATEDG"), " - Écart de la date de la dernière gelée : Décalage entre la date réelle de la dernière gelée et une date moyenne historique."
                 ),
                 
                 h3("Indicateurs de vent", style = "color: #337ab7; border-bottom: 1px solid #337ab7; padding-bottom: 5px; margin-top: 20px;"),
                 tags$div(
                   tags$b("NORFFQ98"), " - Vent fort : Vitesse du vent correspondant aux 2% des jours les plus venteux.", tags$br(),
                   tags$b("AFFQ98"), " - Écart de vent fort : Différence dans l'intensité des vents forts par rapport à une valeur historique.", tags$br(),
                   tags$b("AFFAV"), " - Écart de la vitesse de vent quotidienne moyenne : Différence dans la vitesse moyenne du vent par rapport à une période donnée.", tags$br(),
                   tags$b("NORFF98"), " - Nombre de jours de vent > Q98 : Nombre de jours où le vent dépasse une valeur correspondant aux 2% des jours les plus venteux.", tags$br(),
                   tags$b("AFF98"), " - Écart du nombre de jours de vent > Q98 : Différence dans le nombre de jours avec des vents très forts par rapport à une moyenne historique.", tags$br(),
                   tags$b("AFF3"), " - Écart du nombre de jours sans vent : Variation dans le nombre de jours avec une absence significative de vent."
                 )
              )
          )
        ),
      
      # Onglet Horizons et Scénarios
      tabPanel(
        title = "Horizons et Scénarios",
        fluidRow(
          column(width = 12,
                 h3("Horizons temporels", style = "color: #5bc0de; border-bottom: 1px solid #5bc0de; padding-bottom: 5px;"),
                 tags$div(
                   tags$b("REF"), " - Période de référence", tags$br(),
                   "Période historique utilisée comme base de comparaison.", tags$br(), tags$br(),
                   tags$b("H1 (2021-2050)"), " - Horizon proche", tags$br(),
                   "Projections climatiques pour le futur proche.", tags$br(), tags$br(),
                   tags$b("H2 (2041-2070)"), " - Horizon moyen", tags$br(),
                   "Projections climatiques pour le milieu du siècle.", tags$br(), tags$br(),
                   tags$b("H3 (2071-2100)"), " - Horizon lointain", tags$br(),
                   "Projections climatiques pour la fin du siècle."
                 ),
                 h3("Scénarios d'émissions", style = "color: #f0ad4e; border-bottom: 1px solid #f0ad4e; padding-bottom: 5px; margin-top: 20px;"),
                 tags$div(
                   tags$b("RCP 2.6"), " - Émissions maîtrisées", tags$br(),
                   "Scénario optimiste impliquant une forte réduction des émissions de gaz à effet de serre et une neutralité carbone 
                   atteinte dans la seconde moitié du siècle. L'augmentation de température moyenne globale serait limitée à environ 2°C 
                   par rapport à l'ère préindustrielle.", tags$br(), tags$br(),
                   tags$b("RCP 4.5"), " - Émissions modérées", tags$br(),
                   "Scénario intermédiaire avec stabilisation des émissions à un niveau moyen, impliquant certaines mesures d'atténuation. 
                   L'augmentation de température moyenne serait d'environ 2,5 à 3°C d'ici 2100.", tags$br(), tags$br(),
                   tags$b("RCP 8.5"), " - Émissions non réduites", tags$br(),
                   "Scénario pessimiste avec des émissions continuant à augmenter tout au long du siècle. L'augmentation de température 
                   pourrait atteindre 4 à 5°C d'ici 2100, entraînant des impacts climatiques majeurs."
                 )
              )
          )
      ),
      
      # Onglet Comment utiliser cette application
      tabPanel(
        title = "Utilisation de l'application",
        fluidRow(
          column(width = 12,
                 h3("Guide d'utilisation", style = "color: #5cb85c; border-bottom: 1px solid #5cb85c; padding-bottom: 5px;"),
                 tags$ol(
                   tags$li(tags$b("Sélectionnez un thème"), " : Choisissez parmi les indicateurs saisonniers, annuels, feux ou agricoles selon votre intérêt."),
                   tags$li(tags$b("Choisissez le format spatial"), " : Communes pour une vision détaillée, départements pour une vue plus globale."),
                   tags$li(tags$b("Sélectionnez un scénario climatique"), " : Du plus optimiste (RCP 2.6) au plus pessimiste (RCP 8.5)."),
                   tags$li(tags$b("Choisissez un horizon temporel"), " : De la période de référence (REF) au futur lointain (H3)."),
                   tags$li(tags$b("Sélectionnez une variable"), " : Choisissez l'indicateur spécifique que vous souhaitez visualiser."),
                   tags$li(tags$b("Confirmez vos choix"), " : Cliquez sur le bouton vert pour charger la carte."),
                   tags$li(tags$b("Explorez la carte"), " : Survolez ou cliquez sur les zones pour voir les valeurs détaillées."),
                   tags$li(tags$b("Exportez si nécessaire"), " : Utilisez le bouton de téléchargement pour obtenir une version PDF.")
                 ),
                 h3("Interprétation des résultats", style = "color: #5bc0de; border-bottom: 1px solid #5bc0de; padding-bottom: 5px; margin-top: 20px;"),
                 tags$div(
                   tags$p("Les couleurs sur la carte indiquent l'intensité de l'indicateur sélectionné :"),
                   tags$ul(
                     tags$li(tags$b("Températures"), " : Du bleu (plus froid) au rouge (plus chaud)"),
                     tags$li(tags$b("Précipitations"), " : Du blanc/jaune clair (plus sec) au bleu foncé (plus humide)"),
                     tags$li(tags$b("Autres indicateurs"), " : L'échelle de couleur est adaptée à chaque variable")
                   ),
                   tags$p("Pour une analyse complète, il est recommandé de comparer :"),
                   tags$ul(
                     tags$li("Différents horizons temporels pour voir l'évolution dans le temps"),
                     tags$li("Différents scénarios pour comprendre la gamme des futurs possibles"),
                     tags$li("Différentes variables pour saisir les multiples aspects du changement climatique")
                   )
                 )
              )
          )
      )
    )
  ),
  
  # Nouvel onglet - Diagnostic climatique
  tabPanel(
    title = "Diagnostic 🩺",
    fluidRow(
      column(width = 12,
             h2("Diagnostic climatique personnalisé", style = "text-align: center; margin-bottom: 20px;"),
             p("Cette page vous permet d'obtenir un diagnostic personnalisé des projections climatiques pour votre commune et de les comparer avec les moyennes nationales.", 
               style = "font-size: 16px; margin-bottom: 20px;")
      )
    ),
    fluidRow(
      column(width = 4,
             wellPanel(
               h3("Adresse sélectionnée"),
               # Affichage de l'adresse sélectionnée
               textOutput("diagSelectedAddress"),
               # Commune correspondante
               textOutput("diagSelectedCommune"),
               # Bouton pour générer le diagnostic et le télécharger en PDF
               downloadButton("downloadDiagnostic", "Télécharger le diagnostic (PDF)", 
                           icon = icon("file-pdf"),
                           style = "margin-top: 15px; width: 100%; background-color: #4CAF50; color: white;"),
               # Message d'instruction s'il n'y a pas d'adresse sélectionnée
               uiOutput("diagInstructions")
             )
      ),
      column(width = 8,
             # Zone d'explication sur le diagnostic
             wellPanel(
               h3("Comment fonctionne le diagnostic climatique ?"),
               p("Le diagnostic climatique vous fournit une analyse personnalisée des projections climatiques pour votre commune, en les comparant aux moyennes nationales."),
               tags$ul(
                 tags$li(strong("Sélectionnez une adresse"), " dans l'onglet Carte interactive."),
                 tags$li(strong("Le système identifie automatiquement la commune"), " en utilisant les coordonnées GPS et en déterminant dans quel polygone communal elles se trouvent."),
                 tags$li(strong("Téléchargez votre diagnostic"), " au format PDF pour une analyse détaillée.")
               ),
               h4("Variables analysées dans le diagnostic"),
               p("Le diagnostic analyse plusieurs indicateurs clés pour comprendre l'évolution du climat dans votre commune :"),
               tags$ul(
                 tags$li(strong("Température moyenne"), " - Évolution des températures moyennes selon différents scénarios"),
                 tags$li(strong("Journées d'été"), " - Nombre de jours où la température dépasse 25°C"),
                 tags$li(strong("Jours de forte chaleur"), " - Nombre de jours où la température atteint ou dépasse 35°C")
               ),
               div(
                 style = "margin-top: 20px; font-style: italic; color: #666;",
                 "Note : Les données utilisées pour ce diagnostic proviennent de DRIAS - les futurs du climat, et sont basées sur les projections climatiques de Météo-France."
               )
             )
      )
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
  
  # Initialiser les scénarios dès le démarrage ou quand le format spatial change
  observe({
    folder_path <- selected_folder_path()
    gpkg_files <- get_gpkg_files(folder_path, input$use_departments)
    
    if (length(gpkg_files) > 0) {
      # Extraire les scénarios
      scenarios <- unique(sapply(gpkg_files, extract_scenario))
      # Créer un vecteur nommé pour les scénarios avec leurs noms complets
      named_scenarios <- scenarios
      names(named_scenarios) <- scenarios
      # Associer les fichiers aux scénarios pour les retrouver plus tard
      scenario_files <- split(gpkg_files, sapply(gpkg_files, extract_scenario))
      # Stocker les associations fichiers-scénarios pour une utilisation ultérieure
      session$userData$scenario_files <- scenario_files
      # Mettre à jour le menu déroulant avec les noms complets
      updateSelectInput(session, "scenario", choices = named_scenarios)
    } else {
      updateSelectInput(session, "scenario", choices = character(0))
      showNotification(
        paste("Aucun fichier", if(input$use_departments) "départemental" else "communal", "trouvé dans le dossier sélectionné."),
        type = "warning",
        duration = 5
      )
    }
  }, priority = 1)
  
  # Observer pour la modification du format spatial (département ou commune)
  observeEvent(input$use_departments, {
    # Réinitialiser complètement les sélections et forcer le rechargement
    folder_path <- selected_folder_path()
    gpkg_files <- get_gpkg_files(folder_path, input$use_departments)
    
    # Notification pour informer l'utilisateur du changement de format spatial
    showNotification(
      paste0("Format spatial modifié : ", 
             if(input$use_departments) "Départements" else "Communes", 
             ". Réinitialisation des sélections en cours..."),
      type = "message",
      duration = 5
    )
    
    # Réinitialiser les données sélectionnées
    selected_data(NULL)
    current_map(NULL)
    
    # Effacer la carte actuelle
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>%
      addControl(
        html = tags$div(
          style = "padding: 6px 8px; background: white; border-radius: 5px; box-shadow: 0 0 15px rgba(0,0,0,0.2);",
          tags$h3("Veuillez sélectionner un scénario, un horizon et une variable", 
                  style = "margin: 0; text-align: center; font-weight: bold;")
        ),
        position = "topright"
      )
    
    if (length(gpkg_files) > 0) {
      # Extraire les scénarios
      scenarios <- unique(sapply(gpkg_files, extract_scenario))
      # Créer un vecteur nommé pour les scénarios avec leurs noms complets
      named_scenarios <- scenarios
      names(named_scenarios) <- scenarios
      # Associer les fichiers aux scénarios pour les retrouver plus tard
      scenario_files <- split(gpkg_files, sapply(gpkg_files, extract_scenario))
      # Stocker les associations fichiers-scénarios pour une utilisation ultérieure
      session$userData$scenario_files <- scenario_files
      
      # Réinitialiser toutes les sélections pour partir sur une base propre
      updateSelectInput(session, "scenario", choices = named_scenarios, selected = character(0))
      updateSelectInput(session, "horizon", choices = character(0), selected = character(0))
      updateSelectInput(session, "variable", choices = character(0), selected = character(0))
    } else {
      # Si aucun fichier trouvé avec le format spécifié, afficher un message
      updateSelectInput(session, "scenario", choices = character(0))
      updateSelectInput(session, "horizon", choices = character(0))
      updateSelectInput(session, "variable", choices = character(0))
      
      showNotification(
        paste("Aucun fichier", if(input$use_departments) "départemental" else "communal", "trouvé dans le dossier sélectionné."),
        type = "warning",
        duration = 5
      )
    }
  }, priority = 0)
  
  # Observer pour le changement de thème - même logique de réinitialisation
  observeEvent(input$theme, {
    # Réinitialiser complètement les sélections et forcer le rechargement
    folder_path <- selected_folder_path()
    gpkg_files <- get_gpkg_files(folder_path, input$use_departments)
    
    # Notification pour informer l'utilisateur du changement de thème
    showNotification(
      paste0("Thème modifié : ", input$theme, ". Réinitialisation des sélections en cours..."),
      type = "message",
      duration = 5
    )
    
    # Réinitialiser les données sélectionnées
    selected_data(NULL)
    current_map(NULL)
    
    # Effacer la carte actuelle
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>%
      addControl(
        html = tags$div(
          style = "padding: 6px 8px; background: white; border-radius: 5px; box-shadow: 0 0 15px rgba(0,0,0,0.2);",
          tags$h3("Veuillez sélectionner un scénario, un horizon et une variable", 
                  style = "margin: 0; text-align: center; font-weight: bold;")
        ),
        position = "topright"
      )
    
    if (length(gpkg_files) > 0) {
      # Extraire les scénarios
      scenarios <- unique(sapply(gpkg_files, extract_scenario))
      # Créer un vecteur nommé pour les scénarios avec leurs noms complets
      named_scenarios <- scenarios
      names(named_scenarios) <- scenarios
      # Associer les fichiers aux scénarios pour les retrouver plus tard
      scenario_files <- split(gpkg_files, sapply(gpkg_files, extract_scenario))
      # Stocker les associations fichiers-scénarios pour une utilisation ultérieure
      session$userData$scenario_files <- scenario_files
      
      # Réinitialiser toutes les sélections pour partir sur une base propre
      updateSelectInput(session, "scenario", choices = named_scenarios, selected = character(0))
      updateSelectInput(session, "horizon", choices = character(0), selected = character(0))
      updateSelectInput(session, "variable", choices = character(0), selected = character(0))
    } else {
      # Si aucun fichier trouvé avec le format spécifié, afficher un message
      updateSelectInput(session, "scenario", choices = character(0))
      updateSelectInput(session, "horizon", choices = character(0))
      updateSelectInput(session, "variable", choices = character(0))
      
      showNotification(
        paste("Aucun fichier", if(input$use_departments) "départemental" else "communal", "trouvé dans le dossier sélectionné."),
        type = "warning",
        duration = 5
      )
    }
  }, priority = 0)
  
  # Charger les données en fonction du thème et du scénario
  raw_data <- reactive({
    req(input$scenario)
    # Récupérer le scénario sélectionné
    selected_scenario <- input$scenario
    
    # Récupérer les fichiers correspondant au scénario
    scenario_files <- session$userData$scenario_files[[selected_scenario]]
    
    if (length(scenario_files) == 0) {
      return(NULL)
    }
    
    # Notification pour indiquer le début du chargement
    showNotification(
      "Chargement des données en cours...", 
      type = "message", 
      duration = NULL,
      id = "loading_notification"
    )
    
    # Charger les données du premier fichier correspondant en utilisant le cache
    data <- get_cached_data(scenario_files[1])
    
    # Fermer la notification de chargement
    removeNotification("loading_notification")
      
      return(data)
  })
  
  # Données sélectionnées qui ne seront actualisées que lors de la confirmation
  selected_data <- reactiveVal(NULL)
  
  # Mettre à jour les horizons dès que les données sont disponibles
  observe({
    data <- raw_data()
    if (!is.null(data)) {
      horizons <- extract_horizons(data)
      
      # Créer un vecteur pour les horizons avec leurs noms complets
      named_horizons <- sapply(horizons, function(h) horizon_full_names[[h]])
      
      # Important: définir les noms explicitement pour que la sélection fonctionne
      names(named_horizons) <- named_horizons
      
      updateSelectInput(session, "horizon", choices = named_horizons)
    } else {
      updateSelectInput(session, "horizon", choices = character(0))
    }
  }, priority = 2)
  
  # Mettre à jour les variables disponibles dès que l'horizon est sélectionné
  observe({
    data <- raw_data()
    horizon_input <- input$horizon
    
    # Extraire le code de l'horizon à partir du nom complet
    if (!is.null(horizon_input) && nchar(horizon_input) > 0) {
      # Extraire le code (REF, H1, H2, H3) du nom complet
      horizon_code <- substr(horizon_input, 1, if(startsWith(horizon_input, "REF")) 3 else 2)
    } else {
      return()
    }
    
    if (!is.null(data)) {
      variables <- get_variables_for_horizon(data, horizon_code, var_descriptions())
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
    
    # Extraire le code de l'horizon à partir du nom complet
    horizon_input <- input$horizon
    if (!is.null(horizon_input) && nchar(horizon_input) > 0) {
      # Extraire le code (REF, H1, H2, H3) du nom complet
      horizon_code <- substr(horizon_input, 1, if(startsWith(horizon_input, "REF")) 3 else 2)
    } else {
      horizon_code <- NULL
    }
    
    # Extraire le code de la variable à partir du nom complet
    variable_input <- input$variable
    if (!is.null(variable_input) && nchar(variable_input) > 0) {
      # Si la variable est au format "CODE - Description", extraire le code
      variable_code <- strsplit(variable_input, " - ")[[1]][1]
    } else {
      variable_code <- variable_input
    }
    
    # Mettre à jour la carte avec les paramètres choisis
    data <- selected_data()
    req(horizon_code, variable_code)
    
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
    col_name <- paste0(variable_code, "_", horizon_code)
    
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
    if (grepl("^(NORT|AT).*AV$", variable_code)) {
      # Palette pour les températures
      pal <- colorNumeric(palette = "RdYlBu", domain = values, reverse = TRUE, na.color = "transparent")
    } else if (grepl("^(NORP|AP)", variable_code)) {
      # Palette pour les précipitations
      pal <- colorNumeric(palette = "Blues", domain = values, na.color = "transparent")
    } else {
      # Palette par défaut pour les autres variables
      pal <- colorNumeric(palette = "Spectral", domain = values, reverse = FALSE, na.color = "transparent")
    }
    
    # Obtenir la description de la variable
    descriptions <- var_descriptions()
    var_desc <- descriptions[[variable_code]]
    if (is.null(var_desc) || var_desc == "") {
      var_desc <- "Description non disponible"
    }
    
    # Créer le titre avec l'horizon et sa période
    horizon_period <- horizon_periods[[horizon_code]]
    horizon_name <- horizon_full_names[[horizon_code]]
    
    title <- paste0(
      variable_code, " - ", var_desc, "<br>",
      "<span style='font-size: 0.9em;'>", input$scenario, " - ", horizon_name, "</span>"
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
        # Revenir à une approche simple pour les popups
        popup = if(input$use_departments) {
          if("NOM" %in% colnames(data) && "INSEE_DEP" %in% colnames(data)) {
            paste0(
              "<strong>Département:</strong> ", data$NOM, "<br>",
              "<strong>Code:</strong> ", data$INSEE_DEP, "<br>",
              "<strong>Valeur:</strong> ", ifelse(is.na(data[[col_name]]), "Non disponible", round(data[[col_name]], 2)),
          "<br><strong>Variable:</strong> ", variable_code, " - ", var_desc
            )
          } else {
            paste0(
              "<strong>Valeur:</strong> ", ifelse(is.na(data[[col_name]]), "Non disponible", round(data[[col_name]], 2)),
              "<br><strong>Variable:</strong> ", variable_code, " - ", var_desc
            )
          }
        } else {
          if("LIB" %in% colnames(data) && "CODE_C" %in% colnames(data)) {
            paste0(
              "<strong>Commune:</strong> ", data$LIB, "<br>",
              "<strong>Code commune:</strong> ", data$CODE_C, "<br>",
              "<strong>Valeur:</strong> ", ifelse(is.na(data[[col_name]]), "Non disponible", round(data[[col_name]], 2)),
              "<br><strong>Variable:</strong> ", variable_code, " - ", var_desc
            )
          } else {
            paste0(
              "<strong>Valeur:</strong> ", ifelse(is.na(data[[col_name]]), "Non disponible", round(data[[col_name]], 2)),
              "<br><strong>Variable:</strong> ", variable_code, " - ", var_desc
            )
          }
        },
        # Simplifier également les labels
        label = if(input$use_departments && "NOM" %in% colnames(data)) {
          paste0(data$NOM, " - Valeur: ", ifelse(is.na(data[[col_name]]), "Non disponible", round(data[[col_name]], 2)))
        } else if(!input$use_departments && "LIB" %in% colnames(data)) {
          paste0(data$LIB, " - Valeur: ", ifelse(is.na(data[[col_name]]), "Non disponible", round(data[[col_name]], 2)))
        } else {
          paste0("Valeur: ", ifelse(is.na(data[[col_name]]), "Non disponible", round(data[[col_name]], 2)))
        }
      ) %>%
      addLegend(
        position = "bottomleft",
        pal = pal,
        values = values_for_legend,
        # Modification: Ne pas afficher de titre dans la légende
        title = NULL,
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
      values = values_for_legend,
      variable_code = variable_code,
      var_desc = var_desc,
      use_departments = input$use_departments  # Ajouter le format spatial
    )
    current_map(map_data)
  })
  
  # Téléchargement de la carte en PDF
  output$downloadPDF <- downloadHandler(
    filename = function() {
      # Extraire les codes des sélections pour le nom de fichier
      horizon_input <- input$horizon
      if (!is.null(horizon_input) && nchar(horizon_input) > 0) {
        horizon_code <- substr(horizon_input, 1, if(startsWith(horizon_input, "REF")) 3 else 2)
      } else {
        horizon_code <- "unknown"
      }
      
      variable_input <- input$variable
      if (!is.null(variable_input) && grepl(" - ", variable_input)) {
        variable_code <- strsplit(variable_input, " - ")[[1]][1]
      } else {
        variable_code <- variable_input
      }
      
      # Ajouter l'information sur le format spatial
      spatial_format <- if(input$use_departments) "DEPARTEMENTS" else "COMMUNES"
      
      # Simplifier le nom du scénario pour éviter les problèmes de caractères spéciaux
      scenario_simplified <- gsub("[^a-zA-Z0-9]", "_", input$scenario)
      
      paste0("carte_", input$theme, "_", scenario_simplified, "_", horizon_code, "_", variable_code, "_", spatial_format, ".pdf")
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
      
      # Approche simple: générer un PDF directement avec les données de la carte
      tryCatch({
        # Créer un PDF simple
        pdf(file, width = 11, height = 8.5)
        
        # Configurer la mise en page
        par(mar = c(2, 2, 4, 2))
        
        # Titre du PDF
        title_text <- paste0(
          map_data$variable_code, " - ", map_data$var_desc, "\n",
          input$scenario, " - ", input$horizon
        )
        
        # Extraire les valeurs pour la légende
        values <- map_data$data[[map_data$col_name]]
        values <- values[!is.na(values)]
        
        # Obtenir les couleurs pour chaque polygone
        if(length(values) > 0) {
          colors <- map_data$pal(sort(values))
          
          # Créer une carte simplifiée
          plot(st_geometry(map_data$data), col = map_data$pal(map_data$data[[map_data$col_name]]), 
               border = "#444444", lwd = 0.5, main = title_text)
          
          # Ajouter une légende simplifiée
          min_val <- min(values, na.rm = TRUE)
          max_val <- max(values, na.rm = TRUE)
          legend_breaks <- seq(min_val, max_val, length.out = 5)
          legend_colors <- map_data$pal(legend_breaks)
          legend_labels <- round(legend_breaks, 2)
          
          legend("bottomleft", legend = legend_labels, fill = legend_colors, 
                 title = map_data$variable_code, cex = 0.8, bty = "n")
          
          # Ajouter des informations supplémentaires
          mtext(paste0("Format: ", if(map_data$use_departments) "Départements" else "Communes"), 
                side = 1, line = 0, adj = 0.02, cex = 0.8)
          
          # Ajouter la date de génération
          mtext(paste0("Généré le: ", format(Sys.time(), "%d/%m/%Y %H:%M")), 
                side = 1, line = 0, adj = 0.98, cex = 0.8)
        } else {
          # Si pas de données, afficher un message
          plot.new()
          text(0.5, 0.5, "Données insuffisantes pour générer la carte", cex = 1.5)
        }
        
        dev.off()
      }, error = function(e) {
        # En cas d'erreur, créer un PDF basique avec un message d'erreur détaillé
        message("Erreur lors de l'export PDF: ", e$message)
        pdf(file, width = 11, height = 8.5)
        plot.new()
        text(0.5, 0.5, paste0("Erreur: ", e$message), cex = 1.2)
        text(0.5, 0.45, "Veuillez réessayer ou contacter l'administrateur", cex = 1)
        dev.off()
      })
    }
  )

  # Téléchargement des données au format Excel
  output$downloadExcel <- downloadHandler(
    filename = function() {
      # Obtenir le fichier gpkg actuellement sélectionné
      req(input$scenario)
      
      selected_scenario <- input$scenario
      scenario_files <- session$userData$scenario_files[[selected_scenario]]
      
      if(length(scenario_files) == 0) {
        return("donnees.xlsx")
      }
      
      # Obtenir le nom du fichier gpkg et le convertir en xlsx
      gpkg_file <- basename(scenario_files[1])
      excel_file <- gsub("\\.gpkg$", ".xlsx", gpkg_file)
      
      return(excel_file)
    },
    content = function(file) {
      # Obtenir le chemin du fichier gpkg actuellement sélectionné
      req(input$scenario)
      
      selected_scenario <- input$scenario
      scenario_files <- session$userData$scenario_files[[selected_scenario]]
      
      if(length(scenario_files) == 0) {
        # Créer un fichier Excel vide avec un message d'erreur
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Erreur")
        openxlsx::writeData(wb, "Erreur", "Aucune donnée disponible", startRow = 1, startCol = 1)
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
        return()
      }
      
      # Obtenir le chemin du fichier Excel correspondant
      gpkg_file <- scenario_files[1]
      excel_file <- gsub("\\.gpkg$", ".xlsx", gpkg_file)
      
      # Vérifier si le fichier Excel existe
      if(file.exists(excel_file)) {
        # Copier le fichier Excel existant vers la destination
        file.copy(excel_file, file)
      } else {
        # Si le fichier Excel n'existe pas, créer un fichier Excel vide avec un message d'erreur
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Erreur")
        openxlsx::writeData(wb, "Erreur", "Le fichier Excel correspondant n'existe pas", startRow = 1, startCol = 1)
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    }
  )

  # Fonction pour rechercher une adresse avec l'API BAN (Base Adresse Nationale)
  search_address <- function(query) {
    if (nchar(query) < 3) {
      return(list())
    }
    
    print(paste("Recherche BAN pour:", query))
    
    # URL de l'API BAN
    url <- "https://api-adresse.data.gouv.fr/search/"
    
    # Effectuer la requête
    tryCatch({
      response <- httr::GET(url, query = list(q = query, limit = 5))
      
      if (httr::status_code(response) == 200) {
        content <- httr::content(response, "text", encoding = "UTF-8")
        data <- jsonlite::fromJSON(content)
        
        if ("features" %in% names(data) && length(data$features) > 0) {
          addresses <- list()
          
          for (i in seq_along(data$features)) {
            feature <- data$features[[i]]
            
            # Vérifier que tous les éléments nécessaires existent
            if (!is.null(feature) && 
                "geometry" %in% names(feature) && 
                "coordinates" %in% names(feature$geometry) && 
                length(feature$geometry$coordinates) >= 2 &&
                "properties" %in% names(feature)) {
              
              prop <- feature$properties
              
              # Créer l'entrée avec uniquement les informations essentielles
              addresses[[length(addresses) + 1]] <- list(
                label = if ("label" %in% names(prop)) prop$label else "Adresse sans nom",
                score = if ("score" %in% names(prop)) as.numeric(prop$score) else 0,
                type = if ("type" %in% names(prop)) prop$type else "inconnu",
                longitude = as.numeric(feature$geometry$coordinates[[1]]),
                latitude = as.numeric(feature$geometry$coordinates[[2]]),
                city = if ("city" %in% names(prop)) prop$city else NA,
                postcode = if ("postcode" %in% names(prop)) prop$postcode else NA,
                citycode = if ("citycode" %in% names(prop)) prop$citycode else NA
              )
            }
          }
          
          return(addresses)
        }
      }
    }, error = function(e) {
      print(paste("Erreur API BAN:", e$message))
    })
    
    return(list())
  }
  
  # Fonction pour rechercher une adresse avec Nominatim (alternative à BAN)
  search_address_nominatim <- function(query) {
    if (nchar(query) < 3) {
      return(list())
    }
    
    print(paste("Recherche Nominatim pour:", query))
    
    # Ajouter "France" à la requête
    if (!grepl("france", tolower(query))) {
      query <- paste(query, "France")
    }
    
    # URL de l'API Nominatim
    url <- "https://nominatim.openstreetmap.org/search"
    
    # Effectuer la requête
    tryCatch({
      response <- httr::GET(
        url, 
        query = list(
          q = query,
          format = "json",
          addressdetails = 1,
          limit = 5,
          countrycodes = "fr"
        ),
        httr::add_headers(`User-Agent` = "DRIAS_App/1.0")
      )
      
      # Respecter les limites de requêtes de Nominatim
      Sys.sleep(1)
      
      if (httr::status_code(response) == 200) {
        content <- httr::content(response, "text", encoding = "UTF-8")
        results <- jsonlite::fromJSON(content)
        
        if (length(results) > 0) {
          addresses <- list()
          
          # Gérer les cas où un seul résultat est retourné (comme un vecteur et non une data frame)
          if (is.data.frame(results)) {
            for (i in 1:nrow(results)) {
              result <- results[i,]
              
              # Ne traiter que les résultats avec des coordonnées
              if ("lat" %in% names(result) && "lon" %in% names(result)) {
                # Extraire la ville et le code postal si disponibles
                city <- NULL
                postcode <- NULL
                
                if ("address" %in% names(result) && is.list(result$address)) {
                  addr <- result$address
                  
                  # Trouver la ville (plusieurs champs possibles)
                  if ("city" %in% names(addr)) {
                    city <- addr$city
                  } else if ("town" %in% names(addr)) {
                    city <- addr$town
                  } else if ("village" %in% names(addr)) {
                    city <- addr$village
                  }
                  
                  # Récupérer le code postal
                  if ("postcode" %in% names(addr)) {
                    postcode <- addr$postcode
                  }
                }
                
                # Créer l'entrée
                addresses[[length(addresses) + 1]] <- list(
                  label = if ("display_name" %in% names(result)) result$display_name else "Adresse sans nom",
                  score = 1 - (i * 0.1),  # Score décroissant
                  type = if ("type" %in% names(result)) result$type else "lieu",
                  longitude = as.numeric(result$lon),
                  latitude = as.numeric(result$lat),
                  city = city,
                  postcode = postcode
                )
              }
            }
          } else if (is.list(results)) {
            # Cas d'un seul résultat
            result <- results
            
            # Ne traiter que les résultats avec des coordonnées
            if ("lat" %in% names(result) && "lon" %in% names(result)) {
              # Extraire la ville et le code postal si disponibles
              city <- NULL
              postcode <- NULL
              
              if ("address" %in% names(result) && is.list(result$address)) {
                addr <- result$address
                
                # Trouver la ville (plusieurs champs possibles)
                if ("city" %in% names(addr)) {
                  city <- addr$city
                } else if ("town" %in% names(addr)) {
                  city <- addr$town
                } else if ("village" %in% names(addr)) {
                  city <- addr$village
                }
                
                # Récupérer le code postal
                if ("postcode" %in% names(addr)) {
                  postcode <- addr$postcode
                }
              }
              
              # Créer l'entrée
              addresses[[length(addresses) + 1]] <- list(
                label = if ("display_name" %in% names(result)) result$display_name else "Adresse sans nom",
                score = 1,  # Score maximum pour un unique résultat
                type = if ("type" %in% names(result)) result$type else "lieu",
                longitude = as.numeric(result$lon),
                latitude = as.numeric(result$lat),
                city = city,
                postcode = postcode
              )
            }
          }
          
          return(addresses)
        }
      }
    }, error = function(e) {
      print(paste("Erreur Nominatim:", e$message))
    })
    
    return(list())
  }
  
  # Réactive value pour stocker l'adresse sélectionnée pour le diagnostic
  selected_address_for_diag <- reactiveVal(NULL)
  selected_commune_code <- reactiveVal(NULL)
  selected_commune_name <- reactiveVal(NULL)
  
  # Stocker les adresses trouvées en tant que valeur réactive au lieu d'utiliser session$userData
  search_results <- reactiveVal(NULL)
  
  # Indicateur pour savoir si une adresse est sélectionnée
  output$hasSelectedAddress <- reactive({ 
    !is.null(selected_address_for_diag()) && !is.null(selected_commune_code()) 
  })
  outputOptions(output, "hasSelectedAddress", suspendWhenHidden = FALSE)
  
  # Fonction pour détecter la commune à partir des coordonnées GPS
  detect_commune_from_coordinates <- function(lon, lat) {
    print(paste("Détection de commune pour les coordonnées:", lon, lat))
    
    # Vérifier que les coordonnées sont dans des limites raisonnables pour la France
    if (is.na(lon) || is.na(lat) || lon < -5.5 || lon > 10 || lat < 41 || lat > 52) {
      print("Coordonnées hors des limites de la France métropolitaine")
      return(NULL)
    }
    
    # Chercher tous les fichiers GPKG de communes
    all_gpkg_files <- c()
    
    # Chercher dans le dossier des indicateurs saisonniers
    saisonniers_files <- list.files(path_indicateurs_saisonniers, 
                                   pattern = ".*COMMUNES.*\\.gpkg$", 
                                   recursive = TRUE, 
                                   full.names = TRUE)
    all_gpkg_files <- c(all_gpkg_files, saisonniers_files)
    
    # Chercher dans le dossier des indicateurs annuels
    annuels_files <- list.files(path_indicateurs_annuels, 
                              pattern = ".*COMMUNES.*\\.gpkg$", 
                              recursive = TRUE, 
                              full.names = TRUE)
    all_gpkg_files <- c(all_gpkg_files, annuels_files)
    
    # Chercher dans le dossier des feux
    feux_files <- list.files(path_feux_indicateurs, 
                           pattern = ".*COMMUNES.*\\.gpkg$", 
                           recursive = TRUE, 
                           full.names = TRUE)
    all_gpkg_files <- c(all_gpkg_files, feux_files)
    
    # Chercher dans le dossier agricole
    agri_files <- list.files(path_agri_indicateurs, 
                           pattern = ".*COMMUNES.*\\.gpkg$", 
                           recursive = TRUE, 
                           full.names = TRUE)
    all_gpkg_files <- c(all_gpkg_files, agri_files)
    
    # Supprimer les doublons
    all_gpkg_files <- unique(all_gpkg_files)
    
    print(paste("Nombre total de fichiers GPKG trouvés:", length(all_gpkg_files)))
    
    if (length(all_gpkg_files) == 0) {
      print("Aucun fichier GPKG de communes trouvé!")
      return(NULL)
    }
    
    # Utiliser le premier fichier trouvé
    gpkg_file <- all_gpkg_files[1]
    print(paste("Utilisation du fichier:", gpkg_file))
    
    # Nom du fichier de cache
    gpkg_basename <- basename(gpkg_file)
    cache_filename <- paste0("communes_", gsub("[^a-zA-Z0-9]", "_", gpkg_basename), ".rds")
    cache_filepath <- file.path(path_cache, cache_filename)
    
    # Essayer de charger depuis le cache
    commune_sf <- NULL
    if (file.exists(cache_filepath)) {
      print("Chargement des communes depuis le cache...")
      tryCatch({
        commune_sf <- readRDS(cache_filepath)
        print(paste("Chargé", nrow(commune_sf), "communes depuis le cache"))
      }, error = function(e) {
        print(paste("Erreur lors du chargement du cache:", e$message))
        commune_sf <- NULL
      })
    }
    
    # Si pas de cache, charger depuis le fichier GPKG
    if (is.null(commune_sf)) {
      print("Chargement des communes depuis le fichier GPKG...")
      tryCatch({
        commune_sf <- sf::st_read(gpkg_file, quiet = TRUE)
        print(paste("Chargé", nrow(commune_sf), "communes depuis GPKG"))
        
        # Trouver les colonnes de code et nom commune
        code_column <- NULL
        name_column <- NULL
        
        # Rechercher des colonnes possibles pour le code
        for (col_name in c("CODE_C", "INSEE_COM", "CODE_INSEE", "ID", "CODE")) {
          if (col_name %in% colnames(commune_sf)) {
            code_column <- col_name
            print(paste("Colonne de code commune trouvée:", code_column))
            break
          }
        }
        
        # Si aucune colonne de code trouvée, créer une colonne CODE_C vide
        if (is.null(code_column)) {
          print("Aucune colonne de code commune trouvée, création d'une colonne CODE_C")
          commune_sf$CODE_C <- NA
          code_column <- "CODE_C"
        }
        
        # Rechercher des colonnes possibles pour le nom
        for (col_name in c("LIB", "NOM_COM", "NOM", "COMMUNE", "LIBELLE")) {
          if (col_name %in% colnames(commune_sf)) {
            name_column <- col_name
            print(paste("Colonne de nom commune trouvée:", name_column))
            break
          }
        }
        
        # Si aucune colonne de nom trouvée, créer une colonne LIB vide
        if (is.null(name_column)) {
          print("Aucune colonne de nom commune trouvée, création d'une colonne LIB")
          commune_sf$LIB <- NA
          name_column <- "LIB"
        }
        
        # Si la colonne s'appelle différemment de CODE_C ou LIB, créer des alias
        if (code_column != "CODE_C") {
          commune_sf$CODE_C <- commune_sf[[code_column]]
        }
        
        if (name_column != "LIB") {
          commune_sf$LIB <- commune_sf[[name_column]]
        }
        
        # S'assurer que la géométrie est valide
        print("Validation des géométries...")
        commune_sf <- sf::st_make_valid(commune_sf)
        
        # Vérifier et transformer en WGS84 si nécessaire
        print(paste("CRS original:", sf::st_crs(commune_sf)$epsg))
        if (sf::st_crs(commune_sf)$epsg != 4326) {
          print("Transformation en WGS84 (EPSG:4326)...")
          commune_sf <- sf::st_transform(commune_sf, 4326)
        }
        
        # Sauvegarder dans le cache pour utilisation future
        print("Sauvegarde des communes dans le cache...")
        dir.create(path_cache, showWarnings = FALSE, recursive = TRUE)
        saveRDS(commune_sf, cache_filepath)
        print("Communes sauvegardées dans le cache")
        
      }, error = function(e) {
        print(paste("Erreur lors du chargement du fichier GPKG:", e$message))
        return(NULL)
      })
    }
    
    if (is.null(commune_sf) || nrow(commune_sf) == 0) {
      print("Aucune donnée de commune disponible")
      return(NULL)
    }
    
    # Créer un point à partir des coordonnées (en WGS84)
    point <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)
    
    # Trouver la commune qui contient le point
    print("Recherche de la commune contenant le point...")
    commune_found <- NULL
    
    tryCatch({
      # Utiliser st_intersects pour trouver quelle commune contient le point
      intersects <- sf::st_intersects(point, commune_sf)
      
      if (length(intersects[[1]]) > 0) {
        # Récupérer la première commune qui contient le point
        commune_idx <- intersects[[1]][1]
        commune_found <- commune_sf[commune_idx, ]
        
        # Extraire les informations de la commune
        code_commune <- as.character(commune_found$CODE_C)
        commune_name <- as.character(commune_found$LIB)
        
        print(paste("Commune trouvée par intersection spatiale:", commune_name, "Code:", code_commune))
        
        return(list(
          code = code_commune,
          name = commune_name
        ))
      } else {
        print("Aucune commune ne contient ce point. Recherche de la commune la plus proche...")
        
        # Comme alternative, trouver la commune la plus proche
        dists <- sf::st_distance(point, commune_sf)
        nearest_idx <- which.min(dists)
        
        nearest_commune <- commune_sf[nearest_idx, ]
        nearest_code <- as.character(nearest_commune$CODE_C)
        nearest_name <- as.character(nearest_commune$LIB)
        
        # Calculer la distance en mètres
        min_dist <- min(dists)
        print(paste("Commune la plus proche:", nearest_name, "Code:", nearest_code, 
                   "Distance:", round(min_dist), "mètres"))
        
        # Ne retourner la commune la plus proche que si elle est à moins de 5km
        if (min_dist < 5000) {
          return(list(
            code = nearest_code,
            name = nearest_name,
            approx = TRUE,
            distance = round(min_dist)
          ))
        } else {
          print("La commune la plus proche est trop éloignée (>5km)")
          return(NULL)
        }
      }
    }, error = function(e) {
      print(paste("Erreur lors de la recherche spatiale:", e$message))
      return(NULL)
    })
    
    return(NULL)
  }
  
  # Observer pour la sélection d'un résultat de recherche
  observeEvent(input$selectedAddress, {
    # Extraire les coordonnées de l'adresse sélectionnée
    index <- as.numeric(input$selectedAddress)
    addresses <- search_results()
    
    if (!is.null(addresses) && index <= length(addresses)) {
      selected <- addresses[[index]]
      
      # Vérifier que l'adresse sélectionnée est une liste valide
      if (!is.list(selected)) {
        print("Erreur: L'adresse sélectionnée n'est pas une liste valide")
        return()
      }
      
      # Stocker l'adresse sélectionnée pour le diagnostic
      selected_address_for_diag(if ("label" %in% names(selected)) selected$label else "Adresse sans nom")
      
      # Vérifier que longitude et latitude existent
      if (!"longitude" %in% names(selected) || !"latitude" %in% names(selected) ||
          is.null(selected$longitude) || is.null(selected$latitude) ||
          is.na(selected$longitude) || is.na(selected$latitude)) {
        print("Coordonnées manquantes dans les résultats de recherche")
        return()
      }
      
      # Zoomer sur l'adresse sélectionnée
      leafletProxy("map") %>%
        setView(lng = selected$longitude, lat = selected$latitude, zoom = 14) %>%
        # Nettoyer les anciens marqueurs et ajouter un nouveau marqueur
        clearGroup("searchMarkers") %>%
        addMarkers(
          lng = selected$longitude, 
          lat = selected$latitude,
          popup = if ("label" %in% names(selected)) selected$label else "Adresse sélectionnée",
          group = "searchMarkers"
        )
      
      # Détection de commune par analyse spatiale avec les fichiers GPKG
      commune_found <- FALSE
      
      print(paste("Coordonnées valides, détection de la commune...", selected$longitude, selected$latitude))
      
      # Utiliser notre nouvelle fonction pour détecter la commune
      commune_info <- find_commune_by_gps(selected$longitude, selected$latitude)
      
      if (!is.null(commune_info) && is.list(commune_info)) {
        # La commune a été trouvée, on stocke ses informations
        if ("code" %in% names(commune_info) && "name" %in% names(commune_info)) {
          code_commune <- commune_info$code 
          commune_name <- commune_info$name
          
          # Vérification supplémentaire pour s'assurer que les valeurs ne sont pas NULL ou NA
          if (!is.null(code_commune) && !is.na(code_commune) && 
              !is.null(commune_name) && !is.na(commune_name)) {
            
            # Vérifier si la commune a été trouvée par approximation
            if ("approx" %in% names(commune_info) && isTRUE(commune_info$approx) && 
                "distance" %in% names(commune_info)) {
              print(paste("Commune approximative trouvée par proximité:", commune_name, 
                          "Code:", code_commune, "Distance:", commune_info$distance, "m"))
              msg <- paste("Commune détectée (approximative, à", commune_info$distance, "m):", 
                          commune_name, "(", code_commune, ")")
            } else {
              print(paste("Commune trouvée par analyse spatiale:", commune_name, "Code:", code_commune))
              msg <- paste("Commune détectée:", commune_name, "(", code_commune, ")")
            }
            
            selected_commune_code(code_commune)
            selected_commune_name(commune_name)
            commune_found <- TRUE
            
            # Notification pour l'utilisateur
            showNotification(msg, type = "message", duration = 5)
            
            # Indiquer à l'utilisateur qu'il peut générer un diagnostic
            output$diagInstructions <- renderUI({
              div(
                style = "margin-top: 10px; padding: 10px; background-color: #dff0d8; border-radius: 5px;",
                p(icon("info-circle"), " Commune identifiée avec succès. Vous pouvez maintenant télécharger le diagnostic climatique.")
              )
            })
          } else {
            print("Résultat de commune valide mais code ou nom manquant")
          }
        } else {
          print("Structure de commune_info incorrecte: code ou name manquant")
        }
      } else {
        print("Aucune commune n'a été détectée via l'analyse spatiale")
      }
      
      # Si la commune n'est pas trouvée par l'analyse spatiale, proposer l'entrée manuelle
      if (!commune_found) {
        output$diagInstructions <- renderUI({
          div(
            style = "margin-top: 10px; padding: 10px; background-color: #fcf8e3; border-radius: 5px;",
            p(icon("exclamation-triangle"), " Impossible de détecter automatiquement la commune pour cette adresse."),
            p("Vous pouvez entrer manuellement le code INSEE et le nom de la commune :"),
            div(
              style = "display: flex; gap: 10px; margin-top: 10px;",
              textInput("manualCommuneCode", "Code INSEE", width = "150px"),
              textInput("manualCommuneName", "Nom de la commune", width = "250px"),
              actionButton("setManualCommune", "Définir la commune", class = "btn-primary")
            )
          )
        })
      }
    }
  })
  
  # Observer pour la définition manuelle d'une commune
  observeEvent(input$setManualCommune, {
    code_commune <- input$manualCommuneCode
    commune_name <- input$manualCommuneName
    
    # Vérifier que les champs ne sont pas vides
    if (nchar(code_commune) > 0 && nchar(commune_name) > 0) {
      selected_commune_code(code_commune)
      selected_commune_name(commune_name)
      
      showNotification(
        paste("Commune définie manuellement:", commune_name, "(", code_commune, ")"),
        type = "message",
        duration = 5
      )
      
      output$diagInstructions <- renderUI({
        div(
          style = "margin-top: 10px; padding: 10px; background-color: #dff0d8; border-radius: 5px;",
          p(icon("info-circle"), " Commune définie avec succès. Vous pouvez maintenant télécharger le diagnostic climatique.")
        )
      })
    } else {
      showNotification(
        "Veuillez remplir à la fois le code INSEE et le nom de la commune.",
        type = "error",
        duration = 5
      )
    }
  })
  
  # Afficher la commune sélectionnée dans l'onglet diagnostic
  output$diagSelectedCommune <- renderText({
    code <- selected_commune_code()
    name <- selected_commune_name()
    if (is.null(code) || is.null(name)) {
      "Aucune commune sélectionnée."
    } else {
      paste("Commune : ", name, " (Code : ", code, ")")
    }
  })
  
  # Observer pour le bouton de diagnostic - redirection vers l'onglet diagnostic
  observeEvent(input$goDiagnostic, {
    updateNavbarPage(session, "navbarPage", selected = "Diagnostic 🩺")
  })
  
  # Afficher l'adresse sélectionnée dans l'onglet diagnostic
  output$diagSelectedAddress <- renderText({
    addr <- selected_address_for_diag()
    if (is.null(addr)) {
      "Aucune adresse sélectionnée. Utilisez la recherche d'adresse dans l'onglet 'Carte interactive'."
    } else {
      addr
    }
  })
  
  # Handler pour le téléchargement du diagnostic en PDF
  output$downloadDiagnostic <- downloadHandler(
    filename = function() {
      # Nom de fichier personnalisé avec la commune et la date
      commune_name <- selected_commune_name()
      if (is.null(commune_name)) {
        commune_name <- "inconnue"
      } else {
        # Nettoyer le nom pour qu'il soit utilisable dans un nom de fichier
        commune_name <- gsub("[^a-zA-Z0-9]", "_", commune_name)
      }
      
      paste0("Diagnostic_climatique_", commune_name, "_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      # Vérifier si on a une commune sélectionnée
      code_commune <- selected_commune_code()
      commune_name <- selected_commune_name()
      
      if (is.null(code_commune) || is.null(commune_name)) {
        # Si aucune commune n'est sélectionnée, afficher un message d'erreur
        showNotification("Aucune commune sélectionnée pour le diagnostic.", type = "error", duration = 5)
        return()
      }
      
      # Afficher un message de chargement
      withProgress(message = 'Génération du diagnostic en cours...', value = 0.3, {
        # Effacer le cache pour s'assurer que de nouvelles données sont générées
        if (exists("excel_data_cache", envir = .GlobalEnv)) {
          rm("excel_data_cache", envir = .GlobalEnv)
        }
        
        # Tenter de générer le PDF
        success <- tryCatch({
          # Mise à jour de la barre de progression
          incProgress(0.3, detail = "Création des graphiques...")
          
          # Générer le PDF de diagnostic
          generate_diagnostic_pdf(file, code_commune, commune_name)
          
          # Mise à jour de la barre de progression
          incProgress(0.4, detail = "Finalisation...")
          
          TRUE  # Succès
        }, error = function(e) {
          # En cas d'erreur, afficher un message et retourner FALSE
          print(paste("Erreur lors de la génération du PDF:", e$message))
          showNotification(paste("Erreur:", e$message), type = "error", duration = 10)
          FALSE
        })
        
        if (success) {
          showNotification(paste("Diagnostic pour", commune_name, "généré avec succès!"), 
                          type = "message", duration = 5)
        }
      })
    }
  )
  
  # Fonction pour charger et mettre en cache les fichiers Excel
  load_excel_files <- function() {
    # On ne vérifie plus le cache global pour forcer de nouvelles données à chaque appel
    # if (exists("excel_data_cache", envir = .GlobalEnv)) {
    #   print("Utilisation des données Excel en cache global")
    #   return(get("excel_data_cache", envir = .GlobalEnv))
    # }
    
    print("Chargement des fichiers Excel...")
    
    # RECHERCHE DE FICHIERS EXCEL DANS DIFFÉRENTS RÉPERTOIRES
    possible_paths <- c(
      "Data/INDICATEURS_SAISONNIERS_ETE",
      "Data/INDICATEURS_ANNUELS_HORIZONS",
      "Data",
      "."
    )
    
    excel_files <- c(
      "DRIAS_ETE_REFERENCE_clean_FINAL_RESULTS_COMMUNES.xlsx",
      "DRIAS_ETE_2_6_clean_FINAL_RESULTS_COMMUNES.xlsx",
      "DRIAS_ETE_4_5_clean_FINAL_RESULTS_COMMUNES.xlsx",
      "DRIAS_ETE_8_5_clean_FINAL_RESULTS_COMMUNES.xlsx"
    )
    
    # Vérifier si des fichiers existent
    found_files <- list()
    
    for (path in possible_paths) {
      for (file in excel_files) {
        file_path <- file.path(path, file)
        if (file.exists(file_path)) {
          found_files[[file]] <- file_path
          print(paste("Fichier trouvé:", file_path))
        }
      }
    }
    
    # Vérifier si nous avons trouvé les fichiers
    if (length(found_files) < length(excel_files)) {
      print("ATTENTION: Certains fichiers Excel n'ont pas été trouvés!")
      print("Génération de données fictives pour le diagnostic...")
      
      # Générer des données simulées basées sur environ 35000 communes
      generate_mock_data <- function(n_communes = 100) {
        # Création d'un ensemble de CODE_C simulés (codes INSEE)
        # On n'utilise plus de seed fixe pour assurer des données différentes à chaque appel
        # set.seed(123)  # Pour la reproductibilité
        
        # Créer 100 communes aléatoires (au lieu de 35000 pour des raisons de performance)
        code_c <- sprintf("%05d", sample(1:99999, n_communes))
        
        # Créer des noms de communes fictifs
        commune_names <- sapply(1:n_communes, function(i) paste("Commune", i))
        
        # Générer quelques codes postaux
        cp <- sprintf("%05d", sample(10000:99999, n_communes, replace=TRUE))
        
        # Créer un dataframe de base
        base_df <- data.frame(
          CODE_C = code_c,
          LIB = commune_names,
          CP = cp,
          stringsAsFactors = FALSE
        )
        
        # Ajouter des variables climatiques pour différents horizons
        add_climate_vars <- function(base_df, prefix) {
          df <- base_df
          
          # Référence
          df[[paste0(prefix, "_REF")]] <- runif(n_communes, 10, 30)
          
          # Horizons H1, H2, H3
          for (h in c("H1", "H2", "H3")) {
            df[[paste0(prefix, "_", h)]] <- 
              df[[paste0(prefix, "_REF")]] + runif(n_communes, 0.5, 5) * 
              match(h, c("H1", "H2", "H3"))  # Augmentation progressive avec l'horizon
          }
          
          return(df)
        }
        
        # Ajouter toutes les variables climatiques nécessaires
        for (prefix in c("NORTAV", "NORSD", "NORTX35")) {
          base_df <- add_climate_vars(base_df, prefix)
        }
        
        return(base_df)
      }
      
      # Générer les données de référence
      mock_data <- generate_mock_data()
      
      # Ajouter la ligne pour notre code_commune spécifique
      code_commune_specific <- selected_commune_code()
      commune_name_specific <- selected_commune_name()
      
      if (!is.null(code_commune_specific) && !is.null(commune_name_specific)) {
        specific_row <- mock_data[1,]
        specific_row$CODE_C <- code_commune_specific
        specific_row$LIB <- commune_name_specific
        mock_data <- rbind(mock_data, specific_row)
      }
      
      # Créer des données légèrement différentes pour chaque scénario
      modify_for_scenario <- function(base_df, intensity = 1) {
        df <- base_df
        
        # Ajuster les valeurs pour différents scénarios
        for (prefix in c("NORTAV", "NORSD", "NORTX35")) {
          for (h in c("H1", "H2", "H3")) {
            col <- paste0(prefix, "_", h)
            if (col %in% names(df)) {
              df[[col]] <- df[[col]] * (1 + intensity * 0.1 * match(h, c("H1", "H2", "H3")))
            }
          }
        }
        
        return(df)
      }
      
      mock_ref <- mock_data
      mock_s26 <- modify_for_scenario(mock_data, 0.5)  # Intensité plus faible
      mock_s45 <- modify_for_scenario(mock_data, 1)    # Intensité moyenne
      mock_s85 <- modify_for_scenario(mock_data, 1.5)  # Intensité forte
      
      # Stocker les données simulées
      excel_data <- list(
        ref = mock_ref,
        s26 = mock_s26,
        s45 = mock_s45,
        s85 = mock_s85
      )
      
      print("Données simulées générées avec succès!")
      print(paste("Communes simulées:", nrow(mock_ref)))
      print(paste("Variables incluses:", paste(names(mock_ref)[-(1:3)], collapse=", ")))
      
      # Ne plus mettre en cache global
      # assign("excel_data_cache", excel_data, envir = .GlobalEnv)
      
      return(excel_data)
    }
    
    # Si tous les fichiers sont trouvés, les charger normalement
    print("Tous les fichiers Excel ont été trouvés. Chargement...")
    
    tryCatch({
      # Charger les fichiers Excel dans l'ordre
      ref_data <- readxl::read_excel(found_files[["DRIAS_ETE_REFERENCE_clean_FINAL_RESULTS_COMMUNES.xlsx"]])
      s26_data <- readxl::read_excel(found_files[["DRIAS_ETE_2_6_clean_FINAL_RESULTS_COMMUNES.xlsx"]])
      s45_data <- readxl::read_excel(found_files[["DRIAS_ETE_4_5_clean_FINAL_RESULTS_COMMUNES.xlsx"]])
      s85_data <- readxl::read_excel(found_files[["DRIAS_ETE_8_5_clean_FINAL_RESULTS_COMMUNES.xlsx"]])
      
      # Stocker les données
      excel_data <- list(
        ref = ref_data,
        s26 = s26_data,
        s45 = s45_data,
        s85 = s85_data
      )
      
      print("Fichiers Excel chargés avec succès!")
      print(paste("Nombre de communes:", nrow(ref_data)))
      
      # Vérifier la présence de CODE_C
      if ("CODE_C" %in% colnames(ref_data)) {
        print(paste("Nombre de CODE_C uniques:", length(unique(ref_data$CODE_C))))
      } else {
        print("ATTENTION: La colonne CODE_C est absente!")
      }
      
      # Ne plus mettre en cache global
      # assign("excel_data_cache", excel_data, envir = .GlobalEnv)
      
      return(excel_data)
      
    }, error = function(e) {
      print(paste("Erreur lors du chargement des fichiers Excel:", e$message))
      
      # Générer des données fictives en cas d'erreur
      print("Génération de données fictives suite à une erreur...")
      generate_mock_data <- function(n_communes = 100) {
        # Ne plus utiliser la même graine à chaque fois
        # set.seed(123)
        code_c <- sprintf("%05d", sample(1:99999, n_communes))
        commune_names <- sapply(1:n_communes, function(i) paste("Commune", i))
        cp <- sprintf("%05d", sample(10000:99999, n_communes, replace=TRUE))
        
        base_df <- data.frame(
          CODE_C = code_c,
          LIB = commune_names,
          CP = cp,
          stringsAsFactors = FALSE
        )
        
        # Ajouter la commune demandée
        code_commune_specific <- selected_commune_code()
        commune_name_specific <- selected_commune_name()
        
        if (!is.null(code_commune_specific) && !is.null(commune_name_specific)) {
          new_row <- data.frame(
            CODE_C = code_commune_specific,
            LIB = commune_name_specific,
            CP = substr(code_commune_specific, 1, 2) * 1000 + sample(100:999, 1),
            stringsAsFactors = FALSE
          )
          base_df <- rbind(base_df, new_row)
        }
        
        # Ajouter des variables climatiques
        for (prefix in c("NORTAV", "NORSD", "NORTX35")) {
          base_df[[paste0(prefix, "_REF")]] <- runif(nrow(base_df), 10, 30)
          for (h in c("H1", "H2", "H3")) {
            base_df[[paste0(prefix, "_", h)]] <- 
              base_df[[paste0(prefix, "_REF")]] + runif(nrow(base_df), 0.5, 5) * 
              match(h, c("H1", "H2", "H3"))
          }
        }
        
        return(base_df)
      }
      
      mock_data <- generate_mock_data()
      excel_data <- list(
        ref = mock_data,
        s26 = mock_data,
        s45 = mock_data,
        s85 = mock_data
      )
      
      # Mettre en cache global
      assign("excel_data_cache", excel_data, envir = .GlobalEnv)
      
      return(excel_data)
    })
  }
  
  # Observer pour le bouton de recherche d'adresse
  observeEvent(input$searchBtnClicked, {
    query <- input$searchBtnClicked$address
    
    if (is.null(query) || nchar(query) < 3) {
      # Afficher un message si la requête est trop courte
      session$sendCustomMessage(type = "updateSearchResults", 
                               message = "<div style='color: #d9534f;'>Veuillez entrer au moins 3 caractères</div>")
      return()
    }
    
    # Afficher un message de chargement
    session$sendCustomMessage(type = "updateSearchResults", 
                             message = "<div style='color: #5bc0de;'>Recherche en cours...</div>")
    
    # Rechercher l'adresse avec l'API BAN
    print(paste("Recherche de l'adresse:", query))
    addresses <- tryCatch({
      search_address(query)
    }, error = function(e) {
      print(paste("Erreur lors de la recherche BAN:", e$message))
      list()
    })
    
    # Si aucun résultat avec BAN, essayer avec Nominatim
    if (length(addresses) == 0) {
      print("Aucun résultat avec BAN, tentative avec Nominatim")
      addresses <- tryCatch({
        search_address_nominatim(query)
      }, error = function(e) {
        print(paste("Erreur lors de la recherche Nominatim:", e$message))
        list()
      })
    }
    
    print(paste("Nombre de résultats:", length(addresses)))
    
    if (length(addresses) == 0) {
      # Aucun résultat trouvé
      session$sendCustomMessage(type = "updateSearchResults", 
                               message = "<div style='color: #d9534f;'>Aucun résultat trouvé</div>")
    } else {
      # Construire la liste des résultats
      result_html <- "<div style='display: flex; flex-direction: column; gap: 5px;'>"
      
      for (i in seq_along(addresses)) {
        addr <- addresses[[i]]
        # S'assurer que les données sont bien définies
        label <- ifelse(is.null(addr$label), "Adresse sans nom", addr$label)
        type <- ifelse(is.null(addr$type), "inconnu", addr$type)
        score <- ifelse(is.null(addr$score), 0, addr$score)
        
        print(paste("Résultat", i, ":", label, "- Lat:", addr$latitude, "Lng:", addr$longitude))
        
        result_html <- paste0(
          result_html,
          "<div class='address-result' style='padding: 5px; border-radius: 3px; cursor: pointer; background-color: #f5f5f5; border: 1px solid #ddd;' ",
          "data-lat='", addr$latitude, "' data-lng='", addr$longitude, "'>",
          "<div style='font-weight: bold;'>", label, "</div>",
          "<div style='font-size: 0.8em; color: #666;'>Type: ", type, " | Score: ", round(score * 100), "%</div>",
          "</div>"
        )
      }
      
      result_html <- paste0(result_html, "</div>")
      
      # Envoyer les résultats au navigateur
      session$sendCustomMessage(type = "updateSearchResults", message = result_html)
      
      # Stocker les adresses dans la valeur réactive au lieu de userData
      search_results(addresses)
    }
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)