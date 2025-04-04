# Charger les bibliothèques nécessaires
library(sf)
library(dplyr)
library(tools)
library(openxlsx)  # Pour la conversion vers Excel

# Fonction pour convertir un fichier GPKG en Excel
convert_gpkg_to_excel <- function(input_folder) {
  # Vérifier si le dossier existe
  if (!dir.exists(input_folder)) {
    stop(paste("Le dossier", input_folder, "n'existe pas."))
  }
  
  # Obtenir la liste des fichiers GPKG dans le dossier
  gpkg_files <- list.files(input_folder, pattern = "\\.gpkg$", full.names = TRUE)
  
  if (length(gpkg_files) == 0) {
    message("Aucun fichier GPKG trouvé dans le dossier.")
    return(NULL)
  }
  
  message(paste("Trouvé", length(gpkg_files), "fichiers GPKG à convertir."))
  
  # Convertir chaque fichier GPKG en Excel
  for (gpkg_file in gpkg_files) {
    tryCatch({
      # Construire le chemin du fichier de sortie Excel
      output_excel <- paste0(file_path_sans_ext(gpkg_file), ".xlsx")
      
      message(paste("Conversion de", basename(gpkg_file), "en", basename(output_excel), "..."))
      
      # Lire le fichier GPKG avec sf et supprimer immédiatement la géométrie
      gdf <- st_read(gpkg_file, quiet = TRUE) %>% 
        st_drop_geometry()
      
      # Enregistrer en Excel
      write.xlsx(gdf, file = output_excel, rowNames = FALSE)
      
      message(paste("Conversion réussie:", basename(output_excel)))
    }, error = function(e) {
      message(paste("Erreur lors de la conversion de", basename(gpkg_file), ":", e$message))
    })
  }
  
  message("Traitement terminé.")
}

# Chemin vers le dossier contenant les fichiers GPKG
input_folder <- "/Users/noa/Desktop/TESTING/FEUX_INDICATEURS_ANNUELS_HORIZONS/Resultats"

# Exécuter la conversion
convert_gpkg_to_excel(input_folder)