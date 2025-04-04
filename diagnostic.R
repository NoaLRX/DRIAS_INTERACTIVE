library(readxl)
scenarioREF <- read_excel("/Users/noa/Desktop/TESTING/INDICATEURS_SAISONNIERS_ETE/Resultats/DRIAS_ETE_2_6_clean_FINAL_RESULTS_COMMUNES.xlsx")
colnames(scenarioREF)

scenario26<- read_excel("/Users/noa/Desktop/TESTING/INDICATEURS_SAISONNIERS_ETE/Resultats/DRIAS_ETE_2_6_clean_FINAL_RESULTS_COMMUNES.xlsx")
colnames(scenario26)

scenario45<- read_excel("/Users/noa/Desktop/TESTING/INDICATEURS_SAISONNIERS_ETE/Resultats/DRIAS_ETE_4_5_clean_FINAL_RESULTS_COMMUNES.xlsx")
colnames(scenario45)


scenario85<- read_excel("/Users/noa/Desktop/TESTING/INDICATEURS_SAISONNIERS_ETE/Resultats/DRIAS_ETE_8_5_clean_FINAL_RESULTS_COMMUNES.xlsx")
colnames(scenario85)

library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(cowplot)  # Pour une meilleure mise en page des graphiques multiples

# Fonction pour créer un graphique pour une variable spécifique
create_scenario_plot <- function(ref_data, s26_data, s45_data, s85_data, var_prefix, var_name, y_label, subtitle = NULL) {
  # Extraire la valeur de référence (moyenne)
  ref_value <- mean(ref_data[[paste0(var_prefix, "_H1")]], na.rm = TRUE)
  
  # Calculer les moyennes pour chaque scénario et horizon
  s26_h1 <- mean(s26_data[[paste0(var_prefix, "_H1")]], na.rm = TRUE)
  s26_h2 <- mean(s26_data[[paste0(var_prefix, "_H2")]], na.rm = TRUE)
  s26_h3 <- mean(s26_data[[paste0(var_prefix, "_H3")]], na.rm = TRUE)
  
  s45_h1 <- mean(s45_data[[paste0(var_prefix, "_H1")]], na.rm = TRUE)
  s45_h2 <- mean(s45_data[[paste0(var_prefix, "_H2")]], na.rm = TRUE)
  s45_h3 <- mean(s45_data[[paste0(var_prefix, "_H3")]], na.rm = TRUE)
  
  s85_h1 <- mean(s85_data[[paste0(var_prefix, "_H1")]], na.rm = TRUE)
  s85_h2 <- mean(s85_data[[paste0(var_prefix, "_H2")]], na.rm = TRUE)
  s85_h3 <- mean(s85_data[[paste0(var_prefix, "_H3")]], na.rm = TRUE)
  
  # Créer un dataframe pour le graphique
  plot_data <- data.frame(
    scenario = c(
      "REF", "2.6", "4.5", "8.5",  # H1
      "REF", "2.6", "4.5", "8.5",  # H2
      "REF", "2.6", "4.5", "8.5"   # H3
    ),
    horizon = c(
      rep("H1", 4),
      rep("H2", 4),
      rep("H3", 4)
    ),
    value = c(
      ref_value, s26_h1, s45_h1, s85_h1,
      ref_value, s26_h2, s45_h2, s85_h2,
      ref_value, s26_h3, s45_h3, s85_h3
    )
  )
  
  # Convertir scenario en facteur ordonné
  plot_data$scenario <- factor(plot_data$scenario, levels = c("REF", "2.6", "4.5", "8.5"))
  
  # Créer un dataframe filtré pour les connexions entre les points (sans REF)
  plot_data_filtered <- plot_data %>%
    filter(scenario != "REF")
  
  # Créer le graphique
  p <- ggplot(plot_data_filtered, aes(x = scenario, y = value, color = horizon, group = horizon)) +
    # Ajouter les lignes reliant les points
    geom_line(size = 1.5) +
    # Ajouter les points
    geom_point(size = 10, alpha = 0.7) +
    # Ajouter les étiquettes de valeurs
    geom_text(aes(label = round(value, 1)), color = "white", size = 3) +
    # Ajouter la valeur de référence comme ligne horizontale en pointillés
    geom_hline(yintercept = ref_value, linetype = "dashed", color = "gray50") +
    # Ajouter l'étiquette de la valeur de référence
    annotate("text", x = 0.75, y = ref_value, label = paste("Réf:", round(ref_value, 1)), 
             hjust = 0, color = "gray50", fontface = "italic") +
    # Définir les couleurs
    scale_color_manual(values = c("H1" = "#69b3d6", "H2" = "#f89c39", "H3" = "#d93b48")) +
    # Personnaliser les axes et le titre
    labs(
      title = paste("Évolution de", var_name),
      subtitle = subtitle,
      x = "Scénario RCP",
      y = y_label,
      color = "Horizon"
    ) +
    # Personnaliser le thème
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      axis.title = element_text(face = "bold", size = 10),
      legend.title = element_text(face = "bold")
    )
  
  return(p)
}

# Créer les graphiques pour chaque variable - Données locales
p1_local <- create_scenario_plot(scenarioREF, scenario26, scenario45, scenario85, 
                                 "NORTAV", "NORTAV", "Valeur moyenne", "Données locales")

p2_local <- create_scenario_plot(scenarioREF, scenario26, scenario45, scenario85, 
                                 "NORTXAV", "NORTX35", "Valeur moyenne", "Données locales")

p3_local <- create_scenario_plot(scenarioREF, scenario26, scenario45, scenario85, 
                                 "ATAV", "ARR", "Valeur moyenne", "Données locales")

# Simuler des données pour la moyenne française (vous devrez les remplacer par vos vraies données)
# Définissons des données fictives pour la France - À REMPLACER PAR VOS DONNÉES RÉELLES
scenarioREF_fr <- scenarioREF  # Copie pour simulation
scenario26_fr <- scenario26    # Copie pour simulation
scenario45_fr <- scenario45    # Copie pour simulation
scenario85_fr <- scenario85    # Copie pour simulation

# Modifier artificiellement ces données pour simuler la moyenne française (À REMPLACER)
# Par exemple, ajoutons un petit décalage aux valeurs
for(col in grep("NORTAV|NORTXAV|ATAV", names(scenarioREF_fr), value = TRUE)) {
  scenarioREF_fr[[col]] <- scenarioREF_fr[[col]] * 0.95  # 5% de moins
  scenario26_fr[[col]] <- scenario26_fr[[col]] * 0.95
  scenario45_fr[[col]] <- scenario45_fr[[col]] * 0.95
  scenario85_fr[[col]] <- scenario85_fr[[col]] * 0.95
}

# Créer les graphiques pour la moyenne française
p1_fr <- create_scenario_plot(scenarioREF_fr, scenario26_fr, scenario45_fr, scenario85_fr, 
                              "NORTAV", "NORTAV", "Valeur moyenne", "Moyenne française")

p2_fr <- create_scenario_plot(scenarioREF_fr, scenario26_fr, scenario45_fr, scenario85_fr, 
                              "NORTXAV", "NORTX35", "Valeur moyenne", "Moyenne française")

p3_fr <- create_scenario_plot(scenarioREF_fr, scenario26_fr, scenario45_fr, scenario85_fr, 
                              "ATAV", "ARR", "Valeur moyenne", "Moyenne française")

# Extraire les légendes une seule fois pour éviter les répétitions
legend <- get_legend(p1_local + theme(legend.position = "bottom", legend.box = "horizontal"))

# Supprimer les légendes de tous les graphiques individuels
p1_local <- p1_local + theme(legend.position = "none")
p2_local <- p2_local + theme(legend.position = "none")
p3_local <- p3_local + theme(legend.position = "none")
p1_fr <- p1_fr + theme(legend.position = "none")
p2_fr <- p2_fr + theme(legend.position = "none")
p3_fr <- p3_fr + theme(legend.position = "none")

# Organiser les graphiques en grille
grid_plot <- plot_grid(
  p1_local, p2_local, p3_local,
  p1_fr, p2_fr, p3_fr,
  ncol = 3, nrow = 2,
  align = "hv",
  labels = NULL
)

# Ajouter la légende commune en bas
final_plot <- plot_grid(
  grid_plot, 
  legend, 
  ncol = 1, 
  rel_heights = c(1, 0.1)
)

# Afficher le graphique final
final_plot

# Sauvegarder le graphique
ggsave("evolution_scenarios_climatiques_grille.png", 
       final_plot, 
       width = 15, height = 10, dpi = 300)