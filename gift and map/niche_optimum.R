source("D:/gitrepo/SBG_eco_taxo/src/preparation data/format_data.R")
#source("./src/gift.R") #############very long don't nedd if data_env available
##################################
####    OPTIMUM RASTER      ######
##################################
library(raster)
library(rinat)
library(rgbif)
library(dplyr)
library(sp)
library(tidyverse)


data_env <- read.csv("D:/Thèse Metabolomique/Thèse Metabolomique/jardin botanique/data_env.csv",sep=",")
data_env <- data_env$species[which(is.na(data_env$mean_wc2.0_bio_30s_01))]


# Initialisation de la barre de progression
progress <- 0
total_iterations <- length(data_env)

# Création d'un dataframe pour stocker les moyennes
mean_df <- data.frame(species = character(), stringsAsFactors = FALSE)

# Boucle pour chaque espèce dans la liste data_env
for (species_name in data_env) {
  # Mise à jour de la barre de progression
  progress <- progress + 1
  cat(sprintf("\rProgression: %d%%", round(progress / total_iterations * 100)))
  

  data_gbif_selected <- data.frame()
  data_gbif <- data.frame()
  especetest <- data.frame()
  data_inat <- data.frame()
  data_gbif_wgs84 <- data.frame()




  # Partie pour Inaturalist
  tryCatch({
    especetest <- rinat::get_inat_obs(query = species_name)
    data_inat <- especetest[c("longitude", "latitude", "quality_grade", "captive_cultivated")]
    filtered_data <- data_inat[data_inat$quality_grade == "research" & data_inat$captive_cultivated == "false", ]
    filtered_data <- filtered_data[, c(2, 1, 3, 4)]
    filtered_data$Source <- "Inaturalist"
  }, error = function(e) {
    cat("Erreur lors de la récupération des données depuis Inaturalist pour l'espèce ", species_name, ". La boucle continue.\n")
    return(NULL)  # Retourne NULL pour sortir du bloc tryCatch sans erreur
  })
  
  # Partie pour GBIF
  tryCatch({
    gbif_data <- rgbif::occ_data(scientificName = species_name, hasCoordinate = TRUE)
    data_gbif <- gbif_data$data

    # création objet SpatialPointsDataFrame à partir des données GBIF
    coordinates(data_gbif) <- c("decimalLongitude", "decimalLatitude")
    # système de projection
    proj4string(data_gbif) <- sp::CRS("+proj=longlat +datum=WGS84")
    # EPSG:4326 (WGS84 Plate Carrée)
    data_gbif_wgs84 <- sp::spTransform(data_gbif, CRS("+init=epsg:4326"))
    # Crée un nouveau DataFrame avec ces coordonnées
    longitude <- sp::coordinates(data_gbif_wgs84)[, 1]
    latitude <- sp::coordinates(data_gbif_wgs84)[, 2]
    data_gbif_selected <- data.frame(longitude = longitude, latitude = latitude)

    ### Data frame avec données de localisation GBIF et inat fusionné
    data_gbif_selected$Source <- "GBIF"
    data_gbif_selected$quality_grade <- NA
    data_gbif_selected$captive_cultivated <- NA
  }, error = function(e) {
    cat("Erreur lors de la récupération des données depuis GBIF pour l'espèce ", species_name, ". La boucle continue.\n")
    return(NULL)  # Retourne NULL pour sortir du bloc tryCatch sans erreur
  })

  # Si les données sont récupérées avec succès, poursuivre ici
  if (exists("filtered_data") && exists("data_gbif_selected")) {
    # Data frame avec données de localisation GBIF et inat fusionné
    if (nrow(filtered_data) > 0 && nrow(data_gbif_selected) > 0) {
      data_gps <- rbind(filtered_data, data_gbif_selected)
    } else if (nrow(filtered_data) > 0) {
      data_gps <- filtered_data
    } else if (nrow(data_gbif_selected) > 0) {
      data_gps <- data_gbif_selected
    } else {
      data_gps <- data.frame(longitude = numeric(), latitude = numeric())  # Créer un dataframe vide
    }
    data_gps <- data_gps[, c("longitude", "latitude")]
 }


    ###########################################
    #####    DONNÉES CLIMATIQUES ET SOL   #####
    ###########################################
    raster_coupe <- "D:/Thèse Metabolomique/Thèse Metabolomique/medicinal_plant/data_clim/raster_cut"
  
    # Liste des fichiers tif dans le répertoire raster_coupe
    tif_files <- list.files(raster_coupe, pattern = "\\.tif$", full.names = TRUE)
  
    # Filtrer uniquement les fichiers "Annual_Mean_Temperature.tif" et "Annual_Precipitation.tif"
    tif_files <- tif_files[grep("Annual_Mean_Temperature\\.tif$|Annual_Precipitation\\.tif$", tif_files)]
  
    # Create a list to store the raster objects
    raster_list <- list()
    # Loop through the .tif files and load them into raster objects
    for (file in tif_files) {
      raster_complet <- raster::raster(file)
      raster_list[[basename(file)]] <- raster_complet
    }
    
    # Recuperation des donnees dans les rasters
    for (raster_name in names(raster_list)) {
      raster_obj <- raster_list[[raster_name]]
      extracted_data <- raster::extract(raster_obj, data_gps[, c("longitude", "latitude")])
      data_gps[[raster_name]] <- extracted_data
    }
    data_loc <- na.omit(data_gps)
    # Calcul de la moyenne pour chaque colonne
    #mean_values <- colMeans(data_loc)
    
    tryCatch({
  # Ajout du nom de l'espèce à la première colonne de mean_values
  mean_values <- c(species_name, data_loc)
  mean_values <- data.frame(mean_values)
  colnames(mean_values)[1] <- "species"
  # Ajout des moyennes à mean_df
  mean_df <- rbind(mean_df, mean_values, use.names = FALSE)
  }, error = function(e) {
  message("Pas de données disponibles")
  })
  
}

write.csv(mean_df, "D:/Thèse Metabolomique/Thèse Metabolomique/jardin botanique/data_env_restant.csv", row.names = FALSE)
