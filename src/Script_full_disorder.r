############################
library(myTAI)
library(dplyr)
library("GIFT")
library(igraph)
library(readxl)

###loading species list 

neu_acq <- read_excel("D:/Thèse Metabolomique/Thèse Metabolomique/jardin botanique/acquisitionExport_neu.xlsx")
neu_cult <- read_excel("D:/Thèse Metabolomique/Thèse Metabolomique/jardin botanique/cultivatedExport_neu.xlsx")


###mix dataframe
# Conversion en minuscules et remplacement des caractères spécifiques
colnames(neu_cult) <- gsub(" ", "_", colnames(neu_cult))
colnames(neu_cult) <- tolower(colnames(neu_cult))
colnames(neu_cult) <- gsub("[éè]", "e", colnames(neu_cult))
colnames(neu_cult) <- gsub("[^a-z0-9_]", "", iconv(colnames(neu_cult), "latin1", "ASCII", sub=""))
neu_cult<- data.frame(neu_cult)
neu_cult$numero_de_specimen_cultive <- substr(neu_cult$numero_de_specimen_cultive, 1, 8)


colnames(neu_acq) <- gsub(" ", "_", colnames(neu_acq))
colnames(neu_acq) <- tolower(colnames(neu_acq))
colnames(neu_acq) <- gsub("[éè]", "e", colnames(neu_acq))
colnames(neu_acq) <- gsub("[^a-z0-9_]", "", iconv(colnames(neu_acq), "latin1", "ASCII", sub=""))
neu_acq<- data.frame(neu_acq)
neu_acq$numero_de_specimen_acquis <- substr(neu_acq$numero_de_specimen_acquis, 1, 8)

neu_bota <- merge(neu_acq, neu_cult, by.x = "numero_de_specimen_acquis", by.y = "numero_de_specimen_cultive", all = TRUE)
neu_bota <- neu_bota[!is.na(neu_bota$vivant), ]


# Parcourir les noms des colonnes de neu_bota
for (col_name in names(neu_bota)) {
  # Vérifier si le nom de la colonne se termine par ".x"
  if (endsWith(col_name, ".x")) {
    # Trouver le nom de la colonne correspondante dans neu_bota
    col_name_y <- sub("\\.x$", ".y", col_name)
    
    # Vérifier si la colonne correspondante existe dans neu_bota
    if (col_name_y %in% names(neu_bota)) {
      # Fusionner les deux colonnes
      merged_column <- paste0(neu_bota[[col_name]], neu_bota[[col_name_y]])
      
      # Remplacer les valeurs "NA" ou NA dans la colonne fusionnée par celles de la colonne .x
      merged_column[merged_column == "NA" | is.na(merged_column)] <- neu_bota[[col_name]][merged_column == "NA" | is.na(merged_column)]
      
      # Supprimer les doublons dans la colonne fusionnée
      merged_column <- gsub("(\\b\\w+\\b)\\s+\\1", "\\1", merged_column, perl = TRUE)
      
      # Renommer la nouvelle colonne sans le suffixe ".x"
      names(neu_bota)[names(neu_bota) == col_name] <- gsub("\\.x$", "", col_name)
      
      # Appliquer les modifications dans neu_bota
      neu_bota[[col_name]] <- merged_column
      
      # Supprimer les colonnes .x et .y de neu_bota
      neu_bota <- neu_bota[, !(names(neu_bota) %in% c(col_name, col_name_y))]
    }
  }
}

###############################
######FRIBOURG
fri_bota <- read.csv("D:/Thèse Metabolomique/Thèse Metabolomique/jardin botanique/species_list_croisee_final.csv",sep=",")
fix(fri_bota)

fri_tax <- data.frame(
  species = fri_bota$matched_name,
  genus = fri_bota$organism_otol_genus,
  family = fri_bota$taxonFamille1
)

fri_tax <- fri_tax %>%
  filter(species != "" & genus != "" & family != "")



######################################
########       taxonomie        ######
######################################

neu_espece <- unique(data.frame(species = paste(neu_bota$genre, neu_bota$espece, sep = " ")))
length(neu_espece)

resultats_liste <- list()

for (organisme in neu_espece$species) {
  tryCatch({
    resultat <- myTAI::taxonomy(
      organism = "Arabidopsis thaliana",
      db = "ncbi",
      output = "classification"
    )
    resultats_liste[[organisme]] <- resultat
  }, error = function(e) {
    if (grepl("HTTP 400", e$message)) {
      cat("Erreur HTTP 400 détectée. Relance de la boucle pour", organisme, "\n")
    } else {
      stop(e)
    }
  })
}

# Vérifier si toutes les espèces ont été traitées
if (length(resultats_liste) < length(neu_espece$species)) {
  for (i in seq_along(neu_espece$species)[-seq_along(resultats_liste)]) {
    organisme <- neu_espece$species[i]
    tryCatch({
      resultat <- myTAI::taxonomy(
        organism = organisme,
        db = "ncbi",
        output = "classification"
      )
      resultats_liste[[organisme]] <- resultat
    }, error = function(e) {
      if (grepl("HTTP 400", e$message)) {
        cat("Erreur HTTP 400 détectée. Relance de la boucle pour", organisme, "\n")
      } else {
        stop(e)
      }
    })
  }
}

list_taxo_neu <- resultats_liste

resultats_liste_clean <- lapply(resultats_liste, na.omit)

length(resultats_liste)


# Créer un dataframe vide pour stocker les résultats finaux
list_species_taxo <- data.frame(organism = character(),
                       rank = character(),
                       name = character(),
                       stringsAsFactors = FALSE)

# Initialiser un dataframe vide
list_species_taxo <- data.frame(stringsAsFactors = FALSE)

# Parcourir la liste de résultats
for (organisme in names(resultats_liste)) {
  tryCatch({
    # Sélectionnez les lignes avec les rangs "genus" ou "family"
    selected_rows <- resultats_liste[[organisme]] %>% 
      filter(rank %in% c("order", "family", "genus"))
    
    # Ajoutez le nom de l'organisme à la colonne "organism"
    selected_rows <- mutate(selected_rows, organism = organisme)
    
    # Ajoutez les résultats au dataframe final
    list_species_taxo <- bind_rows(list_species_taxo, selected_rows)
  }, error = function(e) {
  })
}


# Réinitialisez les index du dataframe final
row.names(list_species_taxo) <- NULL

# Utilisez la fonction group_by et summarize pour fusionner les lignes par espèce
list_species_taxo <- list_species_taxo %>%
  group_by(organism) %>%
  summarize(order_id = paste(id[rank == "order"], collapse = ", "),
            order_name = paste(name[rank == "order"], collapse = ", "),
            family_id = paste(id[rank == "family"], collapse = ", "),
            family_name = paste(name[rank == "family"], collapse = ", "),
            genus_id = paste(id[rank == "genus"], collapse = ", "),
            genus_name = paste(name[rank == "genus"], collapse = ", "))

list_species_taxo <- unique(list_species_taxo)
fix(neu_bota)

list_species_taxo <- list_species_taxo %>%
  rename(species = organism)

neu_bota <- neu_bota %>%
  mutate(species = paste(genre, espece, sep = " "))

neu_bota <- merge(neu_bota, list_species_taxo, by = "species", all.x = TRUE)

write.csv(neu_bota, "D:/Thèse Metabolomique/Thèse Metabolomique/jardin botanique/neu_bota_taxo.csv", row.names = FALSE)











##############################################
########       taxonomie result        ######
##############################################
classif <- read.csv("D:/Thèse Metabolomique/Thèse Metabolomique/jardin botanique/classification.csv",sep=";")
length(unique(classif$genus))
length(unique(classif$family))

#293 familles botaniques regroupant 7476 genres botaniques et se répartissant sur 80 ordres botaniques. *Chiffres en date de 2021.
######pour neuchatel seul

length(unique(neu_bota$famille))
(177 / 565) * 100
31%

length(unique(neu_bota$genre))
(765 / 32170) * 100
2.3%

######pour fribourg seul
length(unique(fri_tax$family))
(207 / 565) * 100
36%

length(unique(fri_tax$genus))
(1410 / 32170) * 100
4.3%

##### pour les deux reunis 

family_list <- c(neu_bota$famille, fri_tax$family)
length(unique(family_list))
(237 / 565) * 100
41%

genus_list <- c(neu_bota$genre, fri_tax$genus)
length(unique(genus_list))
(1582 / 32170) * 100
4.9%





# Obtenir les familles uniques de neu_bota$famille
familles_uniques <- unique(c(neu_bota$famille, fri_tax$family))
familles_uniques <- na.omit(familles_uniques)

# Créer une liste pour stocker les genres uniques associés à chaque famille
liste_genres_par_famille <- list()

# Pour chaque famille unique
for (famille in familles_uniques) {
  # Trouver les genres associés à cette famille dans classif$genus
  genres_associes <- unique(classif$genus[classif$family == famille])
  # Ajouter les genres associés à cette famille à la liste
  liste_genres_par_famille[[famille]] <- genres_associes
}

#dataframe avec nombre de genre par famille possible et disponible 
noms_famille <- names(liste_genres_par_famille)
genus_possible <- sapply(liste_genres_par_famille, length)
df_genus_family <- data.frame(genus_possible)

# Comptage du nombre de genres uniques par famille
nb_genres_famille <- rbind(
  data.frame(family = neu_bota$famille, genus = neu_bota$genre),
  data.frame(family = fri_tax$family, genus = fri_tax$genus)
)

df_bota <- nb_genres_famille %>%
  group_by(family) %>%
  summarise(nb_genus = n_distinct(genus))


# Fusion des dataframes par leurs noms de lignes
df_genus_family$family <- rownames(df_genus_family)
distrib_phylo <- merge(df_bota, df_genus_family, by = "family")
distrib_phylo <- subset(distrib_phylo, genus_possible != 0)
distrib_phylo$pourcentage <- (distrib_phylo$nb_genus / distrib_phylo$genus_possible) * 100

summary(distrib_phylo$pourcentage)

distrib_phylo$family <- as.factor(distrib_phylo$family)

# Création de la colonne "jardin" dans distrib_phylo
distrib_phylo$jardin <- NA

# Parcourir chaque famille dans distrib_phylo
for (famille in distrib_phylo$family) {
  # Vérifier si la famille est présente uniquement dans neu_bota
  if (famille %in% neu_bota$famille & !famille %in% fri_tax$family) {
    distrib_phylo$jardin[distrib_phylo$family == famille] <- "neu"
  }
  # Vérifier si la famille est présente uniquement dans fri_tax
  else if (!famille %in% neu_bota$famille & famille %in% fri_tax$family) {
    distrib_phylo$jardin[distrib_phylo$family == famille] <- "fri"
  }
  # Vérifier si la famille est présente dans les deux dataframes
  else if (famille %in% neu_bota$famille & famille %in% fri_tax$family) {
    distrib_phylo$jardin[distrib_phylo$family == famille] <- "twice"
  }
}

# Création du graphique à points avec les modifications demandées
graph_famille <- ggplot(distrib_phylo, aes(x = family, y = pourcentage, color = jardin)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("neu" = "#14A2D7", "fri" = "red", "twice" = "purple"),
                     labels = c("neu" = "Neuchâtel", 
                                "fri" = "Fribourg", 
                                "twice" = "Both")) +
  labs(x = "Family", y = "Pourcentages of genus present in family", color = "Botanical garden") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 3)) +  # Réduire la taille des écritures sur l'axe des x
  scale_x_discrete(expand = c(0, 5))  # Augmenter l'espace entre chaque valeur sur l'axe x

ggsave("D:/Thèse Metabolomique/Thèse Metabolomique/jardin botanique/graph_familly.png", graph_famille)



######################################################
##########         ARBRE PHYLO           ###############
######################################################

  taxonomy <-
    read_delim(
      archive_read("D:/open_tree_of_life/ott3.3.tgz", file = "ott3.3/taxonomy.tsv"),
      col_types = cols(),
      delim = "|",
      escape_double = FALSE,
      trim_ws = TRUE
    )

  taxonomy %>%
    filter(rank == "domain")

  # taxonomy %>%
  #  filter(name =="Asparagales")

  # Here we use I graph to create a graph from the datatable of OTL tax input

  sub_tax_from_to <- data.frame(
    from = taxonomy$parent_uid,
    to = taxonomy$uid
  )

  g <- graph.data.frame(sub_tax_from_to, directed = TRUE)

  # plot(g)

  # print_all(g)

  # And the subcomponent to filter for all node below Archaeplastida https://igraph.org/r/doc/subcomponent.html

  sub_g <-
    subcomponent(g, "304358", mode = "out") ## Eukaryota  ott id

  g2 <- induced_subgraph(g, sub_g)

  id_sel <- as.numeric(V(g2)$name)

  taxonomy_final <- taxonomy[taxonomy$uid %in% id_sel, ]

  taxonomy_family <- taxonomy_final %>%
    filter(rank == "family" & is.na(flags))

  taxonomy_family_full <- get_lineage_taxo(taxonomy_family)




######################################################
##########              GIFT           ###############
######################################################

misc_env <- GIFT_env_meta_misc()
raster_env <- GIFT_env_meta_raster()


wc2.0_bio_30s_01                       Annual Mean Temperature
wc2.0_bio_30s_12                       Annual Precipitation
MODCF_meanannual                       Mean annual cloud frequency
pet_he_yr                              Potential Evapo-Transpiration
ai_yr                                  Global Aridity Index
mn30_grd_slope                         Slope
mn30_grd                               Altitude
3DGlobalVeg_L3C                        Canopy Height
CHELSA_NFD                             Number of frost days
########


env_bota <- rbind(
  data.frame(species = fri_bota$matched_name, genus = fri_bota$organism_otol_genus),
  data.frame(species = neu_bota$determination_acceptee, genus = neu_bota$genre)
)

#neu_bota_viv <- neu_bota[!is.na(neu_bota$vivant), ]
#env_bota <- data.frame(species = neu_bota_viv$determination_acceptee, genus = neu_bota_viv$genre)

env_bota <- env_bota[!duplicated(env_bota),]

env_bota <- env_bota %>% filter(species != ""& genus != "")


env_bota <- env_bota %>% mutate(epithet = sapply(strsplit(as.character(species), " "), function(x) x[2]))
variable_list <- c("wc2.0_bio_30s_01", "wc2.0_bio_30s_12", "MODCF_meanannual", "pet_he_yr", "ai_yr", "mn30_grd_slope", "mn30_grd", "3DGlobalVeg_L3C", "CHELSA_NFD")
env_bota <- subset(env_bota, !is.na(genus) & !is.na(epithet))



# Initialisation de la barre de progression
total_iterations <- nrow(env_bota)
progress <- 0

# Initialisation de la liste pour stocker les résultats agrégés
df_loca <- data.frame(stringsAsFactors = FALSE)


# Boucle à travers chaque ligne de env_bota
for (i in 1:total_iterations) {
  # Extraire les valeurs "genus" et "epithet" de la ligne i
  current_genus <- env_bota$genus[i]
  current_epithet <- env_bota$epithet[i]
  
  # Appeler la fonction GIFT_species_lookup
  current_lookup <- GIFT_species_lookup(genus = current_genus, epithet = current_epithet)
  
  # Appeler la fonction GIFT_species_distribution
  current_distribution <- GIFT_species_distribution(
    genus = current_genus, 
    epithet = current_epithet, 
    aggregation = TRUE
  )
 

  # Appeler la fonction GIFT_env
  current_entity_ID <- unique(current_distribution$entity_ID)
  current_data_env <- GIFT_env(entity_ID = current_entity_ID,
                               miscellaneous = c("longitude", "latitude"),
                               rasterlayer = variable_list,
                               sumstat = c("mean"))
  
 
  # Vérifier si current_data_env est vide
  if(nrow(current_data_env) != 0) {
    current_data_env$species <- paste(current_genus, current_epithet, sep = " ")
    df_loca <- rbind(df_loca, current_data_env)
    
  }
  
  # Mise à jour de la barre de progression
  progress <- progress + 1
  cat(sprintf("\rProgression: %d%%", round(progress / total_iterations * 100)))
}


df_loca_sauv <- df_loca
#data_env_sauv <- data_env

# Convertir toutes les colonnes en numérique, sauf la colonne 'species'
data_env[, -which(names(data_env) == "species")] <- lapply(data_env[, -which(names(data_env) == "species")], as.numeric)

# Enregistrer le dataframe en tant que fichier CSV
write.csv(df_loca, "D:/Thèse Metabolomique/Thèse Metabolomique/jardin botanique/data_env_gift_complet.csv", row.names = FALSE)


#espece sans valeur 
speciesNA <- data_env$species[which(is.na(data_env$mean_wc2.0_bio_30s_01))]

##################################
####    OPTIMUM RASTER      ######
##################################
library(dismo)
library(raster)
library(sf)
library(enmSdmX)
library(rgugik)
library(sfheaders)
library(ggplot2)
library(viridis)
library(elevatr)
library(rayshader)
library(rinat)X 
library(rnaturalearth)
library(rnaturalearthdata)
library(geodata)
library(rgbif)
library(dplyr)
library(leaflet)
library(sp)X
library(FactoMineR)
library(factoextra)
library(missMDA)
library(mgcv)
library(terra)
library(ggspatial)
library(tidyverse)
library(MODIStsp)
library(RCGLS)

data_env <- read.csv("D:/Thèse Metabolomique/Thèse Metabolomique/jardin botanique/data_env.csv",sep=",")
speciesNA <- data_env$species[which(is.na(data_env$mean_wc2.0_bio_30s_01))]


# Initialisation de la barre de progression
progress <- 0
total_iterations <- length(speciesNA)

# Création d'un dataframe pour stocker les moyennes
mean_df <- data.frame(species = character(), stringsAsFactors = FALSE)

# Boucle pour chaque espèce dans la liste speciesNA
for (species_name in speciesNA) {
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
    longitude <- coordinates(data_gbif_wgs84)[, 1]
    latitude <- coordinates(data_gbif_wgs84)[, 2]
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

# Renommer les colonnes de mean_df
colnames(mean_df) <- c("species", colnames(data_loc))




##################################
####      Whitakker      ######
##################################

devtools::install_github("valentinitnelav/plotbiomes")

library(plotbiomes)


whit_part1 <- read.csv("D:/Thèse Metabolomique/Thèse Metabolomique/jardin botanique/data_env.csv",sep=",")
whit_part2 <- read.csv("D:/Thèse Metabolomique/Thèse Metabolomique/jardin botanique/data_env_restant.csv",sep=";")


data_env_select <- whit_part1[, c(1,2, 3)]
mean_df_select <- whit_part2[, c(1,4, 5)]
colnames(data_env_select) <- c("species", "temperature", "precipitation")
colnames(mean_df_select) <- c("species", "temperature", "precipitation")

mean_df_select$temperature <- mean_df_select$temperature / 10

data_clim <- rbind(mean_df_select, data_env_select)  
data_clim <- data_clim[!duplicated(data_clim$species), ]


# Convertir les données de la colonne "precipitation" de mm en cm
data_clim$precipitation <- as.numeric(data_clim$precipitation)
data_clim$temperature <- as.numeric(data_clim$temperature)
data_clim$species <- as.factor(data_clim$species)
data_clim$precipitation <- data_clim$precipitation / 10


neu_taxo <- read.csv("D:/Thèse Metabolomique/Thèse Metabolomique/jardin botanique/neu_bota_taxo.csv",sep=",")
fri_taxo <- read.csv("D:/Thèse Metabolomique/Thèse Metabolomique/jardin botanique/species_list_croisee_final.csv",sep=",")

neu_jardin <- data.frame(species = neu_taxo$species, neu = "neu")
fri_taxo <- data.frame(species = fri_taxo$matched_name, neu = "fri")

# Créer les dataframes neu_jardin et fri_taxo
neu_jardin <- data.frame(species = neu_taxo$species, neu = "neu")
fri_taxo <- data.frame(species = fri_taxo$matched_name, neu = "fri")

# Fusionner les dataframes en utilisant la colonne "species" comme clé de fusion
merged_df <- merge(neu_jardin, fri_taxo, by = "species", all = TRUE)

# Créer la colonne "jardin" en fonction de la présence dans les dataframes originaux
merged_df$jardin <- ifelse(!is.na(merged_df$neu.x), "neu", 
                           ifelse(!is.na(merged_df$neu.y), "fri", "twice"))

# Retirer les colonnes "neu.x" et "neu.y" maintenant inutiles
merged_df <- merged_df[, !(names(merged_df) %in% c("neu.x", "neu.y"))]

head(merged_df)


setdiff(data_clim$species,neu_taxo$species)


data_clim <- merge(data_clim, nb_genres_famille, by = "species")
summary(data_clim)



# Obtenez les valeurs minimales et maximales de la précipitation et de la température
temp_out_of_range <- subset(data_clim, temperature < -20 | temperature > 35)

summary(data_clim)

library(vegan)
library(plotbiomes)

whittaker_base_plot() +
  # add the temperature - precipitation data points
  geom_point(data = data_clim, 
             aes(x = temperature, 
                 y = precipitation), 
             size   = 3,
             shape  = 21,
             colour = "gray95", 
             fill   = "black",
             stroke = 1,
             alpha  = 0.5) +
  theme_bw()




  
##################################
####     carte      ######
##################################

library(mapchina)
library(sf)
library(spatstat)
library(tidyverse)
library(sf)
library(eks)
library(colorspace)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(rnaturalearth)
require(data.table)
library(dplyr)
library(drc)
library(ggplot2)
library(RColorBrewer)
# devtools::install_github("onofriAndreaPG/aomisc")
library(aomisc)
library(raster)
library(eks)
library(elevatr)
library(rayshader)
library(rayshader)
library(png)

df_loca <- read.csv("D:/Thèse Metabolomique/Thèse Metabolomique/jardin botanique/data_env_gift_complet.csv",sep=",")


dn <- na.omit(df_loca)

  sf_points <- data.frame(
    lat = as.numeric(c(dn$latitude,-85.00294,85.05113)),
    lon = as.numeric(c(dn$longitude,-180,180))
  ) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Creating an empty grid with value 1
empty_grid <- st_bbox(sf_points) %>%
  st_as_sfc() %>%
  st_make_grid(cellsize = c(1, 1))  # Adjust cellsize as per your requirement

empty_grid_sf <- st_sf(geometry = empty_grid)

centroids <- st_centroid(empty_grid_sf)

# Merge partial points with empty grid
sf_points <- rbind(sf_points, centroids)

  dn2 <- dn |>
    dplyr::select(latitude, longitude)

  # Replicates
  replicates <- dn2 |>
    dplyr::group_by(latitude, longitude) |>
    dplyr::summarise(count = n()) |>
    dplyr::ungroup()

  # Merge replicates count with original data
  dn2 <-
    left_join(dn2, replicates, by = c("latitude", "longitude"))
  dn2 <- distinct(dn2)
  # View the result
  head(dn2)

  skde1 <- st_kde(sf_points, gridsize = c(200, 200))

sf_use_s2(FALSE)
world_sf <- ne_countries(scale = "large", type = "countries", returnclass = "sf") 
dataxx = st_get_contour(skde1, cont = c(seq(1, 99, 1)), disjoint = FALSE)
world_sf <- ne_countries(returnclass = "sv")
world_sf <- sf::st_as_sf(world_sf)
ext_world2 <- as(raster::extent(-165, 175, -78, 83.6341), "SpatialPolygons")
world_sp <- crop(as_Spatial(world_sf), ext_world2)
world_sf2 <- st_as_sf(world_sp) 
elevation <- get_elev_raster(world_sf2, z = 2)

###################################################################
# Create 3D visualization of elevation data for Switzerland
world_sf2_inter <- crop(elevation, extent(world_sf2))
world_sf2_cut <- mask(world_sf2_inter, world_sf2)
elmat <- raster_to_matrix(elevation)
elmat[elmat < -1]  <- -1500

attr(elmat, "extent") <- extent(world_sf2)

###################

  sf_points2 <- data.frame(
    lat = as.numeric(dn$latitude),
    lon = as.numeric(dn$longitude)
  ) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)

coords <- st_coordinates(sf_points2)
coords_df <- st_coordinates(sf_points2)
# Rename columns
coords_df <- data.frame(coords_df)
names(coords_df) <- c("Longitude", "Latitude")
# Convert to numeric
coords_df$Longitude <- as.numeric(coords_df$Longitude)
coords_df$Latitude <- as.numeric(coords_df$Latitude)
# Create a matrix with unique coordinates and counts
coord_matrix <- matrix(NA, ncol = 2, nrow = length(unique(coords)))
colnames(coord_matrix) <- c("Coordinates", "Occurrences")

# Fill in the matrix
unique_coords <- unique(coords)
for (i in 1:length(unique_coords)) {
  coord_matrix[i, 1] <- unique_coords[i]
  coord_matrix[i, 2] <- sum(coords == unique_coords[i])
}

coord_matrix <- data.frame(coord_matrix)
# Print the matrix

###################


tofino <- khroma::color("davos")
gradient <- tofino(99)
gradient <- rev(gradient[1:99])

colors <- gradient[round(cumprod(rep(1.359, 15))-1)[2:15]]
colors <- gradient[c(1,10,15,20,30, 40, 50, 55, 60, 65, 70, 72,74,76,78, 80, 82, 84, 86, 88, 90, 92, 94, 96, 98)]
  # Create a color gradient based on the defined colors

gradient_func <- colorRampPalette(colors)

gradient <- gradient_func(99)

###################
map_png <- tempfile(tmpdir = "C:/Users/laboureaum2/Desktop")
#elevation.texture.map <- readPNG("./inst/data/earth.png")
Lotus_plant_map <- elmat %>%
  sphere_shade(texture = "desert") %>%
 add_overlay(generate_polygon_overlay(dataxx, 
                         linewidth=0, palette = gradient,
                        extent = extent(elevation), heightmap = elmat),
                        alphalayer=0.8) %>%
#add_overlay(elevation.texture.map, alphacolor = NULL, alphalayer = 0.4) %>% 
add_water(detect_water(elmat,cutoff = 0.35,min_area = length(elmat)/2000), color = "imhof1") %>%

#add_overlay(generate_point_overlay(sf_points2, color= rgb(25/255, 25/255, 112/255, alpha = 0.5),pch=19, size=coord_matrix$Occurrences/100,
#attr(elevation,"extent"), heightmap = elmat))  
save_png(map_png)

rayshader::plot_map(Lotus_plant_map)


