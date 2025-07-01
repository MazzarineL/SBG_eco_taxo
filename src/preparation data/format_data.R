library(myTAI)
library(dplyr)
library(GIFT)
library(igraph)
library(readxl)
library(TNRS)

### Définir le dossier de travail
folder <- "D:/gitrepo/SBG_eco_taxo/data/botanical_garden_list/"

##########################################
### NEUCHÂTEL

list_neuch_acq <- read_excel(file.path(folder, "acquisitionExport_neu.xlsx"))
list_neuch_cult <- read_excel(file.path(folder, "cultivatedExport_neu.xlsx"))

# Nettoyage des noms de colonnes
clean_names <- function(df) {
  names(df) <- gsub(" ", "_", names(df))
  names(df) <- tolower(names(df))
  names(df) <- gsub("[éè]", "e", names(df))
  names(df) <- gsub("[^a-z0-9_]", "", iconv(names(df), "latin1", "ASCII", sub = ""))
  return(df)
}

list_neuch_cult <- clean_names(list_neuch_cult)
list_neuch_cult <- data.frame(list_neuch_cult)
list_neuch_cult$numero_de_specimen_cultive <- substr(list_neuch_cult$numero_de_specimen_cultive, 1, 8)

list_neuch_acq <- clean_names(list_neuch_acq)
list_neuch_acq <- data.frame(list_neuch_acq)
list_neuch_acq$numero_de_specimen_acquis <- substr(list_neuch_acq$numero_de_specimen_acquis, 1, 8)

list_neuch <- merge(
  list_neuch_acq, list_neuch_cult,
  by.x = "numero_de_specimen_acquis",
  by.y = "numero_de_specimen_cultive",
  all = TRUE
)
list_neuch <- list_neuch[!is.na(list_neuch$vivant), ]

# Fusion des colonnes en doublon (.x / .y)
for (col_name in names(list_neuch)) {
  if (endsWith(col_name, ".x")) {
    col_name_y <- sub("\\.x$", ".y", col_name)
    if (col_name_y %in% names(list_neuch)) {
      # Récupère les colonnes
      x_col <- list_neuch[[col_name]]
      y_col <- list_neuch[[col_name_y]]

      # Crée la version fusionnée sans duplication
      merged_column <- ifelse(!is.na(x_col) & x_col == y_col, x_col,
                              ifelse(!is.na(x_col) & is.na(y_col), x_col,
                                     ifelse(is.na(x_col) & !is.na(y_col), y_col,
                                            paste(x_col, y_col, sep = " "))))

      # Nettoyage : retirer les doublons du type "Anchusa Anchusa"
      merged_column <- gsub("\\b(\\w+)\\b\\s+\\1\\b", "\\1", merged_column)

      # Mise à jour du nom et des données
      clean_name <- sub("\\.x$", "", col_name)
      list_neuch[[clean_name]] <- merged_column

      # Supprime les colonnes originales
      list_neuch[[col_name]] <- NULL
      list_neuch[[col_name_y]] <- NULL
    }
  }
}

list_neuch$garden <- "ne"
##########################################
### FRIBOURG

list_fri <- read.csv(file.path(folder, "list_fribourg.csv"))

fri_tax <- list_fri %>%
  transmute(
    species = matched_name,
    genus = organism_otol_genus,
    family = taxonFamille1,
    garden = "fr"
  ) %>%
  filter(species != "", genus != "", family != "")

##########################################
### LAUSANNE

list_lau <- read_excel(file.path(folder, "list_lausanne.xlsx"))

lau_inv_subset <- list_lau %>%
  select(Taxons) %>%
  mutate(Index = row_number()) %>%
  distinct(Taxons, .keep_all = TRUE) %>%
  na.omit()

  lau_inv_subset <- lau_inv_subset %>% select(Index, Taxons)


lau_resolved <- TNRS(
  lau_inv_subset,
  sources = c("wcvp", "wfo"),
  classification = "wfo",
  mode = "resolve",
  matches = "best"
)

lau_tax <- data.frame(
  species = lau_resolved$Name_matched,
  genus = lau_resolved$Genus_matched,
  family = lau_resolved$Name_matched_accepted_family,
  garden = "la"
)

##########################################
### GENÈVE

list_geneve <- read_excel(file.path(folder, "list_geneva.xlsx"))

gen_tax <- data.frame(
  species = paste(list_geneve$genre, list_geneve$espece),
  genus = list_geneve$genre,
  family = list_geneve$famille,
  garden = "ge"
)

##########################################
### CHAMPEX

list_champex <- read_excel(file.path(folder, "list_champex.xlsx"))

cha_tax <- data.frame(
  species = paste(list_champex$Genre_nouveau, list_champex$Sps_nouveau),
  genus = list_champex$Genre_nouveau,
  family = list_champex$Famille,
  garden = "ch"
)
