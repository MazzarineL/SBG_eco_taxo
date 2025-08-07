source("D:/gitrepo/SBG_eco_taxo/src/preparation data/format_data.R")


library(ggplot2)
library(dplyr)
library(ggtree)

######## Mise en forme des data #####
#### format : genre, garden #########

neu_bota_G <- list_neuch %>%
  dplyr::select(genre, garden)

neu_bota_G <- neu_bota_G %>%
  dplyr::rename(genus = genre) 

fri_tax_G <- fri_tax %>%
  dplyr::select(genus, garden) 

lau_res_G <- lau_tax %>%
  dplyr::select(genus, garden) 

gen_res_G <- gen_tax %>%
  dplyr::select(genus, garden) 

cha_res_G <- cha_tax %>%
  dplyr::select(genus, garden)

pra_res_G <- pra_tax %>%
  dplyr::select(genus, garden)

lon_res_G <- lon_tax %>%
  dplyr::select(genus, garden)

taxonomy_family_Garden <- rbind(fri_tax_G,neu_bota_G,lau_res_G,gen_res_G,cha_res_G,pra_res_G,lon_res_G)
taxonomy_family_Garden <- taxonomy_family_Garden %>%
  mutate(pres = 1)
#uniquement si derniere ligne vide
#taxonomy_family_Garden <- taxonomy_family_Garden[-nrow(taxonomy_family_Garden), ]

######## Taxonomy complete des genre #####
taxonomy_family_full = read.csv("D:/gitrepo/SBG_eco_taxo/data/taxonomy_family_genus.csv")

taxonomy_family_full <- taxonomy_family_full %>% dplyr::rename(genus = name)
taxonomy_family_full <- taxonomy_family_full %>% dplyr::rename(family = name.family)

 
taxonomy_merge <-
      left_join(taxonomy_family_full,
        taxonomy_family_Garden,
        by = c("genus"))



taxonomy_merge$pres <- ifelse(is.na(taxonomy_merge$pres), 0, taxonomy_merge$pres)
taxonomy_merge <- taxonomy_merge[!is.na(taxonomy_merge$family),]


taxonomy_merge$code_garden <- NA
select_taxo <- data.frame()
unique_genera <- unique(taxonomy_merge$genus)

for (genus in unique_genera) {
  
  select_taxo <- taxonomy_merge[taxonomy_merge$genus == genus, ]
  
  unique_gardens <- unique(select_taxo$garden)
  
  sorted_gardens <- sort(unique_gardens)
  code_garden <- paste(sorted_gardens, collapse = "_")
  
  taxonomy_merge$code_garden[taxonomy_merge$genus == genus] <- code_garden
}
taxonomy_merge$code_garden[taxonomy_merge$code_garden == ""] <- NA

write.csv(taxonomy_merge, "D:/gitrepo/SBG_eco_taxo/data/taxo_genus_garden.csv", row.names = FALSE)






######## dataframe espece, genre, famille ,garden #####
neu_bota_S <- list_neuch %>%
  dplyr::select(espece,genre,famille,garden)

neu_bota_S <- neu_bota_S %>%
  dplyr::rename(species = espece) 

neu_bota_S <- neu_bota_S %>%
dplyr::rename(genus = genre)

neu_bota_S <- neu_bota_S %>%
  mutate(species = paste(genus, species, sep = " "))
 
neu_bota_S <- neu_bota_S %>%
  dplyr::rename(family = famille) 

taxonomy_complete_Garden <- rbind(fri_tax,neu_bota_S,lau_tax,gen_tax,cha_tax,pra_tax,lon_tax)

write.csv(taxonomy_complete_Garden, "D:/gitrepo/SBG_eco_taxo/data/cover_species_garden.csv", row.names = FALSE)






######## Taxonomy complete des especes #####
taxonomy_family_full = read.csv("D:/gitrepo/SBG_eco_taxo/data/taxonomy_family_genus.csv")

taxonomy_family_full <- taxonomy_family_full %>% dplyr::rename(genus = name)
taxonomy_family_full <- taxonomy_family_full %>% dplyr::rename(family = name.family)

 
taxonomy_merge <-
      left_join(taxonomy_family_full,
        taxonomy_complete_Garden,
        by = c("genus"))


taxonomy_merge$pres <- 1 

taxonomy_merge <- taxonomy_merge %>%
  mutate(pres = ifelse(is.na(garden), 0, pres))


taxonomy_merge$code_garden <- NA
select_taxo <- data.frame()
unique_species <- unique(taxonomy_merge$species)

for (species in unique_species) {
  select_taxo <- taxonomy_merge[taxonomy_merge$species == species, ]
  
  unique_gardens <- unique(select_taxo$garden)
  
  sorted_gardens <- sort(unique_gardens)
  code_garden <- paste(sorted_gardens, collapse = "_")
  
  taxonomy_merge$code_garden[taxonomy_merge$species == species] <- code_garden
}

taxonomy_merge <- taxonomy_merge %>% dplyr::rename(family = family.x)
taxonomy_merge <- taxonomy_merge %>% dplyr::select(-family.y)


write.csv(taxonomy_merge, "D:/gitrepo/SBG_eco_taxo/data/taxo_species_garden.csv", row.names = FALSE)


result_lo <- taxonomy_merge %>%
  filter(code_garden == "lo")
fix(result_lo)

unique(result_lo$family)









#######    family coverage ##################################
taxonomy_family_full = read.csv("D:/gitrepo/SBG_eco_taxo/data/taxonomy_family_genus.csv")

taxonomy_family_full <- taxonomy_family_full %>% dplyr::rename(genus = name)
taxonomy_family_full <- taxonomy_family_full %>% dplyr::rename(family = name.family)

 
taxonomy_merge <-
      left_join(taxonomy_family_full,
        taxonomy_family_Garden,
        by = c("genus"))



taxonomy_merge$pres <- ifelse(is.na(taxonomy_merge$pres), 0, taxonomy_merge$pres)
taxonomy_merge <- taxonomy_merge[!is.na(taxonomy_merge$family),]


taxonomy_merge$code_garden <- NA
select_taxo <- data.frame()
unique_family <- unique(taxonomy_merge$family)

for (family in unique_family) {
  
  select_taxo <- taxonomy_merge[taxonomy_merge$family == family, ]
  
  unique_gardens <- unique(select_taxo$garden)
  
  sorted_gardens <- sort(unique_gardens)
  code_garden <- paste(sorted_gardens, collapse = "_")
  
  taxonomy_merge$code_garden[taxonomy_merge$family == family] <- code_garden
}
taxonomy_merge$code_garden[taxonomy_merge$code_garden == ""] <- NA


write.csv(taxonomy_merge, "D:/gitrepo/SBG_eco_taxo/data/taxo_family_garden.csv", row.names = FALSE)




cover_family_garden_full <- read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/taxo_family_garden.csv") )
cover_genus_garden_full <- read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/taxo_genus_garden.csv") )
cover_species_garden_full <- read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/taxo_species_garden.csv") )
all_species_taxo <-  read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/all_species_taxonomy_full.csv"), sep = ",")

all_species_taxo <-  read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main//data/botanical_garden_list/list_geneva.csv"), sep = ",")

/data/botanical_garden_list/list_geneva
#LONDON
result <- cover_family_garden_full %>%
  filter(code_garden == "lo")

species_list <- cover_species_garden_full %>%
  filter(family %in% unique(result$family)) %>%
  pull(species) %>%
  unique()

result_genus <- cover_genus_garden_full %>%
  filter(code_garden == "lo")

genus_list <- lon_tax %>%
  filter(genus %in% unique(result_genus$genus)) %>%
  pull(species) %>%
  unique()

list_family_londre <- data.frame(species_list)
list_genus_londre <- data.frame(genus_list)

colnames(list_family_londre)[1] <- "species"
colnames(list_genus_londre)[1] <- "species"
list_family_londre$target <- "family"
list_genus_londre$target <- "genus"

merged_london <- rbind(list_family_londre,list_genus_londre)

# Résultat
merged_london <- as.data.frame(merged_london)
merged_london <- unique(merged_london)

final_london <- merge(
  list_kew,
  merged_list,
  by.x = "Accepted.Name",
  by.y = "species",
  all = FALSE
)

final_london <- final_london %>%
  group_by(Catalogue.Number) %>%
  filter(!(n() > 1 & target == "genus")) %>%
  ungroup()
  

#PRAGUE
result <- cover_family_garden_full %>%
  filter(code_garden == "pr")

species_list <- cover_species_garden_full %>%
  filter(family %in% unique(result$family)) %>%
  pull(species) %>%
  unique()

result_genus <- cover_genus_garden_full %>%
  filter(code_garden == "pr")

genus_list <- pra_tax %>%
  filter(genus %in% unique(result_genus$genus)) %>%
  pull(species) %>%
  unique()

list_family_prague <- data.frame(species_list)
list_genus_prague <- data.frame(genus_list)

colnames(list_family_prague)[1] <- "species"
colnames(list_genus_prague)[1] <- "species"
list_family_prague$target <- "family"
list_genus_prague$target <- "genus"

merged_prague <- rbind(list_family_prague,list_genus_prague)

# Résultat
merged_prague <- as.data.frame(merged_prague)
merged_prague <- unique(merged_prague)
merged_prague <- merged_prague[!is.na(merged_prague$species), ]




#GENEVA
result <- cover_family_garden_full %>%
  filter(code_garden == "ge")

species_list <- cover_species_garden_full %>%
  filter(family %in% unique(result$family)) %>%
  pull(species) %>%
  unique()

result_genus <- cover_genus_garden_full %>%
  filter(code_garden == "ge")

genus_list <- gen_tax %>%
  filter(genus %in% unique(result_genus$genus)) %>%
  pull(species) %>%
  unique()

list_family_geneva <- data.frame(species_list)
list_genus_geneva <- data.frame(genus_list)

colnames(list_family_geneva)[1] <- "species"
colnames(list_genus_geneva)[1] <- "species"
list_family_geneva$target <- "family"
list_genus_geneva$target <- "genus"

merged_geneva <- rbind(list_family_geneva,list_genus_geneva)

# Résultat
merged_geneva <- as.data.frame(merged_geneva)
merged_geneva <- unique(merged_geneva)



list_geneve <- data.frame(
  species = paste(list_geneve$genre, list_geneve$espece)
)

final_geneve <- merge(
  list_geneve,
  merged_geneva,
  by.x = "species",
  by.y = "species",
  all = FALSE
)

final_geneve <- final_geneve %>%
  group_by(Code ipen) %>%
  filter(!(n() > 1 & target == "genus")) %>%
  ungroup()
colnames(final_geneve)
