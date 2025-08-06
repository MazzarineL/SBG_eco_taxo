
whit_part1.1 <- read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/gift/data_env_gift_part1.csv"), sep = ";")
whit_part1.2 <-  read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/gift/data_env_gift_part2.csv"), sep = ";")
whit_part1.3 <-  read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/gift/data_env_gift_part3.csv"), sep = ";")
whit_part1.4 <-  read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/gift/data_env_gift_part1.4.csv"), sep = ",")
whit_part2 <-  read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/gift/data_env_gift_part4.csv"), sep = ";")
whit_part1.5 <-  read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/gift/data_env_gift_geneve.csv"), sep = ",")
whit_part1.6 <-  read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/gift/data_env_gift_champex.csv"), sep = ",")
whit_part1.7 <-  read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/gift/data_env_gift_prague.csv"), sep = ",")
whit_part1.8 <-  read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/gift/data_env_gift_london.csv"), sep = ",")

whit_part1.4 <- whit_part1.4 %>% dplyr::select(-biome)
whit_part1.5 <- whit_part1.5 %>% dplyr::select(-biome)
whit_part1.6 <- whit_part1.6 %>% dplyr::select(-biome)
whit_part1.7 <- whit_part1.7 %>% dplyr::select(-biome)
whit_part1.8 <- whit_part1.8 %>% dplyr::select(-biome)

whit_part1 <- rbind(whit_part1.1, whit_part1.2,whit_part1.3,whit_part1.4,whit_part1.5,whit_part1.6,whit_part1.7,whit_part1.8)  

cover_species_garden_full <- read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/taxo_species_garden.csv") )


    cover_whit <- cover_species_garden_full
    #input_code <- input$Garden
    
    whit_part1$mean_wc2.0_bio_30s_12 <- as.numeric(whit_part1$mean_wc2.0_bio_30s_12)
    whit_part1$mean_wc2.0_bio_30s_01 <- as.numeric(whit_part1$mean_wc2.0_bio_30s_01)

    whit_part1 <- whit_part1 %>%
        dplyr::select(species, where(is.numeric)) %>%
        dplyr::group_by(species) %>%
        dplyr::summarise_all(mean, na.rm = TRUE)

    whit_part2 <- whit_part2[!duplicated(whit_part2$species), ]
    data_env_select <- whit_part1[, c(1,4,5)]
    mean_df_select <- whit_part2[, c(1,4, 5)]

    colnames(data_env_select) <- c("species","temperature", "precipitation")
    colnames(mean_df_select) <- c("species", "temperature", "precipitation")
    mean_df_select$temperature <- mean_df_select$temperature / 10
    data_clim <- rbind(mean_df_select, data_env_select)  
    data_clim$precipitation <- as.numeric(data_clim$precipitation)
    data_clim$temperature <- as.numeric(data_clim$temperature)
    data_clim$species <- as.factor(data_clim$species)
    data_clim$precipitation <- data_clim$precipitation / 10

    unique_species <- unique(cover_whit$species)
    for (species in unique_species) {
      select_taxo <- cover_whit[cover_whit$species == species, ]
      unique_gardens <- unique(select_taxo$garden)
      sorted_gardens <- sort(unique_gardens)
      code_garden <- paste(sorted_gardens, collapse = "_")
      cover_whit$code_garden[cover_whit$species == species] <- code_garden
    }

cover_whit <- cover_whit %>%
  distinct(species, .keep_all = TRUE)

      data_clim <- merge(data_clim, cover_whit, by = "species")
    data_clim <- data_clim %>% dplyr::mutate(temperature = ifelse(species %in% c("Drosera spathulata", "Duvalia modesta"), 25, temperature))
    data_clim <- data_clim %>% filter(!is.na(temperature))
    data_clim <- data_clim[!duplicated(data_clim$species), ]


write.csv(data_clim, "D:/gitrepo/SBG_eco_taxo/data/gift/data_env_gift_fusion.csv", row.names = FALSE)
