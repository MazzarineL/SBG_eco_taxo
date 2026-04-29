#install.packages(c("shiny", "rsconnect", "ggplot2", "dplyr", "ggtree", "rotl", 
#                   "slider", "gt", "plotbiomes", "rgbif", "sp", "Polychrome",
#                   "rinat", "RColorBrewer", "curl", "maps", "ggvenn","VennDiagram","gridExtra","BiocManager","devtools","UpSetR"))

library(DT)
library(BiocManager) 
library(shiny) 
library(rsconnect) 
library(ggplot2) 
library(dplyr) 
library(devtools) 
#BiocManager::install("ggtree")
library(ggtree) 
library(rotl)  
library(slider) 
library(tidyquant)  
library(gt)  
#devtools::install_github("valentinitnelav/plotbiomes")
library(plotbiomes) 
library(rgbif) 
library(sp) 
library(rinat)
library(RColorBrewer)
library(curl) 
library(maps)
library(Polychrome)
library(VennDiagram)
library(ggvenn)
library(gridExtra)
library(httr)
library(jsonlite)
library(stringr)
library(sf)
library(rmapshaper)
library(nngeo)
library(stringi)
library(stringr)
library(ggspatial)
library(tibble)
library(tidyr) 
library(colorspace)


clean_family <- function(fam_vec) {
  fam_vec <- trimws(fam_vec) # enlève espaces autour
  fam_vec <- gsub("\\?", "", fam_vec) # supprime les ?
  fam_vec <- gsub("\\s+", " ", fam_vec) # supprime espaces multiples
  fam_vec <- stringr::str_squish(fam_vec) # nettoyage extra
  fam_vec
}
# Définir le serveur
server <- function(input, output, session) {

world <- map_data("world")

cover_family_garden_full <- read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/taxo_family_garden.csv") )
cover_genus_garden_full <- read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/taxo_genus_garden.csv") )
cover_species_garden_full <- read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/taxo_species_garden.csv") )
all_species_taxo <-  read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/all_species_taxonomy_full.csv"), sep = ",")

list_geneve <-  read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/botanical_garden_list/list_geneva.csv"), sep = ";")
list_lausanne <-  read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/botanical_garden_list/list_lausanne.csv"), sep = ";")
list_prague <-  read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/botanical_garden_list/list_prague.csv"), sep = ";")


list_kew_PoW <-  read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/botanical_garden_list/kew_PoW_list.csv"), sep = ";")
list_kew_RG <-  read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/botanical_garden_list/kew_rock_garden_list.csv"), sep = ",")
list_kew_TH <-  read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/botanical_garden_list/kew_temperate_house_list.csv"), sep = ";")

list_london <- bind_rows(list_kew_TH, list_kew_PoW, list_kew_RG)



observe({
  cleaned_families <- sort(unique(clean_family(cover_species_garden_full$family)))
  
  updateSelectInput(
    session,
    inputId = "family",
    choices = cleaned_families,
    selected = cleaned_families[1]  # ou "" si tu veux rien sélectionner par défaut
  )
})

gift_fusion <-  read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/gift/data_env_gift_fusion.csv"), sep = ",")

# 1. Codes de base des jardins (à compléter selon ton jeu complet)
base_gardens <- c("fr", "ne", "la", "ge", "ch", "lo", "pr")

# 2. Générer toutes les combinaisons possibles (1 à length(base_gardens))
generate_combinations <- function(gardens) {
  combis <- unlist(lapply(1:length(gardens), function(n) {
    apply(combn(gardens, n), 2, function(x) paste(sort(x), collapse = "_"))
  }))
  c(combis, "NA")  # ajoute "NA"
}
family_levels <- generate_combinations(base_gardens)

# 3. Générer palette de couleurs unique adaptée au nombre de combis
# Tous les niveaux à colorier
n_colors <- length(family_levels)

# Couleur grise pour NA
color_values <- rep(NA, n_colors)
names(color_values) <- family_levels
color_values[is.na(names(color_values))] <- "grey"
color_values["NA"] <- "grey"
# Couleurs très distinctes pour les jardins spécifiques
distinct_colors <- qualitative_hcl(length(base_gardens), palette = "Dark 3")
names(distinct_colors) <- base_gardens

# Appliquer les couleurs distinctes aux jardins uniques
color_values[names(color_values) %in% base_gardens] <- distinct_colors[names(color_values)[names(color_values) %in% base_gardens]]

# Pour les autres combinaisons, générer des teintes variées (HSV) excluant les déjà assignées
remaining_labels <- setdiff(family_levels, c(names(color_values)[!is.na(color_values)]))

# Génération de couleurs variées par HSV
remaining_n <- length(remaining_labels)
if (remaining_n > 0) {
  hsv_colors <- grDevices::hsv(
    h = seq(0, 1, length.out = remaining_n + 1)[-1],   # Teintes réparties
    s = runif(remaining_n, 0.6, 1),                    # Saturation aléatoire élevée
    v = runif(remaining_n, 0.7, 1)                     # Valeur aléatoire élevée
  )
  names(hsv_colors) <- remaining_labels
  color_values[remaining_labels] <- hsv_colors
}



# Fonction de labels mise à jour 
generate_labels <- function(family_levels) {
  labels <- sapply(family_levels, function(code) {
    locations <- c()
    if (grepl("fr", code)) locations <- c(locations, "Fribourg")
    if (grepl("ge", code)) locations <- c(locations, "Geneva")
    if (grepl("la", code)) locations <- c(locations, "Lausanne")
    if (grepl("ne", code)) locations <- c(locations, "Neuchâtel")
    if (grepl("ch", code)) locations <- c(locations, "Champex")
    if (grepl("lo", code)) locations <- c(locations, "London")
    if (grepl("pr", code)) locations <- c(locations, "Prague")

    if (length(locations) > 0) {
      paste("Available in", paste(unique(locations), collapse = ", "))
    } else {
      "Not available"
    }
  })
  names(labels) <- family_levels
  return(labels)
}

labels <- generate_labels(family_levels)

# Mapping lisible pour les légendes
replacement_mapping <- function(family_levels) {
  labels <- sapply(family_levels, function(code) {
    locations <- c()
    if (grepl("fr", code)) locations <- c(locations, "Fribourg")
    if (grepl("ge", code)) locations <- c(locations, "Geneva")
    if (grepl("la", code)) locations <- c(locations, "Lausanne")
    if (grepl("ne", code)) locations <- c(locations, "Neuchâtel")
    if (grepl("ch", code)) locations <- c(locations, "Champex")
    if (grepl("lo", code)) locations <- c(locations, "London")
    if (grepl("pr", code)) locations <- c(locations, "Prague")

    if (length(locations) > 0) {
      paste(paste(unique(locations), collapse = ", "))
    } else {
      "Not available"
    }
  })
  names(labels) <- family_levels
  return(labels)
}

replacement_mapping <- replacement_mapping(family_levels)


#####################################
#########PHYLO TREE GARDEN ##############
#####################################
observeEvent(input$action, {
  withProgress(message = 'Loading data...', value = 0, {
    output$treePlot <- NULL
    req(input$Garden != "")

    # Initialize variables
    taxonomy_merge <- cover_genus_garden_full
    input_code <-input$Garden   

    # Update progress
    incProgress(1/5, detail = "Processing garden codes...")
    
    # If only one option is selected
    if (length(input_code) == 1) {

  taxonomy_merge$code_garden <- ifelse(
    taxonomy_merge$garden == input_code &
      taxonomy_merge$pres != 0,
    input_code,
    NA
  )

}else {
      selected_values <- paste(input_code, collapse = "|")
      taxonomy_merge$code_garden[!grepl(selected_values, taxonomy_merge$code_garden)] <- NA
      entire_codes <- c("fr", "ne", "la", "ge","ch","lo","pr")
      diff <- setdiff(entire_codes, input_code)
      taxonomy_merge$code_garden <- gsub(paste(diff, collapse = "|"), "", taxonomy_merge$code_garden)
      taxonomy_merge$code_garden <- gsub("_+", "_", taxonomy_merge$code_garden)
      taxonomy_merge$code_garden <- gsub("^_|_$", "", taxonomy_merge$code_garden)
    }
    
    incProgress(2/6, detail = "Filtering data...")
    
    taxonomy_merge$pres[is.na(taxonomy_merge$pres)] <- 0
    taxonomy_merge <- taxonomy_merge %>%
      dplyr::mutate(code_garden = na_if(code_garden, ""))
    
    taxonomy_merge <- taxonomy_merge[!is.na(taxonomy_merge$ott_id.family), ]
    
    incProgress(3/5, detail = "Generating phylogenetic tree...")
 
    my_tree <- rotl::tol_induced_subtree(ott_ids = taxonomy_merge$ott_id.family)
    sp_name <- gsub("_.*", "", my_tree$tip.label)
    my_tree$tip.label <- sp_name
    family <- taxonomy_merge$family
    g <- split(family, taxonomy_merge$code_garden)
    
    incProgress(4/5, detail = "Creating plot...")
    
    output$treePlot <- renderPlot({
      isolate({
        tree_plot <- ggtree::ggtree(my_tree, layout = "circular") +
          geom_tiplab(size = 4, offset = 0.5)
        
        g2 <- ggtree::groupOTU(tree_plot, g, "family") +
          aes(color = family) +
          theme(legend.position = "right") +
          scale_color_manual(
            name = "Family",
            values = color_values,  
            labels = labels,        
            breaks = family_levels  
          ) +
          theme(
            legend.title = element_text(size = 20),
            legend.text = element_text(size = 15)
          )
        
        print(g2)
        
        output$downloadFullPlot <- downloadHandler(
          filename = function() {
            paste0("Tree_garden_plot_", Sys.Date(), ".pdf")
          },
          content = function(file) {
            ggsave(filename = file, plot = g2, device = "pdf", width = 50, height = 50, units = "cm")
          }
        )
      })
    })
    
    incProgress(5/5, detail = "Finalizing...")
  })
})



#####################################
#########PHYLO TREE FAMILY ##############
#####################################
observeEvent(c(input$actionfamily, input$genus_select), {
  withProgress(message ='Loading data...', value = 0, {
    req(input$Garden != "")

    family_test <- input$family
 output$onlygenus <- renderDT({ NULL })
output$mytable <- renderDT({ NULL })
output$FamilyPlot <- renderPlot({ NULL })
output$textgenus <- renderText({ NULL })
    genus_cover <- NULL
    genus_select <- input$genus_select
    input_code <- input$Garden
    cover_genus_garden <- cover_genus_garden_full
    
    # Nouvelle condition ajoutée
    if (genus_select > sum(cover_genus_garden$pres == 0)) {
      cover_genus_garden$pres[cover_genus_garden$pres == 0] <- 3
      final_best_df <- cover_genus_garden
      # Skip the rest of the script and proceed directly to split by 'pres' for plot coloring
      goto_split <- TRUE
    } else {
      goto_split <- FALSE
    }
    
    if (!goto_split) {

      # Étape de mise à jour de la progression
      incProgress(1/6, detail = "Preparing data...")
      
      if (length(input_code) == 1) {
        cover_genus_garden$code_garden <- ifelse(!grepl(paste(input_code, collapse = "|"), cover_genus_garden$code_garden), NA, cover_genus_garden$code_garden)
        cover_genus_garden$code_garden[!is.na(cover_genus_garden$code_garden)] <- paste(input_code, collapse = "_") 
      } else {
        selected_values <- paste(input_code, collapse = "|")
        cover_genus_garden$code_garden[!grepl(selected_values, cover_genus_garden$code_garden)] <- NA
        entier <-c("fr", "ne", "la", "ge","ch","lo","pr")
        diff <- setdiff(entier, input_code)
        cover_genus_garden$code_garden <- gsub(paste(diff, collapse = "|"), "", cover_genus_garden$code_garden)
        cover_genus_garden$code_garden <- gsub("_+", "_", cover_genus_garden$code_garden)
        cover_genus_garden$code_garden <- gsub("^_|_$", "", cover_genus_garden$code_garden)
      }
      
      incProgress(2/6, detail = "Filtering data...")
      
      unique_genera_count <- cover_genus_garden_full %>%
        filter(family == family_test) %>%
        distinct(genus) %>%
        nrow()
      
      if (unique_genera_count == 1) {
        output$onlygenus <- DT::renderDT({
  genus_line <- subset(cover_species_garden_full, family == input$family)
  
  genus_line <- genus_line %>%
    dplyr::select(any_of(c("species", "genus", "family", "garden", "pres")))
  
  datatable(
    genus_line,
    options = list(pageLength = 10, scrollX = TRUE),
    rownames = FALSE,
    class = "stripe hover compact"
  )
})

        output$textgenus <- renderText({
          "Tree not available, there is only one genus in this family"
        })
        output$FamilyPlot <- renderPlot({})
      } else {
        cover_genus_garden <- cover_genus_garden %>%
          filter(family == family_test)
        cover_genus_garden$pres[is.na(cover_genus_garden$pres)] <- 0
        cover_genus_garden <- cover_genus_garden %>%
           dplyr::mutate(code_garden = na_if(code_garden, ""))
        
        cover_genus_garden <- cover_genus_garden[!is.na(cover_genus_garden$ott_id.family), ]
        cover_genus_garden$genus <- gsub("^x ", "", cover_genus_garden$genus)
        cover_genus_garden$pres[is.na(cover_genus_garden$code_garden)] <- 0
        
        incProgress(3/6, detail = "Generating phylogenetic tree...")

incProgress(3/6, detail = "Generating phylogenetic tree...")

# Nettoyage et vérification des ott_ids
valid_ott_ids <- unique(na.omit(cover_genus_garden$uid))

if (length(valid_ott_ids) < 2) {
  showNotification("Not enough valid ott_ids to generate a phylogenetic tree.", type = "error")
  output$FamilyPlot <- renderPlot({ NULL })  # Ne pas afficher de plot
  output$mytable <- renderDT({ NULL })       # Ne pas afficher de table
  return(NULL)                               # Arrêt ici
}

max_ids <- 150  
if (length(valid_ott_ids) > max_ids) {
  valid_ott_ids <- sample(valid_ott_ids, max_ids)
  showNotification(
    paste("Tree reduced to", max_ids, "genera to avoid memory crash."),
    type = "warning"
  )
}


# Si OK, construire l'arbre

tree <- tryCatch({

  rotl::tol_induced_subtree(ott_ids = valid_ott_ids)

}, error = function(e) {

  # capture TOUTES les erreurs rotl (dont std::vector)
  showNotification(
    "Tree too large to construct — tree skipped.",
    type = "error",
    duration = 6
  )

  return(NULL)
})

if (is.null(tree)) {

  # on continue le script MAIS sans arbre
  final_best_df <- cover_genus_garden

  output$GenusPlot <- renderPlot({ NULL })

} else {

        p <- ggtree(tree) + geom_tiplab()
        
        df_rangement <- data.frame(genus = get_taxa_name(p))
        df_rangement <- merge(df_rangement, cover_genus_garden, by = "genus", all.x = TRUE, sort = FALSE)
        
        # Ajouter une colonne date...
        
        # Fonction pour ajuster les valeurs before, after, step...
        
        best_diff <- Inf
        best_dfs <- list()
        before_values <- 0:10
        after_values <- 0:10
        step_values <- 1:5
        
        for (before in before_values) {
          for (after in after_values) {
            for (step in step_values) {
              df_temp <- df_rangement %>%
                dplyr::mutate(
                  reg_7day = slide_dbl(
                    pres,
                    .f = ~sum(.x, na.rm = TRUE),
                    .before = before,
                    .after = after,
                    .step = step
                  )
                )
              
              # Gérer les NA dans reg_7day
              df_temp <- df_temp %>%
                 dplyr::mutate(
                  reg_7day = case_when(
                    is.na(reg_7day) & pres == 0 ~ 1,
                    is.na(reg_7day) & pres == 1 ~ 2,
                    TRUE ~ reg_7day
                  ),
                  pres = if_else(reg_7day == 0 & pres == 0, 3, pres)
                )
              
              count_3 <- sum(df_temp$pres == 3, na.rm = TRUE)
              
              # Vérifier si count_3 dépasse le seuil et passer à la boucle suivante si c'est le cas
              if (is.na(count_3) || count_3 > genus_select) {
                next
              }
              
              diff <- abs(count_3 - genus_select)
              
              if (diff == 0) {
                best_dfs[[length(best_dfs) + 1]] <- df_temp
              }
              
              if (diff < best_diff) {
                best_diff <- diff
                best_df <- df_temp
              }
            }
          }
        }
        
        # Fonction pour calculer la distance minimale entre les valeurs 3 dans la colonne 'pres'
        calculate_distance <- function(df) {
          indices <- which(df$pres == 3)
          if (length(indices) < 2) return(0)
          return(min(diff(indices)))
        }
        
        # Sélectionner le meilleur dataframe qui maximise la distance entre les valeurs 3 dans 'pres'
        max_distance <- -Inf
        final_best_df <- NULL
        
        for (df in best_dfs) {
          distance <- calculate_distance(df)
          if (distance > max_distance) {
            max_distance <- distance
            final_best_df <- df
          }
        }
        
        # Si best_dfs est vide, sélectionner les df avec la valeur de diff la plus proche de 0
        if (length(best_dfs) == 0) {
          min_diff <- Inf
          for (before in before_values) {
            for (after in after_values) {
              for (step in step_values) {
                df_temp <- df_rangement %>%
                  dplyr::mutate(
                    reg_7day = slide_dbl(
                      pres,
                      .f = ~sum(.x, na.rm = TRUE),
                      .before = before,
                      .after = after,
                      .step = step
                    )
                  )
                
                # Gérer les NA dans reg_7day
                df_temp <- df_temp %>%
                   dplyr::mutate(
                    reg_7day = case_when(
                      is.na(reg_7day) & pres == 0 ~ 1,
                      is.na(reg_7day) & pres == 1 ~ 2,
                      TRUE ~ reg_7day
                    ),
                    pres = if_else(reg_7day == 0 & pres == 0, 3, pres)
                  )
                
                count_3 <- sum(df_temp$pres == 3)
                
                # Vérifier si count_3 dépasse le seuil et passer à la boucle suivante si c'est le cas
                if (is.na(count_3) || count_3 > genus_select) {
                  next
                }
                
                diff <- abs(count_3 - genus_select)
                
                if (diff < min_diff) {
                  min_diff <- diff
                  best_df <- df_temp
                  
                }
              }
            }
          }
          final_best_df <- df_temp
        } 
        
        # Split par pres pour la couleur dans le plot
        incProgress(4/6, detail = "Preparing the table...")
        
        output$mytable <- DT::renderDT({
  df_rangement_priority <- final_best_df %>% filter(pres == 3) %>% dplyr::select(genus)
  
  length_to_pad <- (3 - length(df_rangement_priority$genus) %% 3) %% 3
  padded_genus <- c(df_rangement_priority$genus, rep(NA, length_to_pad))
  
  matrix_genus <- matrix(padded_genus, ncol = 3, byrow = TRUE)
  df_table <- as.data.frame(matrix_genus)
  
  datatable(
    df_table,
    options = list(pageLength = 10, scrollX = TRUE),
    rownames = FALSE,
    colnames = c("Genus 1", "Genus 2", "Genus 3"),
    class = "stripe hover compact"
  )
})
        
        output$downloadTable <- downloadHandler(
          filename = function() {
            paste("Priority_", family_test, ".csv", sep = "")
          },
          content = function(file) {
            df_rangement_priority <- df_rangement %>% filter(pres == 3) %>% dplyr::select(genus)
            write.csv(df_rangement_priority, file, row.names = FALSE)
          }
        )
        
        incProgress(5/6, detail = "Preparing render family tree...")
        
        output$FamilyPlot <- renderPlot({
          isolate({
            tree_family <- ggtree::ggtree(tree, layout = "circular") +
              theme(legend.position = "right", legend.key.size = unit(3, "lines")) +
              geom_tiplab(size = 3, offset = 0.5)
            
            genus_cover <- split(final_best_df$genus, final_best_df$pres)
            
            tree_family <- ggtree::groupOTU(tree_family, genus_cover, "species") + aes(color = species) +
              theme(
                legend.position = "right",
                legend.text = element_text(size = 14),
                legend.title = element_text(size = 14)
              ) +
              scale_color_manual(
                name = "Genus",
                values = c("0" = "orange", "1" = "darkgreen", "3" = "blue"),
                labels = c("Not available", "Available", "Priority"),
                breaks = c("0", "1", "3")
              ) +
              labs(title = paste("Tree of", family_test)) +
              theme(
                legend.title = element_text(size = 20),
                legend.text = element_text(size = 15)
              )
            
            print(tree_family)
            
            output$downloadFamilyPlot <- downloadHandler(
              filename = function() {
                paste0("Tree_plot_", family_test, ".pdf")
              },
              content = function(file) {
                ggsave(filename = file, plot = tree_family, device = "pdf", width = 40, height = 40, units = "cm")
              }
            )
          })
        })
        
        incProgress(6/6, detail = "Finalizing...")
      
      }
    } else {
      # Directly go to split by 'pres' for plot coloring
      # Split par pres pour la couleur dans le plot
      
      incProgress(1/3, detail = "Preparing the table...")
      
      output$mytable <- gt::render_gt({
        df_rangement_priority <- df_rangement %>% filter(pres == 3) %>% dplyr::select(genus)
        
        length_to_pad <- (3 - length(df_rangement_priority$genus) %% 3) %% 3
        padded_genus <- c(df_rangement_priority$genus, rep(NA, length_to_pad))
        
        matrix_genus <- matrix(padded_genus, ncol = 3, byrow = TRUE)
        df_table <- as.data.frame(matrix_genus)
        
        gt(df_table) %>%
          gt::tab_header(
            title = md("Genus to select")
          )
      })
      
      output$downloadTable <- downloadHandler(
        filename = function() {
          paste("Priority_", family_test, ".csv", sep = "")
        },
        content = function(file) {
          df_rangement_priority <- df_rangement %>% filter(pres == 3) %>% dplyr::select(genus)
          write.csv(df_rangement_priority, file, row.names = FALSE)
        }
      )
      
      incProgress(2/3, detail = "Preparing family tree...")
      
      output$FamilyPlot <- renderPlot({
        isolate({
          tree_family <- ggtree::ggtree(tree, layout = "circular") +
            theme(legend.position = "right", legend.key.size = unit(3, "lines")) +
            geom_tiplab(size = 3, offset = 0.5)
          
          genus_cover <- split(final_best_df$genus, final_best_df$pres)
          
          tree_family <- ggtree::groupOTU(tree_family, genus_cover, "species") + aes(color = species) +
            theme(
              legend.position = "right",
              legend.text = element_text(size = 14),
              legend.title = element_text(size = 14)
            ) +
            scale_color_manual(
              name = "Genus",
              values = c("0" = "orange", "1" = "darkgreen", "3" = "blue"),
              labels = c("Not available", "Available", "Priority"),
              breaks = c("0", "1", "3")
            ) +
            labs(title = paste("Tree of", family_test)) +
            theme(
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 15)
            )
          
          print(tree_family)
              incProgress(3/3, detail = "Finalizing...")

          output$downloadFamilyPlot <- downloadHandler(
            filename = function() {
              paste0("Tree_plot_", family_test, ".pdf")
            },
            content = function(file) {
              ggsave(filename = file, plot = tree_family, device = "pdf", width = 40, height = 40, units = "cm")
            }
          )
        })
      })
    
    }
    
  })
})


#####################################
######### PHYLO TREE GENUS ##########
#####################################
observeEvent(c(input$actiongenus, input$species_select), {
  withProgress(message = 'Loading data...', value = 0, {
    req(input$Garden != "")

    genus_test <-  input$genus
    output$onlyspecies <- renderDT({ NULL })
    output$mytable <- renderDT({ NULL })
    output$GenusPlot <- renderPlot({ NULL })
    output$textspecies <- renderText({ NULL })
    species_cover <- NULL
    species_select <- input$species_select
    input_code <- input$Garden
    cover_species_garden <- cover_species_garden_full

    # Nouvelle condition : si l’utilisateur demande plus d’espèces que disponibles
    if (species_select > sum(cover_species_garden$pres == 0)) {
      cover_species_garden$pres[cover_species_garden$pres == 0] <- 3
      final_best_df <- cover_species_garden
      goto_split <- TRUE
    } else {
      goto_split <- FALSE
    }

    if (!goto_split) {
      incProgress(1/6, detail = "Preparing data...")

      # Nettoyage des codes jardin
      if (length(input_code) == 1) {
        cover_species_garden$code_garden <- ifelse(
          !grepl(paste(input_code, collapse = "|"), cover_species_garden$code_garden),
          NA,
          cover_species_garden$code_garden
        )
        cover_species_garden$code_garden[!is.na(cover_species_garden$code_garden)] <- paste(input_code, collapse = "_")
      } else {
        selected_values <- paste(input_code, collapse = "|")
        cover_species_garden$code_garden[!grepl(selected_values, cover_species_garden$code_garden)] <- NA
        entier <- c("fr", "ne", "la", "ge", "ch", "lo", "pr")
        diff <- setdiff(entier, input_code)
        cover_species_garden$code_garden <- gsub(paste(diff, collapse = "|"), "", cover_species_garden$code_garden)
        cover_species_garden$code_garden <- gsub("_+", "_", cover_species_garden$code_garden)
        cover_species_garden$code_garden <- gsub("^_|_$", "", cover_species_garden$code_garden)
      }

      incProgress(2/6, detail = "Filtering data...")
      unique_species_count <- cover_species_garden_full %>%
        filter(genus == genus_test) %>%
        distinct(species) %>%
        nrow()

      if (unique_species_count == 1) {
        output$onlyspecies <- DT::renderDT({
          species_line <- subset(cover_species_garden_full, genus == input$genus)
          species_line <- species_line %>%
            dplyr::select(any_of(c("species", "genus", "family", "garden", "pres")))

          datatable(
            species_line,
            options = list(pageLength = 10, scrollX = TRUE),
            rownames = FALSE,
            class = "stripe hover compact"
          )
        })

        output$textspecies <- renderText({
          "Tree not available: only one species in this genus."
        })
        output$GenusPlot <- renderPlot({})
      } else {
        cover_species_garden <- cover_species_garden %>%
          filter(genus == genus_test)

        cover_species_garden$species <- gsub("['’].*$", "", cover_species_garden$species)
        cover_species_garden$species <- trimws(cover_species_garden$species)

         ott_species<- unique(cover_species_garden$species) 

          resolved_ott <- tnrs_match_names(ott_species)
          resolved_ott <- resolved_ott[!is.na(resolved_ott$ott_id),]

          cover_species_garden <- merge(
          cover_species_garden,
          resolved_ott,
          by.x = "species",      
          by.y = "unique_name",  
          all.x = TRUE           
          )

        cover_species_garden$pres[is.na(cover_species_garden$pres)] <- 0
        cover_species_garden <- cover_species_garden %>%
           dplyr::mutate(code_garden = na_if(code_garden, ""))

        cover_species_garden <- cover_species_garden[!is.na(cover_species_garden$ott_id), ]
        cover_species_garden$species <- gsub("^x ", "", cover_species_garden$species)
        cover_species_garden$pres[is.na(cover_species_garden$code_garden)] <- 0

        incProgress(3/6, detail = "Generating phylogenetic tree...")

        valid_ott_ids <- unique(na.omit(cover_species_garden$ott_id))

        if (length(valid_ott_ids) < 2) {
          showNotification("Not enough valid ott_ids to generate a phylogenetic tree.", type = "error")
          output$GenusPlot <- renderPlot({ NULL })
          output$mytable <- renderDT({ NULL })
          return(NULL)
        }

        max_ids <- 150
        if (length(valid_ott_ids) > max_ids) {
          valid_ott_ids <- valid_ott_ids[1:max_ids]
          showNotification(paste("Limitation to", max_ids, "ott_ids to avoid memory issues."), type = "warning")
        }

        tree <- rotl::tol_induced_subtree(ott_ids = valid_ott_ids)
        tree_tip_label <- tree$tip.label

        df_rangement <- data.frame(species = tree$tip.label)
        df_rangement$species <- gsub("_", " ", df_rangement$species)           
        df_rangement$species <- sub(" [^ ]+$", "", df_rangement$species)  
        df_rangement <- merge(df_rangement, cover_species_garden, by = "species", all.x = TRUE, sort = FALSE)

        best_diff <- Inf
        best_dfs <- list()
        before_values <- 0:10
        after_values <- 0:10
        step_values <- 1:5

        for (before in before_values) {
          for (after in after_values) {
            for (step in step_values) {
              df_temp <- df_rangement %>%
                dplyr::mutate(
                  reg_7day = slide_dbl(
                    pres,
                    .f = ~sum(.x, na.rm = TRUE),
                    .before = before,
                    .after = after,
                    .step = step
                  )
                )

              df_temp <- df_temp %>%
                 dplyr::mutate(
                  reg_7day = case_when(
                    is.na(reg_7day) & pres == 0 ~ 1,
                    is.na(reg_7day) & pres == 1 ~ 2,
                    TRUE ~ reg_7day
                  ),
                  pres = if_else(reg_7day == 0 & pres == 0, 3, pres)
                )

              count_3 <- sum(df_temp$pres == 3, na.rm = TRUE)
              if (is.na(count_3) || count_3 > species_select) next

              diff <- abs(count_3 - species_select)
              if (diff == 0) best_dfs[[length(best_dfs) + 1]] <- df_temp
              if (diff < best_diff) {
                best_diff <- diff
                best_df <- df_temp
              }
            }
          }
        }

        calculate_distance <- function(df) {
          indices <- which(df$pres == 3)
          if (length(indices) < 2) return(0)
          return(min(diff(indices)))
        }

        max_distance <- -Inf
        final_best_df <- NULL

        for (df in best_dfs) {
          distance <- calculate_distance(df)
          if (distance > max_distance) {
            max_distance <- distance
            final_best_df <- df
          }
        }

        if (length(best_dfs) == 0) {
          min_diff <- Inf
          for (before in before_values) {
            for (after in after_values) {
              for (step in step_values) {
                df_temp <- df_rangement %>%
                  dplyr::mutate(
                    reg_7day = slide_dbl(
                      pres,
                      .f = ~sum(.x, na.rm = TRUE),
                      .before = before,
                      .after = after,
                      .step = step
                    )
                  )

                df_temp <- df_temp %>%
                   dplyr::mutate(
                    reg_7day = case_when(
                      is.na(reg_7day) & pres == 0 ~ 1,
                      is.na(reg_7day) & pres == 1 ~ 2,
                      TRUE ~ reg_7day
                    ),
                    pres = if_else(reg_7day == 0 & pres == 0, 3, pres)
                  )

                count_3 <- sum(df_temp$pres == 3)
                if (is.na(count_3) || count_3 > species_select) next

                diff <- abs(count_3 - species_select)
                if (diff < min_diff) {
                  min_diff <- diff
                  best_df <- df_temp
                }
              }
            }
          }
          final_best_df <- df_temp
        }

        incProgress(4/6, detail = "Preparing the table...")

        output$mytable <- DT::renderDT({
          df_rangement_priority <- final_best_df %>% filter(pres == 3) %>% dplyr::select(species)

          length_to_pad <- (3 - length(df_rangement_priority$species) %% 3) %% 3
          padded_species <- c(df_rangement_priority$species, rep(NA, length_to_pad))

          matrix_species <- matrix(padded_species, ncol = 3, byrow = TRUE)
          df_table <- as.data.frame(matrix_species)

          datatable(
            df_table,
            options = list(pageLength = 10, scrollX = TRUE),
            rownames = FALSE,
            colnames = c("Species 1", "Species 2", "Species 3"),
            class = "stripe hover compact"
          )
        })

        output$downloadTable <- downloadHandler(
          filename = function() {
            paste("Priority_", genus_test, ".csv", sep = "")
          },
          content = function(file) {
            df_rangement_priority <- df_rangement %>% filter(pres == 3) %>% dplyr::select(species)
            write.csv(df_rangement_priority, file, row.names = FALSE)
          }
        )

        incProgress(5/6, detail = "Rendering genus tree...")

        output$GenusPlot <- renderPlot({
          isolate({
            tree_genus <- ggtree::ggtree(tree, layout = "circular") +
              theme(legend.position = "right", legend.key.size = unit(3, "lines")) +
              geom_tiplab(size = 3, offset = 0.5)

# 1. Créer une table de correspondance entre les noms simples et les labels complets
            match_table <- data.frame(
                 species_clean = tolower(gsub("_ott[0-9]+", "", gsub("_", " ", tree_tip_label))),
                 species_label = tree_tip_label
                  )
            match_table$species_clean <- sub(" ott[0-9]+", "", match_table$species_clean)

            final_best_df$species <- match_table$species_label[
  match(tolower(final_best_df$species), match_table$species_clean)
]

            species_cover <- split(final_best_df$species, final_best_df$pres)

            tree_genus <- ggtree::groupOTU(tree_genus, species_cover, "species") +
              aes(color = species) +
              scale_color_manual(
                name = "Species",
                values = c("0" = "orange", "1" = "darkgreen", "3" = "blue"),
                labels = c("Not available", "Available", "Priority"),
                breaks = c("0", "1", "3")
              ) +
              labs(title = paste("Tree of genus", genus_test)) +
              theme(
                legend.title = element_text(size = 20),
                legend.text = element_text(size = 15)
              )

            print(tree_genus)

            output$downloadGenusPlot <- downloadHandler(
              filename = function() {
                paste0("Tree_plot_", genus_test, ".pdf")
              },
              content = function(file) {
                ggsave(filename = file, plot = tree_genus, device = "pdf", width = 40, height = 40, units = "cm")
              }
            )
          })
        })

        incProgress(6/6, detail = "Finalizing...")
      }
    }
  })
})













#####################################
#########BARPLOT COVER ##############
#####################################
observeEvent(input$action, {
  req(input$Garden != "")
 
 withProgress(message ='Loading data...', value = 0, {

  input_values <-input$Garden

  cover_plot <- cover_species_garden_full %>%
    dplyr::select(species, genus, family, garden)

  valid_elements <- c("fr", "ne", "la", "ge","ch","pr","lo")
  
   incProgress(1/3, detail = "Functions...")

  filter_code <- function(code, input_values) {
    elements <- unlist(strsplit(code, "_"))
    filtered <- elements[elements %in% input_values]
    if (length(filtered) == 0) return("NA")
    paste(sort(filtered), collapse = "_")
  }

recompose_code <- function(codes) {
  elements <- unique(unlist(strsplit(codes, "_")))
  valid <- elements[elements %in% valid_elements]
  if (length(valid) == 0) return("NA")
  final_code <- paste(sort(valid), collapse = "_")
  return(final_code)
}

  create_cover_dataframe <- function(data, group_var) {
    unique_groups <- unique(data[[group_var]])
    cover <- lapply(unique_groups, function(g) {
      codes <- unique(data$garden[data[[group_var]] == g])
      final_code <- recompose_code(codes)
      data.frame(group = g, garden = final_code, stringsAsFactors = FALSE)
    })
    df <- do.call(rbind, cover)
    names(df)[1] <- group_var
    return(df)
  }

  add_padding <- function(tbl, total_expected) {
    total_current <- sum(tbl)
    to_add <- total_expected - total_current
    if ("NA" %in% names(tbl)) {
      tbl["NA"] <- tbl["NA"] + to_add
    } else {
      tbl <- c(tbl, "NA" = to_add)
    }
    return(tbl)
  }

  build_df <- function(tbl, type_label) {
    data.frame(
      type = type_label,
      av = ifelse(names(tbl) == "NA", "not available", "available"),
      garden = names(tbl),
      count = as.vector(tbl),
      stringsAsFactors = FALSE
    )
  }
 
 incProgress(2/3, detail = "Preparation data...") 
  # Traitement centralisé
  cover_plot$garden <- sapply(cover_plot$garden, filter_code, input_values = input_values)
  cover_plot <- dplyr::filter(cover_plot, garden != "NA")

  species_cover <- create_cover_dataframe(cover_plot, "species")
  genus_cover   <- create_cover_dataframe(cover_plot, "genus")
  family_cover  <- create_cover_dataframe(cover_plot, "family")

  species_table <- add_padding(table(species_cover$garden), 390000)
  genus_table   <- add_padding(table(genus_cover$garden),   14282)
  family_table  <- add_padding(table(family_cover$garden),  508)

  table_full <- rbind(
    build_df(family_table, "family"),
    build_df(genus_table, "genus"),
    build_df(species_table, "species")
  )

table_full$garden <- factor(table_full$garden, levels = union(family_levels, unique(table_full$garden)))
table_full$garden <- factor(table_full$garden, levels = names(color_values))

  incProgress(3/3, detail = "Finalizing...")
  # BARPLOT
  output$coverplot <- renderPlot({
    ggplot(table_full, aes(x = type, y = count, fill = garden)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(x = "Type", y = "Count", fill = "Garden") +
      ggtitle("Taxonomic Coverage per Garden Combination") +
      theme_minimal() +
      facet_wrap(~type, scales = "free") +
      scale_fill_manual(
        values = color_values,
        labels = replacement_mapping[names(color_values) %in% table_full$garden],
        breaks = names(color_values)[names(color_values) %in% table_full$garden]
      ) +
      theme(
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 13)
      )
  })

  # PIE CHART
  output$piechart <- renderPlot({
    ggplot(table_full, aes(x = "", y = count, fill = garden)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y") +
      facet_wrap(~type, scales = "free") +
      labs(title = "Taxonomic Coverage per Garden Combination",
           fill = "Garden") +
      scale_fill_manual(
        values = color_values,
        labels = replacement_mapping[names(color_values) %in% table_full$garden],
        breaks = names(color_values)[names(color_values) %in% table_full$garden]
      ) +
      theme_void() +
      theme(
        strip.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)
      )
  })

  # DOWNLOADS
  output$downloadcoverplot <- downloadHandler(
    filename = function() {
      paste0("Barplot_coverage_", Sys.Date(), ".jpg")
    },
    content = function(file) {
      ggsave(filename = file,
             plot = ggplot(table_full, aes(x = type, y = count, fill = garden)) +
                      geom_bar(stat = "identity", position = "stack") +
                      facet_wrap(~type, scales = "free"),
             device = "jpg", width = 14, height = 10)
    }
  )

  output$dlpiechart <- downloadHandler(
    filename = function() {
      paste0("PieChart_coverage_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file,
             plot = ggplot(table_full, aes(x = "", y = count, fill = garden)) +
                      geom_bar(stat = "identity", width = 1, color = "white") +
                      coord_polar("y") +
                      facet_wrap(~type, scales = "free"),
             width = 10, height = 8)
    }
  )
})
 })



 

#####################################
######### VENNPLOT ##############
#####################################

observeEvent(input$action, {
  req(input$Garden != "")

  cover_plot <- cover_species_garden_full
  input_code <- input$Garden

  # Nettoyage des NA
  cover_plot <- cover_plot[!is.na(cover_plot$species) & 
                             !is.na(cover_plot$garden) & 
                             !is.na(cover_plot$genus) & 
                             !is.na(cover_plot$family), ]

  # Filtrer selon les jardins sélectionnés
  filtered_data <- cover_plot[cover_plot$garden %in% input_code, ]

  # Création des listes
  list_of_species <- lapply(input_code, function(g) filtered_data$species[filtered_data$garden == g])
  list_of_genus   <- lapply(input_code, function(g) filtered_data$genus[filtered_data$garden == g])
  list_of_family  <- lapply(input_code, function(g) filtered_data$family[filtered_data$garden == g])

  names(list_of_species) <- input_code
  names(list_of_genus)   <- input_code
  names(list_of_family)  <- input_code

  # Labels + couleurs
  labels_to_use <- replacement_mapping[input_code]
  colors_to_use <- color_values[input_code]

  display_venn <- function(x, labels, colors, title) {  old_wd <- getwd()
  tmp_dir <- tempdir()
  setwd(tmp_dir)

  plot_grob <- grid.grabExpr({
    grid.newpage()
    pushViewport(viewport(width = 0.5, height = 0.5))
    venn_obj <- venn.diagram(
      x,
      filename = NULL,
      category.names = labels,
      fill = unname(colors),
      lwd = 1,
      lty = "blank",
      cex = 0.8,
      fontface = "italic",
      cat.cex = 0,
      cat.default.pos = "outer",
      cat.dist = rep(0.05, length(labels))
    )
    grid.draw(venn_obj)
    grid.text(title, x = 0.3, y = 1.2, gp = gpar(fontsize = 12, fontface = "bold"))  # titre plus bas    
    popViewport()
  })

  setwd(old_wd)
  return(plot_grob)
}
  # Générer les Venn diagrams

  venn_species <- display_venn(list_of_species, labels_to_use, colors_to_use, "Species")
  venn_genus   <- display_venn(list_of_genus,   labels_to_use, colors_to_use, "Genus")
  venn_family  <- display_venn(list_of_family,  labels_to_use, colors_to_use, "Family")

  # Légende
  legend_plot <- grid.grabExpr({
    grid.newpage()
    legend_grob <- legendGrob(labels_to_use, pch = 15, gp = gpar(col = colors_to_use, fontsize = 12, fontface = "bold"))
    grid.draw(legend_grob)
  })

  # Affichage dans l'app Shiny avec légende à droite
output$vennplot <- renderPlot({
  plots_col1 <- arrangeGrob(venn_species, venn_genus, venn_family, ncol = 1, heights = c(1,1,1))
  final_plot <- arrangeGrob(plots_col1, legend_plot, ncol = 2, widths = c(3, 1))
  grid.draw(final_plot)
})


  # Téléchargement
  output$dlvenplot <- downloadHandler(
    filename = function() {
      paste0("Venn_plot_", Sys.Date(), ".jpg")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "jpg", width = 10, height = 8)
    }
  )
})



#####################################
#########WHITAKKER GARDEN ##############
#####################################
data_clim_reactive <- reactiveVal(NULL)
observeEvent(input$action, {
  withProgress(message = 'Processing...', value = 0, {
    
    output$whitplot  <- NULL
    req(input$Garden != "")
    input_code <-input$Garden
    cover_whit <- gift_fusion


if(length(input_code) == 1) {
  cover_whit <- cover_whit %>% filter(grepl(input_code, code_garden))
  cover_whit$code_garden <- input_code
} else {
  cover_whit <- cover_whit %>% filter(sapply(code_garden, function(x) any(strsplit(x, "_")[[1]] %in% input_code)))
  
  # Rebuild code_garden as concatenation of selected gardens per species
  cover_whit$code_garden <- sapply(cover_whit$code_garden, function(x) {
    gardens <- strsplit(x, "_")[[1]]
    paste(sort(intersect(gardens, input_code)), collapse = "_")
  })
}

cover_whit <- cover_whit %>% distinct(species, .keep_all = TRUE)


    data_clim_reactive(cover_whit)

    output$whitplot <- renderPlot({
      isolate({
        whit <- plotbiomes::whittaker_base_plot() +
          geom_point(data = cover_whit, 
                     aes(x = temperature, 
                         y = precipitation,
                         color = code_garden),  
                     size = 1.5,             
                     shape = 16,
                     alpha = 0.8) +
          scale_color_manual(
            name = "Garden",
            values = color_values,
            labels = labels,
            breaks = family_levels
          ) +
          theme_minimal() +
                theme(legend.title = element_text(size = 14),
      legend.text = element_text(size = 12))+
          theme(panel.background = element_rect(fill = "white", color = NA)) +
          guides(color = guide_legend(override.aes = list(size = 5)))

        print(whit)

        output$dlwhitplot <- downloadHandler(
          filename = function() {
            paste0("whit_full_plot_", Sys.Date(), ".jpg")
          },
          content = function(file) {
            ggsave(filename = file, plot = whit, device = "jpg", width = 14, height = 10)
          }
        )
      })
    })
  })






#####################################
#########WHITAKKER FAMILY ##############
#####################################


output$whitplotFamily <- renderPlot({
isolate({

family_test <- input$family
cover_whit <- data_clim_reactive()
data_clim_sub <- subset(cover_whit, family == family_test)


data_clim_sub <- data_clim_sub %>%
  filter(!is.na(temperature) & !is.na(precipitation) & 
         is.finite(temperature) & is.finite(precipitation))


whitfamily <- plotbiomes::whittaker_base_plot() +
          geom_point(data = data_clim_sub, 
                     aes(x = temperature, 
                         y = precipitation,
                         color = code_garden),  
                     size = 2,             
                     shape = 16,
                     alpha = 0.8) +
          scale_color_manual(
            name = "Garden",
            values = color_values,  # Ensure color_values is defined
            labels = labels,        # Ensure labels is defined
            breaks = family_levels  # Ensure family_levels is defined
          ) +
          theme_minimal() +
          theme(panel.background = element_rect(fill = "white", color = NA)) +
          theme(legend.title = element_text(size = 14),
      legend.text = element_text(size = 12))+
          guides(color = guide_legend(override.aes = list(size = 5)))

        print(whitfamily)


 output$dlwhitplotFamily <- downloadHandler(
          filename = function() {
            paste0("whit_plot_", family_test, ".jpg")
          },
          content = function(file) {
            ggsave(filename = file, plot = whitfamily, device = "jpg", width = 14, height = 10)
          }
        )
      })
      })


output$whitplotFamilyKernel <- renderPlot({
isolate({

family_test <- input$family
data_clim_sub <- subset(cover_whit, family == family_test)

data_clim_sub <- data_clim_sub %>%
  filter(!is.na(temperature) & !is.na(precipitation) & 
         is.finite(temperature) & is.finite(precipitation))


whitfamilyKernel <- plotbiomes::whittaker_base_plot() +
          geom_point(data = data_clim_sub, 
                     aes(x = temperature, 
                         y = precipitation,
                         color = code_garden),  
                     size = 1,             
                     shape = 16,
                     alpha = 0.8) +
          stat_density_2d(data = data_clim_sub, 
                          aes(x = temperature, 
                              y = precipitation, 
                              color = code_garden),
                          linewidth = 0.5,  
                          alpha = 0.3, 
                          h = 10) +  
          scale_color_manual(
            name = "Garden",
            values = color_values,  # Ensure color_values is defined
            labels = labels,        # Ensure labels is defined
            breaks = family_levels  # Ensure family_levels is defined
          ) +
          labs(title = paste("Whittaker Plot for", family_test)) +
          theme_minimal() +
          theme(panel.background = element_rect(fill = "white", color = NA)) +
          guides(color = guide_legend(override.aes = list(size = 5)))

        print(whitfamilyKernel)


 output$dlwhitplotFamilyKernel <- downloadHandler(
          filename = function() {
            paste0("whit_plot_Kernel_", family_test, ".jpg")
          },
          content = function(file) {
            ggsave(filename = file, plot = whitfamilyKernel, device = "jpg", width = 14, height = 10)
          }
        )
      })
      })




output$whitplotSelect <- renderPlotly({

  isolate({

    cover_whit <- data_clim_reactive()
    family_test <- input$family

    data_clim_sub <- cover_whit %>%
      dplyr::filter(family == family_test) %>%
      dplyr::filter(
        !is.na(temperature),
        !is.na(precipitation),
        is.finite(temperature),
        is.finite(precipitation)
      )

    req(nrow(data_clim_sub) > 0)

    data_clim_sub <- data_clim_sub %>%
      dplyr::mutate(
        garden = dplyr::recode(garden, !!!replacement_mapping),
        code_garden = as.character(code_garden)
      )

    # ✅ FIX PLOTLY
    valid_levels <- intersect(
      family_levels,
      unique(data_clim_sub$code_garden)
    )

    plot <- ggplot(
      data_clim_sub,
      aes(
        x = temperature,
        y = precipitation,
        color = code_garden,
        text = paste(
          "Species:",
          ifelse(is.na(species), "unknown", species)
        )
      )
    ) +
      geom_point(size = 1, alpha = 0.8) +
      theme_minimal() +
      scale_color_manual(
        name = "Garden",
        values = color_values[valid_levels],
        labels = labels[valid_levels],
        breaks = valid_levels,
        drop = TRUE
      ) +
      coord_cartesian(
        xlim = c(-15, 30),
        ylim = c(-5, 450)
      )

    ggplotly(plot, tooltip = "text")
  })
})


})


}


shinyApp(ui = ui, server = server)



#####################################
#########SPECIES MAP ##############
#####################################
# Filtrer les données en fonction du jardin sélectionné

observe({
  updateSelectInput(session, "GPS_family", choices = sort(unique(all_species_taxo$family)))
})

filtered_data <- reactive({
  req(input$GPS_family)  
  all_species_taxo %>%
    filter(family %in% input$GPS_family)
})

# Mettre à jour les choix de genre en fonction de la famille sélectionnée
observeEvent(input$GPS_family, {
  req(filtered_data())
  updateSelectInput(session, "GPS_genus", choices = unique(filtered_data() %>% filter(family == input$GPS_family) %>% pull(genus)))
})

# Mettre à jour les choix d'espèce en fonction du genre sélectionné
observeEvent(input$GPS_genus, {
  req(filtered_data())
  updateSelectInput(session, "GPS_species", choices = unique(filtered_data() %>% filter(family == input$GPS_family & genus == input$GPS_genus) %>% pull(species)))
})

# Initialiser reactiveVal selected_species
  selected_species <- reactiveVal(character(0))
  
  # Ajouter espèces à la sélection
  observeEvent(input$addSpecies, {
    req(input$GPS_species)
    new_selection <- unique(c(selected_species(), input$GPS_species))
    selected_species(new_selection)
  })
  
  # Afficher la liste des espèces sélectionnées
  output$selected_species_ui <- renderUI({
    species <- selected_species()
    if (length(species) == 0) {
      tags$p("No species selected yet.")
    } else {
      tags$ul(
        lapply(species, function(sp) tags$li(sp))
      )
    }
  })
  
  # Bouton pour nettoyer la sélection
  observeEvent(input$clearSelection, {
    selected_species(character(0))
  })
  
  # Modifier l’observeEvent du bouton Go pour utiliser selected_species()
  observeEvent(input$goButton, {
    req(filtered_data())
  
    family_map <- filtered_data()
  
    species_selected <- selected_species()
    if (!is.null(species_selected) && length(species_selected) > 0) {
      family_map <- family_map %>% filter(species %in% species_selected)
    }

  # Initialiser un dataframe vide pour stocker les données GPS
  all_gps_data <- data.frame()

  # Afficher la barre de chargement
  withProgress(message = 'downloading data...(This might take a minute, please be patient.)', value = 0, {
    n <- nrow(family_map)
    # Boucle pour chaque espèce
    for (i in 1:n) {
      tryCatch({
        species_name <- family_map$species[i]
        # Recherche des données sur iNaturalist
        especetest <- rinat::get_inat_obs(query = species_name, maxresults = 100)
        selected_columns <- c("longitude", "latitude", "quality_grade", "captive_cultivated")
        data_inat <- especetest[selected_columns]
        data_inat <- data_inat %>%
          dplyr::filter(quality_grade == "research" & captive_cultivated == "false") %>%
          dplyr::select(longitude, latitude)

        # Recherche des données sur GBIF
        gbif_data <- rgbif::occ_data(scientificName = species_name, hasCoordinate = TRUE, limit = 100)

        # Vérifier si les données de GBIF existent
        if (!is.null(gbif_data$data)) {
          data_gbif <- gbif_data$data
          coordinates(data_gbif) <- c("decimalLongitude", "decimalLatitude")
          proj4string(data_gbif) <- sp::CRS("+proj=longlat +datum=WGS84")
          data_gbif_wgs84 <- sp::spTransform(data_gbif, CRS("+init=epsg:4326"))
          longitude <- sp::coordinates(data_gbif_wgs84)[, 1]
          latitude <- sp::coordinates(data_gbif_wgs84)[, 2]
          df_with_long_lat <- data.frame(longitude = longitude, latitude = latitude)
          data_gbif_selected <- df_with_long_lat 
          data_gbif_selected <- data_gbif_selected[!duplicated(data_gbif_selected[c("longitude", "latitude")]), ]
        } else {
          # Si les données de GBIF sont vides, créer un dataframe vide
          data_gbif_selected <- data.frame(longitude = numeric(0), latitude = numeric(0))
        }

        # Étape 3: Si les deux jeux de données sont non vides, fusionner les données et effectuer les étapes restantes
        if (is.data.frame(data_inat) && nrow(data_inat) > 0 && 
            is.data.frame(data_gbif_selected) && nrow(data_gbif_selected) > 0) {
          
          # Fusionner les données de localisation GBIF et iNaturalist
          data_inat$Source <- "iNaturalist"
          data_gbif_selected$Source <- "GBIF"
          
          # Ajout de la colonne species_gps dans data_gbif_selected
          data_gbif_selected$species_gps <- species_name 
          # Ajout de la colonne species_gps dans data_inat
          data_inat$species_gps <- species_name
          
          # Fusionner les données
          data_gps <- rbind(data_inat, data_gbif_selected)
          
          # Ajouter au dataframe global
          all_gps_data <- rbind(all_gps_data, data_gps)
        }
        
        # Étape 1: Si data_inat est vide, ajouter uniquement les données de data_gbif_selected à all_gps_data
        if (is.data.frame(data_inat) && nrow(data_inat) == 0) {
          data_gbif_selected$Source <- "GBIF"
          data_gbif_selected$species_gps <- species_name
          
          all_gps_data <- rbind(all_gps_data, data_gbif_selected)
        }
        
        # Étape 2: Si data_gbif_selected est vide, ajouter uniquement les données de data_inat à all_gps_data
        if (is.data.frame(data_gbif_selected) && nrow(data_gbif_selected) == 0) {
          data_inat$Source <- "iNaturalist"
          data_inat$species_gps <- species_name
          
          all_gps_data <- rbind(all_gps_data, data_inat)
        }
        
        # Mettre à jour la barre de progression
        incProgress(1/n+1, detail = paste("Traitement des données", i, "sur", n))
      }, error = function(e) {
        if (grepl("replacement has 1 row, data has 0", e$message)) {
          output$errortext <- renderText({
            paste("Species", species_name, "don't have any data")
          })
        }
      })
    }
  })



 
output$map <- renderLeaflet({
# Créer une palette de couleurs pour les espèces
    species_palette <- colorFactor(palette = "viridis", domain = all_gps_data$species_gps)
  
    leaflet::leaflet(all_gps_data) %>%
      leaflet::addTiles() %>%
      leaflet::addCircleMarkers(
        ~longitude, ~latitude,
        color = ~species_palette(species_gps),
        fillOpacity = 0.7,
        radius = 5,
        stroke = FALSE
      ) %>%
      leaflet::addLegend(
        "bottomright",
        pal = species_palette,
        values = ~species_gps,
        title = "Species",
        opacity = 1
      )
  })


output$mapsSimple <- renderPlot({
  # Create a color palette for species using viridis
  species_palette <- scale_color_viridis_d()

  ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "lightgray", color = "white") +
    geom_point(data = all_gps_data, aes(x = longitude, y = latitude, color = species_gps), size = 1.5) +
    species_palette +
    labs(x = "Longitude", y = "Latitude", color = "Species", shape = "Garden") +
    theme_minimal() +
    theme(legend.position = "right") +
    coord_fixed(ratio = 1.2, xlim = c(min(world$long) - 20, max(world$long) + 20), ylim = c(min(world$lat) - 10, max(world$lat) + 10))
})

  # Fonction pour télécharger la carte
output$downloaddistrib <- downloadHandler(
  filename = function() {
    paste("distribution_map", Sys.Date(), ".jpg", sep = "")
  },
  content = function(file) {

  species_palette <- scale_color_viridis_d()

  carte <- ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "lightgray", color = "white") +
    geom_point(data = all_gps_data, aes(x = longitude, y = latitude, color = species_gps), size = 3) +
    species_palette +
    labs(x = "Longitude", y = "Latitude", color = "Species", shape = "Garden") +
    theme_minimal() +
    theme(legend.position = "right") + 
    theme(legend.title = element_text(size = 30), 
    legend.text = element_text(size = 25),
    legend.key.size = unit(2, "cm")) +
    coord_fixed(ratio = 1.2, xlim = c(min(world$long) - 20, max(world$long) + 20), ylim = c(min(world$lat) - 10, max(world$lat) + 10))
    
    ggsave(file, plot = carte, device = "jpg", width = 40, height = 30, units = "in",limitsize = FALSE)
  }
)

})



  

#####################################
#########SPECIES SELECT ##############
#####################################

cover_species <- cover_species_garden_full
all_species <- all_species_taxo

cover_species <- as.data.frame(lapply(cover_species, function(x) iconv(x, from = "", to = "UTF-8", sub = "")))
all_species <- as.data.frame(lapply(all_species, function(x) iconv(x, from = "", to = "UTF-8", sub = "")))

# Remplacer les valeurs dans garden en utilisant le vecteur de correspondances
cover_species <- cover_species %>%
  dplyr::mutate(garden = replacement_mapping[garden])

 # Étape 1 : Identifier les doublons par 'species' et 'garden' et compter les occurrences
  cover_species_summary <- cover_species %>%
   dplyr::group_by(species, garden) %>%
   dplyr::summarize(pres = n(), .groups = 'drop')

  # Étape 2 : Supprimer les doublons en gardant un seul exemplaire avec le compte des occurrences
  cover_species <- cover_species %>%
   dplyr::distinct(species, garden, .keep_all = TRUE) %>%
   dplyr::left_join(cover_species_summary, by = c("species", "garden"))

cover_species <- cover_species %>%
  rename(pres = pres.x) %>%  
  dplyr::select(-pres.y)  

   cover_species <- cover_species %>%
   dplyr::select(species, genus, family, garden, pres)

all_species$garden <- "NA"
all_species$pres <-"0"

cover_species <- rbind(all_species,cover_species)

cover_genus <- cover_genus_garden_full
   cover_genus <- cover_genus[cover_genus$pres == 0, c("family", "genus", "garden", "pres")]
   cover_genus <- cbind(species = NA, cover_genus)

   cover_species <- rbind(cover_species,cover_genus)

   cover_species <- rename(cover_species, `individual available` = pres)


  observe({
    updateSelectInput(session, "selected_family", choices = unique(cover_species$family))
  })
  
  observe({
    if (!is.null(input$selected_family) && input$selected_family != "") {
      updateSelectInput(session, "selected_genus", choices = c("", unique(cover_species$genus[cover_species$family == input$selected_family])))
    } else {
      updateSelectInput(session, "selected_genus", choices = c("", NULL))
    }
  })
  
  observe({
    if (!is.null(input$selected_family) && input$selected_family != "" && !is.null(input$selected_genus) && input$selected_genus != "") {
      updateSelectInput(session, "selected_species", choices = c("", unique(cover_species$species[cover_species$family == input$selected_family & cover_species$genus == input$selected_genus])))
    } else {
      updateSelectInput(session, "selected_species", choices = c("", NULL))
    }
  })
  
  select_species <- reactive({
    filtered_species <- cover_species
    if (!is.null(input$selected_family) && input$selected_family != "") {
      filtered_species <- filtered_species %>%
        filter(family == input$selected_family)
    }
    if (!is.null(input$selected_genus) && input$selected_genus != "") {
      filtered_species <- filtered_species %>%
        filter(genus == input$selected_genus)
    }
    if (!is.null(input$selected_species) && input$selected_species != "") {
      filtered_species <- filtered_species %>%
        filter(species == input$selected_species)
    }
    return(filtered_species)
  })
  
  output$selectedData <- renderTable({
    select_species()
  })


output$downloadTablespecies <- downloadHandler(
    filename = function() {
        paste("species_garden", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
        write.csv(select_species(), file, row.names = FALSE)
    }
  )




  ###################################################
  ###################### DBGI #######################
  ###################################################

  
  # Chargement API + données de base
  data <- reactive({
    res <- GET("https://emi-collection.unifr.ch/directus/items/Field_Data?limit=10000")
    if (status_code(res) == 200) {
      data_raw <- content(res, as = "text", encoding = "UTF-8")
      data_json <- fromJSON(data_raw)
      df <- data_json$data
      return(df)
    } else {
      showNotification(paste("Erreur lors de la récupération :", status_code(res)), type = "error")
      return(NULL)
    }

  })
  list_fr <- reactive({
    read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/botanical_garden_list/list_fribourg.csv"), sep = ",") %>%
      dplyr::select(ipen, secteur, idTaxon, matched_name) %>%
       dplyr::mutate(idTaxon = sapply(strsplit(trimws(idTaxon), "\\s+"), function(x) paste(head(x, 2), collapse = " ")))
  })
  
  list_neu <- reactive({
    neu_cult2024 <- read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/botanical_garden_list/list_neu_2024.csv"), sep = ";") 
    neu_cult2023 <-read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/botanical_garden_list/list_neu_2023.csv"), sep = ";") 
    neu_cult2023$years <- 2023
    neu_cult2024$years <- 2024
    
    df <- bind_rows(neu_cult2023, neu_cult2024)
    colnames(df) <- gsub("\\.", " ", colnames(df))
    colnames(df) <- gsub(" ", "_", colnames(df))
    colnames(df) <- tolower(colnames(df))
    colnames(df) <- gsub("[éè]", "e", colnames(df))
    colnames(df) <- gsub("[^a-z0-9_]", "", iconv(colnames(df), "latin1", "ASCII", sub=""))
    
    df$numero_de_specimen_cultive<- substr(df$numero_de_specimen_cultive, 1, 8)
    
   df$groupe <- iconv(df$groupe, from = "", to = "UTF-8", sub = "byte")
   df$sous_groupe <- iconv(df$sous_groupe, from = "", to = "UTF-8", sub = "byte")
   df$genre <- iconv(df$genre, from = "", to = "UTF-8", sub = "byte")
   df$espece <- iconv(df$espece, from = "", to = "UTF-8", sub = "byte")

    df <- df %>%
      dplyr::select(code_ipen, famille, genre, espece, groupe, sous_groupe, years) %>%
       dplyr::mutate(
        species = tolower(paste(genre, espece)),
        groupe = tolower(groupe) %>% gsub(" ", "_", .) %>% gsub("'", "", .) %>% stri_trans_general("Latin-ASCII"),
        sous_groupe = tolower(sous_groupe) %>% gsub(" ", "_", .) %>% gsub("'", "", .) %>% stri_trans_general("Latin-ASCII")
      )
    
    df$sous_groupe[is.na(df$sous_groupe)] <- df$groupe[is.na(df$sous_groupe)]
    
    df <- df[!duplicated(df$code_ipen), ]
    
  })
  
list_ch <- reactive({
  read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/botanical_garden_list/list_champex.csv"), sep = ";") %>%
    dplyr::select(Famille, Genre_nouveau, Sps_nouveau) %>%
    rename(Familly = Famille, Genus = Genre_nouveau, Species = Sps_nouveau) 
   
})

list_lo <- reactive({
list_kew_PoW <-  read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/botanical_garden_list/kew_PoW_list.csv"), sep = ";")
list_kew_RG <-  read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/botanical_garden_list/kew_rock_garden_list.csv"), sep = ",")
list_kew_TH <-  read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/botanical_garden_list/kew_temperate_house_list.csv"), sep = ";") 

  list_london <- bind_rows(list_kew_TH, list_kew_PoW, list_kew_RG)
  
  list_london <- list_london %>%
    dplyr::select(Famille, Genre_nouveau, Sps_nouveau) %>%
    rename(
      Family = Famille,
      Genus = Genre_nouveau,
      Species = Sps_nouveau
    )
  
  return(list_london)
})



  # jbuf_merged
  jbuf_merged <- reactive({
    req(data())
    d <- data()
    filtered <- d %>% filter(grepl("dbgi", sample_id, ignore.case = TRUE))
    filtered$taxon_name <- ifelse(is.na(filtered$taxon_name), "", filtered$taxon_name)
    filtered$sample_name <- ifelse(is.na(filtered$sample_name), "", filtered$sample_name)
    filtered$taxon_name <- paste(filtered$taxon_name, filtered$sample_name)
    filtered$taxon_name <- sapply(strsplit(trimws(filtered$taxon_name), "\\s+"), function(x) paste(head(x, 2), collapse = " "))
    
    jbuf <- filtered[filtered$qfield_project == "jbuf", ]
    jbuf <- jbuf[, c("taxon_name", "sample_id", "x_coord", "y_coord", "qfield_project")]
    jbuf <- jbuf[!is.na(jbuf$taxon_name) & jbuf$taxon_name != "", ]
    
    jbuf$taxon_name <- tolower(jbuf$taxon_name)
    jbuf$taxon_name <- gsub("[^a-z0-9 ]", "", jbuf$taxon_name)
    jbuf$taxon_name <- trimws(jbuf$taxon_name)
    
    fr <- list_fr()
    fr$idTaxon <- tolower(fr$idTaxon)
    fr$idTaxon <- gsub("[^a-z0-9 ]", "", fr$idTaxon)
    fr$idTaxon <- trimws(fr$idTaxon)
    
    merged <- merge(jbuf, fr, by.x = "taxon_name", by.y = "idTaxon", all.x = TRUE)
    merged <- merged[!duplicated(merged$sample_id), ]
    merged
  })
  
  # jbn_merged
  jbn_merged <- reactive({
    req(data())
    d <- data()
    filtered <- d %>% filter(grepl("dbgi", sample_id, ignore.case = TRUE))
    filtered$taxon_name <- ifelse(is.na(filtered$taxon_name), "", filtered$taxon_name)
    filtered$sample_name <- ifelse(is.na(filtered$sample_name), "", filtered$sample_name)
    filtered$taxon_name <- paste(filtered$taxon_name, filtered$sample_name)
    filtered$taxon_name <- sapply(strsplit(trimws(filtered$taxon_name), "\\s+"), function(x) paste(head(x, 2), collapse = " "))
    
    jbn <- filtered[filtered$qfield_project == "jbn", ]
    jbn <- jbn[, c("taxon_name", "sample_id", "x_coord", "y_coord", "qfield_project")]
    jbn <- jbn[!is.na(jbn$taxon_name) & jbn$taxon_name != "", ]
    
    jbn$taxon_name <- tolower(jbn$taxon_name)
    jbn$taxon_name <- gsub("_", " ", jbn$taxon_name)
    jbn$taxon_name <- trimws(jbn$taxon_name)
    jbn$taxon_name <- sapply(strsplit(jbn$taxon_name, "\\s+"), function(x) paste(head(x, 2), collapse = " "))
    jbn$taxon_name <- gsub("[^a-z0-9 ]", "", jbn$taxon_name)
    jbn$taxon_name <- trimws(jbn$taxon_name)
    
    neu <- list_neu()
    
    merged <- merge(jbn, neu, by.x = "taxon_name", by.y = "species", all.x = TRUE)
    merged
  })
  

  # jbc_merged
 jbc_merged <- reactive({
    req(data())
    d <- data()
    filtered <- d %>% filter(grepl("dbgi", sample_id, ignore.case = TRUE))
    filtered$taxon_name <- ifelse(is.na(filtered$taxon_name), "", filtered$taxon_name)
    filtered$sample_name <- ifelse(is.na(filtered$sample_name), "", filtered$sample_name)
    filtered$taxon_name <- paste(filtered$taxon_name, filtered$sample_name)
    filtered$taxon_name <- sapply(strsplit(trimws(filtered$taxon_name), "\\s+"), function(x) paste(head(x, 2), collapse = " "))
    
    jbc <- filtered[filtered$qfield_project == "jbc", ]
    jbc <- jbc[, c("taxon_name", "sample_id", "x_coord", "y_coord", "qfield_project")]
    jbc <- jbc[!is.na(jbc$taxon_name) & jbc$taxon_name != "", ]
    
    jbc$taxon_name <- tolower(jbc$taxon_name)
    jbc$taxon_name <- gsub("[^a-z0-9 ]", "", jbc$taxon_name)
    jbc$taxon_name <- trimws(jbc$taxon_name)
    
    ch <- list_ch()
    ch$idTaxon <- paste(ch$Genus, ch$Species, sep = " ")
    ch$idTaxon <- iconv(ch$idTaxon, from = "latin1", to = "UTF-8", sub = "")
    ch$idTaxon <- tolower(ch$idTaxon)
    ch$idTaxon <- tolower(ch$idTaxon)
    ch$idTaxon <- gsub("[^a-z0-9 ]", "", ch$idTaxon)
    ch$idTaxon <- trimws(ch$idTaxon)
    
    merged <- merge(jbc, ch, by.x = "taxon_name", by.y = "idTaxon", all.x = TRUE)
    merged <- merged[!duplicated(merged$sample_id), ]
    merged
  })




  # jbuf_sf pour leaflet
 jbuf_sf <- reactive({
  df <- jbuf_merged()
  df <- df[!is.na(df$x_coord) & !is.na(df$y_coord) & df$x_coord != "" & df$y_coord != "", ]
  
  # Application des filtres textuels
  if (nzchar(input$filter_sample_id)) {
    df <- df[grepl(input$filter_sample_id, df$sample_id, ignore.case = TRUE), ]
  }
  if (nzchar(input$filter_taxon_name)) {
    df <- df[grepl(input$filter_taxon_name, df$taxon_name, ignore.case = TRUE), ]
  }
  
  sf <- st_as_sf(df, coords = c("x_coord", "y_coord"), crs = 2056, remove = FALSE)
  st_transform(sf, crs = 4326)
})
  
  # jbn_sf pour leaflet
jbn_sf <- reactive({
  df <- jbn_merged()
  df <- df[!is.na(df$x_coord) & !is.na(df$y_coord) & df$x_coord != "" & df$y_coord != "", ]
  
  if (nzchar(input$filter_sample_id)) {
    df <- df[grepl(input$filter_sample_id, df$sample_id, ignore.case = TRUE), ]
  }
  if (nzchar(input$filter_taxon_name)) {
    df <- df[grepl(input$filter_taxon_name, df$taxon_name, ignore.case = TRUE), ]
  }
  
  sf <- st_as_sf(df, coords = c("x_coord", "y_coord"), crs = 2056, remove = FALSE)
  st_transform(sf, crs = 4326)
})

    # jbn_sf pour leaflet
jbc_sf <- reactive({
  df <- jbc_merged()
  df <- df[!is.na(df$x_coord) & !is.na(df$y_coord) & df$x_coord != "" & df$y_coord != "", ]
  
  if (nzchar(input$filter_sample_id)) {
    df <- df[grepl(input$filter_sample_id, df$sample_id, ignore.case = TRUE), ]
  }
  if (nzchar(input$filter_taxon_name)) {
    df <- df[grepl(input$filter_taxon_name, df$taxon_name, ignore.case = TRUE), ]
  }
  
  sf <- st_as_sf(df, coords = c("x_coord", "y_coord"), crs = 2056, remove = FALSE)
  st_transform(sf, crs = 4326)
})
  
  # Render DataTables
 output$table_jbuf <- renderDT({
  df <- jbuf_merged()
  
  if (nzchar(input$filter_sample_id)) {
    df <- df[grepl(input$filter_sample_id, df$sample_id, ignore.case = TRUE), ]
  }
  if (nzchar(input$filter_taxon_name)) {
    df <- df[grepl(input$filter_taxon_name, df$taxon_name, ignore.case = TRUE), ]
  }
  
  datatable(df, options = list(pageLength = 10, scrollX = TRUE))
})

output$table_jbn <- renderDT({
  df <- jbn_merged()
  
  if (nzchar(input$filter_sample_id)) {
    df <- df[grepl(input$filter_sample_id, df$sample_id, ignore.case = TRUE), ]
  }
  if (nzchar(input$filter_taxon_name)) {
    df <- df[grepl(input$filter_taxon_name, df$taxon_name, ignore.case = TRUE), ]
  }
  
  datatable(df, options = list(pageLength = 10, scrollX = TRUE))
})

output$table_jbc <- renderDT({
  df <- jbc_merged()
  
  if (nzchar(input$filter_sample_id)) {
    df <- df[grepl(input$filter_sample_id, df$sample_id, ignore.case = TRUE), ]
  }
  if (nzchar(input$filter_taxon_name)) {
    df <- df[grepl(input$filter_taxon_name, df$taxon_name, ignore.case = TRUE), ]
  }
  
  datatable(df, options = list(pageLength = 10, scrollX = TRUE))
})
  

 # Handlers de téléchargement
  output$download_jbuf <- downloadHandler(
    filename = function() {
      paste("jbuf_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      df <- jbuf_merged() # Remplacez par votre fonction pour obtenir les données
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  output$download_jbn <- downloadHandler(
    filename = function() {
      paste("jbn_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      df <- jbn_merged() # Remplacez par votre fonction pour obtenir les données
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  output$download_jbc <- downloadHandler(
    filename = function() {
      paste("jbc_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      df <- jbc_merged() # Remplacez par votre fonction pour obtenir les données
      write.csv(df, file, row.names = FALSE)
    }
  )


  # Render Leaflet maps
  output$leaflet_jbuf <- renderLeaflet({
    sf <- jbuf_sf()
    req(sf)
    leaflet(sf) %>%
      addTiles() %>%
      addCircleMarkers(
        radius = 4,
        color = "blue",
        stroke = FALSE,
        fillOpacity = 0.7,
        popup = ~paste0("<b>Sample ID:</b> ", sample_id, "<br>",
                        "<b>Taxon:</b> ", taxon_name)
      ) %>%
      addScaleBar(position = "bottomleft")
  })
  
  output$leaflet_jbn <- renderLeaflet({
    sf <- jbn_sf()
    req(sf)
    leaflet(sf) %>%
      addTiles() %>%
      addCircleMarkers(
        radius = 4,
        color = "darkgreen",
        stroke = FALSE,
        fillOpacity = 0.7,
        popup = ~paste0("<b>Sample ID:</b> ", sample_id, "<br>",
                        "<b>Taxon:</b> ", taxon_name)
      ) %>%
      addScaleBar(position = "bottomleft")
  })


  output$leaflet_jbc <- renderLeaflet({
    sf <- jbc_sf()
    req(sf)
    leaflet(sf) %>%
      addTiles() %>%
      addCircleMarkers(
        radius = 4,
        color = "blue",
        stroke = FALSE,
        fillOpacity = 0.7,
        popup = ~paste0("<b>Sample ID:</b> ", sample_id, "<br>",
                        "<b>Taxon:</b> ", taxon_name)
      ) %>%
      addScaleBar(position = "bottomleft")
  })






progress_data <- reactive({
  req(list_neu(), list_ch(), list_fr(), jbn_merged(), jbc_merged(), jbuf_merged())
  
  advance_long <- tibble::tibble(
    status = c("all", "all", "all", "sampled", "sampled", "sampled"),
    garden = c("jbn", "jbc", "jbuf", "jbn", "jbc", "jbuf"),
    n = c(
      nrow(list_neu()),     # all jbn
      nrow(list_ch()),      # all jbc
      nrow(list_fr()),      # all jbuf
      nrow(jbn_merged()),   # sampled jbn
      nrow(jbc_merged()),   # sampled jbc
      nrow(jbuf_merged())   # sampled jbuf
    )
  )
  
  percent_df <- advance_long %>%
    tidyr::pivot_wider(names_from = status, values_from = n) %>%
     dplyr::mutate(percent_sampled = sampled / all * 100)
  
  return(percent_df)
})


output$progress_plot <- renderPlot({
  df <- progress_data()
  
  ggplot(df, aes(x = garden, y = percent_sampled)) +
    geom_col(fill = "orange", width = 0.6) +
    coord_flip() +
    labs(x = "Garden", y = "Individuals sampled (%)", fill = NULL) +
    theme_minimal(base_size = 16) +
    theme(
      legend.position = "none",
      axis.title.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    geom_text(aes(label = paste0(round(percent_sampled), "%")),
              hjust = 1.1,
              color = "black",
              size = 5)
})





  ###################################################
  ###################### MOST WANTED ################
  ###################################################

  ### ---------- GENEVA ----------
  data_geneva <- reactive({

    
gen_tax <- data.frame(
  species = paste(list_geneve$genre, list_geneve$espece),
  genus = list_geneve$genre,
  family = list_geneve$famille,
  garden = "ge"
)

    list_geneve$species <- paste(list_geneve$genre, list_geneve$espece)
    
    result <- cover_family_garden_full %>% filter(code_garden == "ge")
    species_list <- cover_species_garden_full %>%
      filter(family %in% unique(result$family)) %>%
      pull(species) %>% unique()
    
    result_genus <- cover_genus_garden_full %>% filter(code_garden == "ge")
    genus_list <- gen_tax %>%
      filter(genus %in% unique(result_genus$genus)) %>%
      pull(species) %>% unique()
    
    list_family <- data.frame(species = species_list, target = "family")
    list_genus  <- data.frame(species = genus_list, target = "genus")
    merged <- unique(rbind(list_family, list_genus))
    
    final <- merge(list_geneve, merged, by = "species", all = FALSE)
    final <- final %>%
      group_by(Code.ipen) %>%
      filter(!(n() > 1 & target == "genus")) %>%
      ungroup()
    
    return(final)
  })

  output$table_mw_geneva <- DT::renderDataTable({
    DT::datatable(data_geneva(), options = list(pageLength = 10))
  })

  output$download_table_mw_Geneva <- downloadHandler(
    filename = function() {
      "most_wanted_geneva.csv"
    },
    content = function(file) {
      write.csv(data_geneva(), file, row.names = FALSE)
    }
  )

  ### ---------- PRAGUE ----------
  data_prague <- reactive({

list_prague$species <- iconv(list_prague$species, from = "", to = "UTF-8", sub = "byte")
list_prague$family <- iconv(list_prague$family, from = "", to = "UTF-8", sub = "byte")

pra_tax <- data.frame(
  species = list_prague$species,
  genus = sub(" .*", "", list_prague$species),               
  family = sub(" .*", "", list_prague$family),
  garden = "pr"
)


    result <- cover_family_garden_full %>% filter(code_garden == "pr")
    species_list <- cover_species_garden_full %>%
      filter(family %in% unique(result$family)) %>%
      pull(species) %>% unique()
    
    result_genus <- cover_genus_garden_full %>% filter(code_garden == "pr")
    genus_list <- pra_tax %>%
      filter(genus %in% unique(result_genus$genus)) %>%
      pull(species) %>% unique()
    
    list_family <- data.frame(species = species_list, target = "family")
    list_genus  <- data.frame(species = genus_list, target = "genus")
    merged <- unique(rbind(list_family, list_genus))
    merged <- merged[!is.na(merged$species), ]
    
    return(merged)
  })

  output$table_mw_prague <- DT::renderDataTable({
    DT::datatable(data_prague(), options = list(pageLength = 10))
  })

  output$download_table_mw_Prague <- downloadHandler(
    filename = function() {
      "most_wanted_prague.csv"
    },
    content = function(file) {
      write.csv(data_prague(), file, row.names = FALSE)
    }
  )

  ### ---------- LONDON ----------
  data_london <- reactive({
    lon_tax <- data.frame(
  species = list_london$TaxonomicName,
  genus = list_london$Genus,               
  family = list_london$Family,
  garden = "lo"
)

    result <- cover_family_garden_full %>% filter(code_garden == "lo")
    species_list <- cover_species_garden_full %>%
      filter(family %in% unique(result$family)) %>%
      pull(species) %>% unique()
    
    result_genus <- cover_genus_garden_full %>% filter(code_garden == "lo")
    genus_list <- lon_tax %>%
      filter(genus %in% unique(result_genus$genus)) %>%
      pull(species) %>% unique()
    
    list_family <- data.frame(species = species_list, target = "family")
    list_genus  <- data.frame(species = genus_list, target = "genus")
    merged <- unique(rbind(list_family, list_genus))
    
    final <- merge(
      list_london,
      merged,
      by.x = "Accepted.Name",
      by.y = "species",
      all = FALSE
    ) %>%
      group_by(Catalogue.Number) %>%
      filter(!(n() > 1 & target == "genus")) %>%
      ungroup()
    
    return(final)
  })

  output$table_mw_london <- DT::renderDataTable({
    DT::datatable(data_london(), options = list(pageLength = 10))
  })

  output$download_table_mw_London <- downloadHandler(
    filename = function() {
      "most_wanted_london.csv"
    },
    content = function(file) {
      write.csv(data_london(), file, row.names = FALSE)
    }
  )




}


shinyApp(ui = ui, server = server)
