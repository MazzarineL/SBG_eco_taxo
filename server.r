#install.packages(c("shiny", "rsconnect", "ggplot2", "dplyr", "ggtree", "rotl", 
#                   "slider", "gt", "plotbiomes", "rgbif", "sp", "Polychrome",
#                   "rinat", "RColorBrewer", "curl", "maps", "ggvenn","VennDiagram","gridExtra","BiocManager","devtools"))

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

# Définir le serveur
server <- function(input, output, session) {

world <- map_data("world")

cover_genus_garden_full <- read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/taxo_genus_garden.csv") )
cover_species_garden_full <- read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/taxo_species_garden.csv") )

clean_family <- function(fam_vec) {
  fam_vec <- trimws(fam_vec) # enlève espaces autour
  fam_vec <- gsub("\\?", "", fam_vec) # supprime les ?
  fam_vec <- gsub("\\s+", " ", fam_vec) # supprime espaces multiples
  fam_vec <- stringr::str_squish(fam_vec) # nettoyage extra
  fam_vec
}
observe({
  cleaned_families <- sort(unique(clean_family(cover_species_garden_full$family)))
  
  updateSelectInput(
    session,
    inputId = "family",
    choices = cleaned_families,
    selected = cleaned_families[1]  # ou "" si tu veux rien sélectionner par défaut
  )
})

whit_part1.1 <- read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/gift/data_env_gift_part1.csv"), sep = ";")
whit_part1.2 <-  read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/gift/data_env_gift_part2.csv"), sep = ";")
whit_part1.3 <-  read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/gift/data_env_gift_part3.csv"), sep = ";")
whit_part1.4 <-  read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/gift/data_env_gift_part1.4.csv"), sep = ",")
whit_part2 <-  read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/gift/data_env_gift_part4.csv"), sep = ";")
whit_part1.5 <-  read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/gift/data_env_gift_geneve.csv"), sep = ",")
whit_part1.6 <-  read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/gift/data_env_gift_champex.csv"), sep = ",")

all_species_taxo <-  read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/all_species_taxonomy_full.csv"), sep = ",")

whit_part1.4 <- whit_part1.4 %>% dplyr::select(-biome)
whit_part1.5 <- whit_part1.5 %>% dplyr::select(-biome)
whit_part1.6 <- whit_part1.6 %>% dplyr::select(-biome)

whit_part1 <- rbind(whit_part1.1, whit_part1.2,whit_part1.3,whit_part1.4,whit_part1.5,whit_part1.6)  

# Niveaux de garden codes mis à jour
family_levels <- c(
  "fr", "ne", "la", "ge", "ch",
  "fr_ne", "fr_la", "fr_la_ne", "fr_ge", 
  "fr_ge_la", "fr_ge_la_ne", "fr_ge_ne", 
  "ge_la", "ge_la_ne", "ge_ne", "la_ne",
  "ch_fr", "ch_fr_ne", "ch_fr_la", "ch_fr_ge", 
  "ch_fr_ge_ne", "ch_fr_ge_la", "ch_fr_ge_la_ne", 
  "ch_ge", "ch_ge_ne", "ch_ge_la", "ch_ge_la_ne", 
  "ch_la", "ch_ne",
  "ch", 
  NA
)

# Couleurs (ajoute/modifie selon préférences)
color_values <- c(
  "#E74C3C",  # fr
  "#9B59B6",  # ne
  "#3498DB",  # la
  "#F39C12",  # ge
  "#2ECC71",  # ch
  "#8E44AD", "#D35400", "#E67E22", "#2980B9", "#1ABC9C",
  "#2C3E50", "#C0392B", "#FF5733", "#F1C40F", "#16A085", "#A93226",
  "#7D3C98", "#AF601A", "#2471A3", "#17A589", "#641E16",
  "#D5A6BD", "#B9770E", "#196F3D", "#CA6F1E", "#7FB3D5",
  "#FAD7A0", "#F5B7B1", "#A3E4D7", "#BDC3C7",
  "#566573"   # ← Couleur ajoutée pour le 31e élément
)
names(color_values) <- family_levels


# Fonction de labels mise à jour avec "ch" = Champex
generate_labels <- function(family_levels) {
  labels <- sapply(family_levels, function(code) {
    locations <- c()
    if (grepl("fr", code)) locations <- c(locations, "Fribourg")
    if (grepl("ge", code)) locations <- c(locations, "Genève")
    if (grepl("la", code)) locations <- c(locations, "Lausanne")
    if (grepl("ne", code)) locations <- c(locations, "Neuchâtel")
    if (grepl("ch", code)) locations <- c(locations, "Champex")

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
replacement_mapping <- c(
  "fr" = "Fribourg", "ne" = "Neuchâtel", "la" = "Lausanne", "ge" = "Genève", "ch" = "Champex",
  "fr_ne" = "Fribourg and Neuchâtel",
  "fr_la" = "Fribourg and Lausanne",
  "fr_la_ne" = "Fribourg, Lausanne, and Neuchâtel",
  "fr_ge" = "Fribourg and Genève",
  "fr_ge_la" = "Fribourg, Genève, and Lausanne",
  "fr_ge_la_ne" = "Fribourg, Genève, Lausanne, and Neuchâtel",
  "fr_ge_ne" = "Fribourg, Genève, and Neuchâtel",
  "ge_la" = "Genève and Lausanne",
  "ge_la_ne" = "Genève, Lausanne, and Neuchâtel",
  "ge_ne" = "Genève and Neuchâtel",
  "la_ne" = "Lausanne and Neuchâtel",
  "ch_fr" = "Champex and Fribourg",
  "ch_fr_ne" = "Champex, Fribourg and Neuchâtel",
  "ch_fr_la" = "Champex, Fribourg and Lausanne",
  "ch_fr_ge" = "Champex, Fribourg and Genève",
  "ch_fr_ge_ne" = "Champex, Fribourg, Genève and Neuchâtel",
  "ch_fr_ge_la" = "Champex, Fribourg, Genève and Lausanne",
  "ch_fr_ge_la_ne" = "Champex, Fribourg, Genève, Lausanne and Neuchâtel",
  "ch_ge" = "Champex and Genève",
  "ch_ge_ne" = "Champex, Genève and Neuchâtel",
  "ch_ge_la" = "Champex, Genève and Lausanne",
  "ch_ge_la_ne" = "Champex, Genève, Lausanne and Neuchâtel",
  "ch_la" = "Champex and Lausanne",
  "ch_ne" = "Champex and Neuchâtel",
  "NA" = "Not available"
)


observeEvent(input$action, {
  withProgress(message = 'Loading data...', value = 0, {
    output$treePlot <- NULL
    req(input$Garden != "")
    
    # Initialize variables
    taxonomy_merge <- cover_genus_garden_full
    input_code <-input$Garden
    
    # Update progress
    incProgress(1/6, detail = "Processing garden codes...")
    
    # If only one option is selected
    if (length(input_code) == 1) {
      taxonomy_merge$code_garden[!is.na(taxonomy_merge$code_garden)] <- input_code
      taxonomy_merge$code_garden[taxonomy_merge$pres == 0] <- NA
    } else {
      selected_values <- paste(input_code, collapse = "|")
      taxonomy_merge$code_garden[!grepl(selected_values, taxonomy_merge$code_garden)] <- NA
      entire_codes <- c("fr", "ne", "la", "ge","ch")
      diff <- setdiff(entire_codes, input_code)
      taxonomy_merge$code_garden <- gsub(paste(diff, collapse = "|"), "", taxonomy_merge$code_garden)
      taxonomy_merge$code_garden <- gsub("_+", "_", taxonomy_merge$code_garden)
      taxonomy_merge$code_garden <- gsub("^_|_$", "", taxonomy_merge$code_garden)
    }
    
    incProgress(2/6, detail = "Filtering data...")
    
    taxonomy_merge$pres[is.na(taxonomy_merge$pres)] <- 0
    taxonomy_merge <- taxonomy_merge %>%
      mutate(code_garden = na_if(code_garden, ""))
    
    taxonomy_merge <- taxonomy_merge[!is.na(taxonomy_merge$ott_id.family), ]
    
    incProgress(3/6, detail = "Generating phylogenetic tree...")
 
    my_tree <- rotl::tol_induced_subtree(ott_ids = taxonomy_merge$ott_id.family)
    sp_name <- gsub("_.*", "", my_tree$tip.label)
    my_tree$tip.label <- sp_name
    family <- taxonomy_merge$family
    g <- split(family, taxonomy_merge$code_garden)
    
    incProgress(4/6, detail = "Creating plot...")
    
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
    
    incProgress(5/6, detail = "Finalizing...")
  })
})



############################################
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
        entier <- c("fr", "ne", "la","ge","ch")
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
          mutate(code_garden = na_if(code_garden, ""))
        
        cover_genus_garden <- cover_genus_garden[!is.na(cover_genus_garden$ott_id.family), ]
        cover_genus_garden$genus <- gsub("^x ", "", cover_genus_garden$genus)
        cover_genus_garden$pres[is.na(cover_genus_garden$code_garden)] <- 0
        
        incProgress(3/6, detail = "Generating phylogenetic tree...")

incProgress(3/6, detail = "Generating phylogenetic tree...")

# Nettoyage et vérification des ott_ids
valid_ott_ids <- unique(na.omit(cover_genus_garden$uid))

if (length(valid_ott_ids) < 2) {
  showNotification("Pas assez d'ott_ids valides pour générer un arbre phylogénétique.", type = "error")
  output$FamilyPlot <- renderPlot({ NULL })  # Ne pas afficher de plot
  output$mytable <- renderDT({ NULL })       # Ne pas afficher de table
  return(NULL)                               # Arrêt ici
}

max_ids <- 1000  # limite mémoire (ajuster ou enlever si tu veux)
if (length(valid_ott_ids) > max_ids) {
  valid_ott_ids <- valid_ott_ids[1:max_ids]
  showNotification(paste("Limitation à", max_ids, "ott_ids pour éviter problème mémoire."), type = "warning")
}

# Appel sécurisé à la fonction
tree <- rotl::tol_induced_subtree(ott_ids = valid_ott_ids)
tree$tip.label <- gsub("^x_|\\(genus_in_kingdom_Archaeplastida\\)_|_.*", "", tree$tip.label)

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
                mutate(
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
                  mutate(
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
  df_rangement_priority <- final_best_df %>% filter(pres == 3) %>% select(genus)
  
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
            df_rangement_priority <- df_rangement %>% filter(pres == 3) %>% select(genus)
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
      
      incProgress(4/6, detail = "Preparing the table...")
      
      output$mytable <- gt::render_gt({
        df_rangement_priority <- df_rangement %>% filter(pres == 3) %>% select(genus)
        
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
          df_rangement_priority <- df_rangement %>% filter(pres == 3) %>% select(genus)
          write.csv(df_rangement_priority, file, row.names = FALSE)
        }
      )
      
      incProgress(5/6, detail = "Preparing family tree...")
      
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
    
  })
})

#####################################
#########BARPLOT COVER ##############
#####################################
observeEvent(input$action, {
  output$coverplot <- NULL
  req(input$Garden != "")

  input_code <- input$Garden
  input_values <- unlist(strsplit(input_code, "_"))

  cover_plot <- cover_species_garden_full %>%
    dplyr::select(species, genus, family, code_garden)

  # -- Définir les codes valides
  valid_elements <- c("ne", "la", "fr", "ge", "ch")
  code_list <- c("fr", "ne", "la", "ge", "ch", 
                 "fr_ne", "fr_la", "fr_la_ne", "fr_ge", 
                 "fr_ge_la", "fr_ge_la_ne", "fr_ge_ne", 
                 "ge_la", "ge_la_ne", "ge_ne", "la_ne", "NA")

  # -- Filtrer les codes de jardin selon la sélection utilisateur
  filter_code <- function(code, input_values) {
    elements <- unlist(strsplit(code, "_"))
    filtered <- elements[elements %in% input_values]
    if (length(filtered) == 0) return("NA")
    paste(sort(filtered), collapse = "_")
  }

  cover_plot$code_garden <- sapply(cover_plot$code_garden, filter_code, input_values = input_values)
  cover_plot <- dplyr::filter(cover_plot, code_garden != "NA")

  # -- Recomposer codes (limités aux codes valides)
  recompose_code <- function(codes) {
    elements <- unique(unlist(strsplit(codes, "_")))
    valid <- elements[elements %in% valid_elements]
    if (length(valid) == 0) return("NA")
    final_code <- paste(sort(valid), collapse = "_")
    if (final_code %in% code_list) return(final_code)
    return("NA")
  }

  # -- Fonction générique pour species/genus/family
  create_cover_dataframe <- function(data, group_var) {
    unique_groups <- unique(data[[group_var]])
    cover <- lapply(unique_groups, function(g) {
      codes <- unique(data$code_garden[data[[group_var]] == g])
      final_code <- recompose_code(codes)
      data.frame(group = g, code_garden = final_code, stringsAsFactors = FALSE)
    })
    df <- do.call(rbind, cover)
    names(df)[1] <- group_var
    return(df)
  }

  species_cover <- create_cover_dataframe(cover_plot, "species")
  genus_cover   <- create_cover_dataframe(cover_plot, "genus")
  family_cover  <- create_cover_dataframe(cover_plot, "family")

  # -- Table + padding NA
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

  species_table <- add_padding(table(species_cover$code_garden), 390000)
  genus_table   <- add_padding(table(genus_cover$code_garden),   14282)
  family_table  <- add_padding(table(family_cover$code_garden),  508)

  # -- Combine in final df
  build_df <- function(tbl, type_label) {
    data.frame(
      type = type_label,
      av = ifelse(names(tbl) == "NA", "not available", "available"),
      garden = names(tbl),
      count = as.vector(tbl),
      stringsAsFactors = FALSE
    )
  }

  table_full <- rbind(
    build_df(family_table, "family"),
    build_df(genus_table, "genus"),
    build_df(species_table, "species")
  )
  
  table_full$garden <- factor(table_full$garden, levels = code_list)

  # -- Render plot
  output$coverplot <- renderPlot({
    gg2 <- ggplot(table_full, aes(x = type, y = count, fill = garden)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(x = "Type", y = "Count", fill = "Garden") +
      ggtitle("Taxonomic Coverage per Garden Combination") +
      theme_minimal() +
      facet_wrap(~type, scales = "free") +
      scale_fill_manual(
        values = color_values[names(color_values) %in% table_full$garden],
        labels = replacement_mapping[names(color_values) %in% table_full$garden],
        breaks = names(color_values)[names(color_values) %in% table_full$garden]
      ) +
      theme(
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 13)
      )
    print(gg2)
  })

  # -- Download handler
  output$downloadcoverplot <- downloadHandler(
    filename = function() {
      paste0("Barplot_coverage_", Sys.Date(), ".jpg")
    },
    content = function(file) {
      ggsave(filename = file, plot = last_plot(), device = "jpg", width = 14, height = 10)
    }
  )
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

  # Fonction d'affichage du Venn sans sauvegarde sur disque
  display_venn <- function(x, labels, colors, title) {
    # Sauvegarder le répertoire courant
    old_wd <- getwd()
    # Changer temporairement de répertoire vers un répertoire temporaire
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
      grid.text(title, x = 0.3, y = 0.95, gp = gpar(fontsize = 12, fontface = "bold"))
      popViewport()
    })

    # Revenir au répertoire d'origine
    setwd(old_wd)

    return(plot_grob)
  }

  # Générer les Venn diagrams
  venn_species <- display_venn(list_of_species, labels_to_use, colors_to_use, "Species coverage in Botanical Garden")
  venn_genus   <- display_venn(list_of_genus,   labels_to_use, colors_to_use, "Genus coverage in Botanical Garden")
  venn_family  <- display_venn(list_of_family,  labels_to_use, colors_to_use, "Family coverage in Botanical Garden")

  # Légende
  legend_plot <- grid.grabExpr({
    grid.newpage()
    legend_grob <- legendGrob(labels_to_use, pch = 15, gp = gpar(col = colors_to_use, fontsize = 12, fontface = "bold"))
    grid.draw(legend_grob)
  })

  # Affichage dans l'app Shiny
  output$vennplot <- renderPlot({
    grid.arrange(legend_plot, venn_species, venn_genus, venn_family, ncol = 1, heights = c(0.3, 1, 1, 1))
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






 

##################################################################
observeEvent(input$action, {
  withProgress(message = 'Processing...', value = 0, {
    
    output$whitplot  <- NULL
    req(input$Garden != "")
    cover_whit <- cover_species_garden_full
    input_code <- input$Garden
    
    incProgress(1/6, detail = "Converting data types...")
    whit_part1$mean_wc2.0_bio_30s_12 <- as.numeric(whit_part1$mean_wc2.0_bio_30s_12)
    whit_part1$mean_wc2.0_bio_30s_01 <- as.numeric(whit_part1$mean_wc2.0_bio_30s_01)

    whit_part1 <- whit_part1 %>%
        dplyr::select(species, where(is.numeric)) %>%
        dplyr::group_by(species) %>%
        dplyr::summarise_all(mean, na.rm = TRUE)

    whit_part2 <- whit_part2[!duplicated(whit_part2$species), ]
    data_env_select <- whit_part1[, c(1,4,5)]
    mean_df_select <- whit_part2[, c(1,4, 5)]

    incProgress(2/6, detail = "Merging datasets...")
    colnames(data_env_select) <- c("species","temperature", "precipitation")
    colnames(mean_df_select) <- c("species", "temperature", "precipitation")
    mean_df_select$temperature <- mean_df_select$temperature / 10
    data_clim <- rbind(mean_df_select, data_env_select)  
    data_clim$precipitation <- as.numeric(data_clim$precipitation)
    data_clim$temperature <- as.numeric(data_clim$temperature)
    data_clim$species <- as.factor(data_clim$species)
    data_clim$precipitation <- data_clim$precipitation / 10

    incProgress(3/6, detail = "Preparing garden codes...(This might take a minute, please be patient.)")
    unique_species <- unique(cover_whit$species)
    for (species in unique_species) {
      select_taxo <- cover_whit[cover_whit$species == species, ]
      unique_gardens <- unique(select_taxo$garden)
      sorted_gardens <- sort(unique_gardens)
      code_garden <- paste(sorted_gardens, collapse = "_")
      cover_whit$code_garden[cover_whit$species == species] <- code_garden
    }

    incProgress(4/6, detail = "Filtering data based on input...")
    if(length(input_code) == 1) {
      cover_whit$code_garden <- ifelse(!grepl(paste(input_code, collapse = "|"), cover_whit$code_garden), NA, cover_whit$code_garden)
      cover_whit$code_garden[!is.na(cover_whit$code_garden)] <- paste(input_code, collapse = "_")
      cover_whit <- cover_whit %>% filter(!is.na(code_garden))
    } else {
      selected_values <- paste(input_code, collapse = "|")
      cover_whit$code_garden[!grepl(selected_values, cover_whit$code_garden)] <- NA
      entier <- c("fr", "ne", "la","ge","ch")
      diff <- setdiff(entier, input_code)
      cover_whit$code_garden <- gsub(paste(diff, collapse = "|"), "", cover_whit$code_garden)
      cover_whit$code_garden <- gsub("_+", "_", cover_whit$code_garden)
      cover_whit$code_garden <- gsub("^_|_$", "", cover_whit$code_garden)
      cover_whit <- cover_whit %>% filter(!is.na(code_garden))
    }
    cover_whit <- cover_whit %>% dplyr::distinct(species, .keep_all = TRUE)

    incProgress(5/6, detail = "Merging climatic data...")
    data_clim <- merge(data_clim, cover_whit, by = "species")
    data_clim$jardin <- as.factor(data_clim$code_garden)
    data_clim <- data_clim %>% dplyr::mutate(temperature = ifelse(species %in% c("Drosera spathulata", "Duvalia modesta"), 25, temperature))
    data_clim <- data_clim %>% filter(!is.na(temperature))
    data_clim <- data_clim[!duplicated(data_clim$species), ]

    incProgress(6/6, detail = "Generating plot...")
    output$whitplot <- renderPlot({
      isolate({
        whit <- plotbiomes::whittaker_base_plot() +
          geom_point(data = data_clim, 
                     aes(x = temperature, 
                         y = precipitation,
                         color = code_garden),  
                     size = 0.5,             
                     shape = 16,
                     alpha = 0.8) +
          scale_color_manual(
            name = "Garden",
            values = color_values,
            labels = labels,
            breaks = family_levels
          ) +
          labs(title = "Whittaker Plot") +
          theme_minimal() +
          theme(panel.background = element_rect(fill = "white", color = NA)) +
          guides(color = guide_legend(override.aes = list(size = 3)))

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









output$whitplotFamily <- renderPlot({
isolate({

family_test <- input$family
data_clim_sub <- subset(data_clim, family == family_test)

data_clim_sub <- data_clim_sub %>%
  filter(!is.na(temperature) & !is.na(precipitation) & 
         is.finite(temperature) & is.finite(precipitation))


whitfamily <- plotbiomes::whittaker_base_plot() +
          geom_point(data = data_clim_sub, 
                     aes(x = temperature, 
                         y = precipitation,
                         color = code_garden),  
                     size = 1,             
                     shape = 16,
                     alpha = 0.8) +
          scale_color_manual(
            name = "Garden",
            values = color_values,  # Ensure color_values is defined
            labels = labels,        # Ensure labels is defined
            breaks = family_levels  # Ensure family_levels is defined
          ) +
          labs(title = "Whittaker Plot") +
          theme_minimal() +
          theme(panel.background = element_rect(fill = "white", color = NA)) +
          guides(color = guide_legend(override.aes = list(size = 1)))

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
data_clim_sub <- subset(data_clim, family == family_test)

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
    family_test <- input$family
    data_clim_sub <- subset(data_clim, family == family_test)

   data_clim_sub <- data_clim_sub %>%
         filter(!is.na(temperature) & !is.na(precipitation) & 
         is.finite(temperature) & is.finite(precipitation))

# Replace garden values using the replacement mapping
        data_clim_sub <- data_clim_sub %>%
          mutate(garden = replacement_mapping[garden])  # Ensure replacement_mapping is defined

        # Create the Plotly plot
        plot <- ggplot(data_clim_sub, aes(x = temperature, y = precipitation, color = code_garden, text = paste("Species:", species))) + 
          geom_point(size = 1, shape = 16, alpha = 0.8) +
          labs(title = "Whittaker Plot") +
          theme_minimal() +
          theme(panel.background = element_rect(fill = "white", color = NA)) +
          scale_color_manual(
            name = "Garden",
            values = color_values,  # Ensure color_values is defined
            labels = labels,        # Ensure labels is defined
            breaks = family_levels  # Ensure family_levels is defined
          ) +
          coord_cartesian(xlim = c(-15, 30), ylim = c(-5, 450))

        ggplotly(plot, tooltip = "text")
  })
})


})





  ##############################################

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

# Fonction réactive pour le tracé de la carte
observeEvent(input$goButton, {
  req(filtered_data())

  family_map <- filtered_data()

  if (!is.null(input$GPS_species) && length(input$GPS_species) > 0) {
    family_map <- family_map %>% filter(species %in% input$GPS_species)
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
          select(longitude, latitude)

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



  
  ##############################################################

cover_species <- cover_species_garden_full
all_species <- all_species_taxo

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
      select(ipen, secteur, idTaxon, matched_name) %>%
      mutate(idTaxon = sapply(strsplit(trimws(idTaxon), "\\s+"), function(x) paste(head(x, 2), collapse = " ")))
  })
  
  list_neu <- reactive({
    neu_cult2024 <- read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/botanical_garden_list/list_neu_2024.csv"), sep = ";") 
    neu_cult2023 <-read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/botanical_garden_list/list_neu_2023.csv"), sep = ";") 
    colnames(neu_cult2023)
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
      select(code_ipen, famille, genre, espece, groupe, sous_groupe, years) %>%
      mutate(
        species = tolower(paste(genre, espece)),
        groupe = tolower(groupe) %>% gsub(" ", "_", .) %>% gsub("'", "", .) %>% stri_trans_general("Latin-ASCII"),
        sous_groupe = tolower(sous_groupe) %>% gsub(" ", "_", .) %>% gsub("'", "", .) %>% stri_trans_general("Latin-ASCII")
      )
    
    df$sous_groupe[is.na(df$sous_groupe)] <- df$groupe[is.na(df$sous_groupe)]
    
    df <- df[!duplicated(df$code_ipen), ]
    df
  })
  colnames(df)
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
  
  # jbuf_sf pour leaflet
  jbuf_sf <- reactive({
    df <- jbuf_merged()
    df <- df[!is.na(df$x_coord) & !is.na(df$y_coord) & df$x_coord != "" & df$y_coord != "", ]
    sf <- st_as_sf(df, coords = c("x_coord", "y_coord"), crs = 2056, remove = FALSE)
    st_transform(sf, crs = 4326)
  })
  
  # jbn_sf pour leaflet
  jbn_sf <- reactive({
    df <- jbn_merged()
    df <- df[!is.na(df$x_coord) & !is.na(df$y_coord) & df$x_coord != "" & df$y_coord != "", ]
    sf <- st_as_sf(df, coords = c("x_coord", "y_coord"), crs = 2056, remove = FALSE)
    st_transform(sf, crs = 4326)
  })
  
  # Render DataTables
  output$table_jbuf <- renderDT({
    datatable(jbuf_merged(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$table_jbn <- renderDT({
    datatable(jbn_merged(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
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








advance_long <- tibble::tribble(
  ~status,  ~garden, ~count,
  "all",     "jbn",     2230,
  "all",     "jbuf",    5225,
  "sampled", "jbn",     1634,
  "sampled", "jbuf",    2364
)
advance_long <- advance_long %>%
  group_by(garden) %>%
  mutate(max_count = max(count),
         prop = count / max_count * 100) %>%
  ungroup()

  output$progress_plot <- renderPlot({
    ggplot(advance_long, aes(x = garden, y = prop, fill = status)) +
      geom_bar(stat = "identity", position = "stack", width = 0.6) +
      scale_fill_manual(values = c("all" = "grey70", "sampled" = "orange")) +
      coord_flip() +  # barre horizontale = plus "barre de chargement"
      labs(x = "Garden", y = "Progress (%)", fill = "Status") +
      theme_minimal(base_size = 16) +
      theme(
        legend.position = "top",
        axis.title.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      geom_text(aes(label = paste0(round(prop), "%")),
                position = position_stack(vjust = 0.5),
                color = "black",
                size = 5)
  })

}
