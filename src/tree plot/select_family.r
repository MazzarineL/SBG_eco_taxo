library(tibble)
library(ggplot2)
library(dplyr)
library(ggtree)
library(rotl)
library(readr)
library(archive)
library(igraph)
library(myTAI)
library(castor)
library(tidyverse)
library(slider)
library(tidyquant)  


cover_family_garden <- read.csv("D:/gitrepo/SBG_eco_taxo/data/cover_family_garden_500.csv", sep=";")


##########MAJUSCULE

family_test <- "Rosaceae"
##########

#import de l'arbre

mono_id <- rotl::tnrs_match_names(paste(family_test))

taxonomy <- readr::read_delim(
      archive::archive_read("D:/Thèse Metabolomique/Thèse Metabolomique/jardin botanique/otl/ott3.3.tgz", file = "ott3.3/taxonomy.tsv"),
      col_types = cols(),
      delim = "|",
      escape_double = FALSE,
      trim_ws = TRUE)

taxonomy %>%
    dplyr::filter(rank == "domain")

sub_tax_from_to <- data.frame(
    from = taxonomy$parent_uid,
    to = taxonomy$uid )

g <- igraph::graph.data.frame(sub_tax_from_to, directed = TRUE)

sub_g <-igraph::subcomponent(g, paste(mono_id$ott_id), mode = "out") 

g2 <- igraph::induced_subgraph(g, sub_g)

id_sel <- as.numeric(V(g2)$name)

taxonomy_full <- taxonomy[taxonomy$uid %in% id_sel, ]

taxonomy_genus <- taxonomy_full %>% filter(rank == "genus" & is.na(flags))




tree <- rotl::tol_induced_subtree(ott_ids = taxonomy_genus$uid)

tree$tip.label <- gsub("^x_|\\(genus_in_kingdom_Archaeplastida\\)_|_.*", "", tree$tip.label)





#coloration des branches selon presence

cover_genus_garden <- read.csv("D:/gitrepo/SBG_eco_taxo/data/cover_genus_family_garden_500.csv", sep=";")

cover_genus_select <- cover_genus_garden %>%
  dplyr::filter(family == paste(family_test))
cover_genus_select$genus <- gsub("^x ", "", cover_genus_select$genus)



cover_genus_select <- cover_genus_select %>%
  dplyr::mutate(garden = na_if(garden, "fri"), pres = ifelse(is.na(garden), 0, pres))


cover_genus_select <- cover_genus_select %>%
  dplyr::mutate(garden = na_if(garden, "neu"), pres = ifelse(is.na(garden), 0, pres))




genus_cover <- split(cover_genus_select$genus, cover_genus_select$pres)


tree_plot <- ggtree::ggtree(tree, layout ="circular") +
  theme(legend.position = "right", 
        legend.key.size = unit(3, "lines")) + 
  geom_tiplab(size=2, offset=0.5)

g1 <- ggtree::groupOTU(tree_plot, genus_cover, "species") + aes(color = species) +
 theme(legend.position = "right",
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14)) + 
  scale_color_manual(name = "Family", values = c("orange", "darkgreen"), labels = c("Not available", "Available"))

g1




###obtenir l'ordre sur l'arbre
p <- ggtree(tree) + geom_tiplab() + 
    geom_hilight(node = 12, extendto = 2.5)


df_rangement <- data.frame(genus = get_taxa_name(p))

df_rangement <- merge(df_rangement, cover_genus_select, by = "genus", all.x = TRUE, sort = FALSE)


####moyenne mobile 
df_rangement <- df_rangement %>%
  mutate(date = as.Date('2023-01-01') + row_number() - 1)



df_rangement <- df_rangement %>% 
  mutate( 
    reg_7day = slide_dbl(
      pres, # calculer sur les new_cases
      .f = ~sum(.x, na.rm = T), # la fonction est sum() avec les valeurs manquantes supprimées
      .before = 5,
      .after = 5,
      .step = 3))

     df_rangement <- df_rangement %>%
        mutate(reg_7day = if_else(is.na(reg_7day) & pres == 0, 1, reg_7day),
               reg_7day = if_else(is.na(reg_7day) & pres == 1, 2, reg_7day),
               pres = if_else(reg_7day == 0 & pres == 0, 3, pres))
      

plot_moy <- ggplot(data = df_rangement)+
  geom_line(mapping = aes(x = date, y = indexed_7day), size = 1)



df_rangement <- df_rangement %>%
  mutate(pres = if_else(reg_7day == 0 & pres == 0, 3, pres))

###plot
genus_cover <- split(df_rangement$genus, df_rangement$pres)


tree_plot <- ggtree::ggtree(tree, layout ="circular") +
  theme(legend.position = "right", 
        legend.key.size = unit(3, "lines")) + 
  geom_tiplab(size=2, offset=0.5)

g1 <- ggtree::groupOTU(tree_plot, genus_cover, "species") + 
  aes(color = species) +
  theme(legend.position = "right",
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14)) + 
  scale_color_manual(name = "Family", 
                     values = c("0" = "orange", "1" = "darkgreen", "3" = "blue"), 
                     labels = c("Not available", "Available", "Priority"),
                     breaks = c("0", "1", "3"))
g1

# Enregistrer le graphique en format PDF
#ggsave("C:/Users/laboureaum2/desktop/g1test.pdf", plot = g1, device = "pdf")













# Définir le nombre de genres à sélectionner
genus_select <- 15  # Remplacez 5 par le nombre désiré

# Obtenir l'ordre sur l'arbre
p <- ggtree(tree) + geom_tiplab() + 
  geom_hilight(node = 12, extendto = 2.5)

df_rangement <- data.frame(genus = get_taxa_name(p))
df_rangement <- merge(df_rangement, cover_genus_select, by = "genus", all.x = TRUE, sort = FALSE)

# Ajouter une colonne date
df_rangement <- df_rangement %>%
  mutate(date = as.Date('2023-01-01') + row_number() - 1)

# Fonction pour ajuster les valeurs before, after, step
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
        dplyr::mutate(reg_7day = if_else(is.na(reg_7day) & pres == 0, 1, reg_7day),
               reg_7day = if_else(is.na(reg_7day) & pres == 1, 2, reg_7day),
               pres = if_else(reg_7day == 0 & pres == 0, 3, pres))
      

      df_temp <- df_temp %>%
        mutate(pres = if_else(reg_7day == 0 & pres == 0, 3, pres))
      
      count_3 <- sum(df_temp$pres == 3)
      
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


# Split par pres pour la couleur dans le plot
genus_cover <- split(final_best_df$genus, final_best_df$pres)

# Plot de l'arbre avec les couleurs
tree_plot <- ggtree(tree, layout = "circular") +
  theme(legend.position = "right", 
        legend.key.size = unit(3, "lines")) + 
  geom_tiplab(size = 2, offset = 0.5)

g1 <- ggtree::groupOTU(tree_plot, genus_cover, "species") + 
  aes(color = species) +
  theme(legend.position = "right",
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14)) + 
  scale_color_manual(name = "Family", 
                     values = c("0" = "orange", "1" = "darkgreen", "3" = "blue"), 
                     labels = c("Not available", "Available", "Priority"),
                     breaks = c("0", "1", "3"))
g1



















# Fonction pour ajuster les valeurs before, after, step
best_diff <- Inf
best_dfs <- list()
best_dfs_diff <- list()
before_values <- 0:15
after_values <- 0:15
step_values <- 1:10

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
          reg_7day = if_else(is.na(reg_7day) & pres == 0, 1, reg_7day),
          reg_7day = if_else(is.na(reg_7day) & pres == 1, 2, reg_7day),
          pres = if_else(reg_7day == 0 & pres == 0, 3, pres)
        )
      
      count_3 <- sum(df_temp$pres == 3)
      
      # Vérifier si count_3 dépasse le seuil et passer à la boucle suivante si c'est le cas
      if (is.na(count_3) || count_3 > genus_select) {
        next
      }
      
      diff <- abs(count_3 - genus_select)
      
      if (diff == 0) {
        best_dfs[[length(best_dfs) + 1]] <- df_temp
      } else if (diff == 1) {
        best_dfs_diff[[length(best_dfs_diff) + 1]] <- df_temp
      }
      
      if (diff < best_diff) {
        best_diff <- diff
        best_df <- df_temp
      }
    }
  }
}

# Si best_dfs est vide, utiliser best_dfs_diff
if (length(best_dfs) == 0) {
  best_dfs <- best_dfs_diff
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



# Split par pres pour la couleur dans le plot

        incProgress(4/6, detail = "Préparation de la table...")

        output$mytable <- gt::render_gt({
          df_rangement_priority <- final_best_df %>% filter(pres == 3) %>% select(genus)

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

        incProgress(5/6, detail = "Préparation de l'intrigue de la famille...")

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
              labs(title = "Tree of ",family_test) +
              theme(legend.title = element_text(size = 20),  
                   legend.text = element_text(size = 15)) 

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

        incProgress(6/6, detail = "Finalisation...")
      }
    })
  })
