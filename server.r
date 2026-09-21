# install.packages(c("shiny","rsconnect","ggplot2","dplyr","ggtree","rotl","slider",
#   "gt","plotbiomes","rgbif","sp","Polychrome","rinat","RColorBrewer","curl","maps",
#   "ggvenn","VennDiagram","gridExtra","BiocManager","devtools","UpSetR","httr",
#   "jsonlite","stringr","sf","stringi","tibble","tidyr","colorspace","DT","plotly"))

library(DT)
library(BiocManager)
library(shiny)
library(rsconnect)
library(ggplot2)
library(dplyr)
library(devtools)
library(ggtree)
library(rotl)
library(slider)
library(tidyquant)
library(gt)
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
library(stringr)   # une seule fois
library(sf)
library(rmapshaper)
library(nngeo)
library(stringi)
library(ggspatial)
library(tibble)
library(tidyr)
library(colorspace)
library(leaflet)
library(plotly)

# ══════════════════════════════════════════════════════════════════════════════
# DONNÉES GLOBALES — chargées une seule fois au démarrage, partagées entre sessions
# ══════════════════════════════════════════════════════════════════════════════

world <- map_data("world")

cover_family_garden_full  <- read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/taxo_family_garden.csv"))
cover_genus_garden_full   <- read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/taxo_genus_garden.csv"))
cover_species_garden_full <- read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/taxo_species_garden.csv"))
all_species_taxo          <- read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/all_species_taxonomy_full.csv"))

list_geneve  <- read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/botanical_garden_list/list_geneva.csv"), sep = ";")
list_lausanne<- read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/botanical_garden_list/list_lausanne.csv"), sep = ";")
list_prague  <- read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/botanical_garden_list/list_prague.csv"), sep = ";") %>%
  dplyr::mutate(across(where(is.character), ~iconv(.x, from = "", to = "UTF-8", sub = "")))

# London = fusion de 3 listes Kew (version complète)
list_kew_PoW <- read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/botanical_garden_list/kew_PoW_list.csv"), sep = ";")
list_kew_RG  <- read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/botanical_garden_list/kew_rock_garden_list.csv"), sep = ",")
list_kew_TH  <- read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/botanical_garden_list/kew_temperate_house_list.csv"), sep = ";")
list_london  <- bind_rows(list_kew_TH, list_kew_PoW, list_kew_RG)

gift_fusion  <- read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/gift/data_env_gift_fusion.csv"))

# ── Palette et labels (calculés une seule fois) ──────────────────────────────
base_gardens <- c("fr", "ne", "la", "ge", "ch", "lo", "pr")

generate_combinations <- function(gardens) {
  combis <- unlist(lapply(seq_along(gardens), function(n) {
    apply(combn(gardens, n), 2, function(x) paste(sort(x), collapse = "_"))
  }))
  c(combis, "NA")
}
family_levels <- generate_combinations(base_gardens)

# Couleurs
color_values <- setNames(rep(NA_character_, length(family_levels)), family_levels)
color_values["NA"] <- "grey"
distinct_colors <- qualitative_hcl(length(base_gardens), palette = "Dark 3")
names(distinct_colors) <- base_gardens
color_values[base_gardens] <- distinct_colors

remaining_labels <- names(color_values)[is.na(color_values)]
if (length(remaining_labels) > 0) {
  set.seed(42)  # reproductibilité des couleurs HSV
  hsv_colors <- grDevices::hsv(
    h = seq(0, 1, length.out = length(remaining_labels) + 1)[-1],
    s = runif(length(remaining_labels), 0.6, 1),
    v = runif(length(remaining_labels), 0.7, 1)
  )
  names(hsv_colors) <- remaining_labels
  color_values[remaining_labels] <- hsv_colors
}

# Labels lisibles — une seule fonction au lieu de deux quasi-identiques
make_labels <- function(codes, prefix = "") {
  garden_names <- c(fr = "Fribourg", ge = "Geneva", la = "Lausanne",
                    ne = "Neuchâtel", ch = "Champex", lo = "London", pr = "Prague")
  sapply(codes, function(code) {
    parts <- intersect(strsplit(code, "_")[[1]], names(garden_names))
    if (length(parts) == 0) return("Not available")
    paste0(prefix, paste(garden_names[parts], collapse = ", "))
  }, USE.NAMES = TRUE)
}
labels             <- make_labels(family_levels, prefix = "Available in ")
replacement_mapping<- make_labels(family_levels)

# ── Helpers partagés ─────────────────────────────────────────────────────────

clean_family <- function(fam_vec) {
  fam_vec |> trimws() |> (\(x) gsub("\\?", "", x))() |> stringr::str_squish()
}

# Filtre et recompose les codes jardin selon la sélection
filter_garden_code <- function(code, selected) {
  parts <- intersect(strsplit(code, "_")[[1]], selected)
  if (length(parts) == 0) return(NA_character_)
  paste(sort(parts), collapse = "_")
}

apply_garden_filter <- function(df, input_code) {
  df$code_garden <- sapply(df$code_garden, filter_garden_code, selected = input_code)
  df$code_garden[df$code_garden == ""] <- NA
  df
}

# Algorithme de sélection phylogénétique (partagé Family Tree & Genus Tree)
select_priority <- function(df_rangement, pres_col, n_select) {
  before_values <- 0:10
  after_values  <- 0:10
  step_values   <- 1:5
  best_diff <- Inf
  best_dfs  <- list()
  best_df   <- NULL

  for (before in before_values) {
    for (after in after_values) {
      for (step in step_values) {
        df_temp <- df_rangement %>%
          dplyr::mutate(
            reg = slider::slide_dbl(
              .data[[pres_col]],
              .f = ~sum(.x, na.rm = TRUE),
              .before = before, .after = after, .step = step
            )
          ) %>%
          dplyr::mutate(
            reg = dplyr::case_when(
              is.na(reg) & .data[[pres_col]] == 0 ~ 1,
              is.na(reg) & .data[[pres_col]] == 1 ~ 2,
              TRUE ~ reg
            ),
            !!pres_col := dplyr::if_else(reg == 0 & .data[[pres_col]] == 0, 3L, as.integer(.data[[pres_col]]))
          )
        count_3 <- sum(df_temp[[pres_col]] == 3, na.rm = TRUE)
        if (is.na(count_3) || count_3 > n_select) next
        diff_val <- abs(count_3 - n_select)
        if (diff_val == 0) best_dfs[[length(best_dfs) + 1]] <- df_temp
        if (diff_val < best_diff) { best_diff <- diff_val; best_df <- df_temp }
      }
    }
  }

  # Sélectionner celui qui maximise l'espacement
  min_spacing <- function(df) {
    idx <- which(df[[pres_col]] == 3)
    if (length(idx) < 2) return(0)
    min(diff(idx))
  }
  if (length(best_dfs) > 0) {
    spacings <- sapply(best_dfs, min_spacing)
    best_df  <- best_dfs[[which.max(spacings)]]
  }
  if (is.null(best_df)) best_df <- df_rangement  # fallback
  best_df
}

# ══════════════════════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════════════════════

server <- function(input, output, session) {

  # Mise à jour des familles nettoyées dans le selectInput
  observe({
    cleaned <- sort(unique(clean_family(cover_species_garden_full$family)))
    updateSelectInput(session, "family", choices = cleaned, selected = cleaned[1])
  })

  # Genus Tree — chargement server-side pour éviter le warning sur les grands selectize
  observe({
    genus_choices <- sort(unique(iconv(cover_species_garden_full$genus, to = "UTF-8")))
    updateSelectizeInput(session, "genus", choices = genus_choices, server = TRUE)
  })

  # ── Données réactives ──────────────────────────────────────────────────────
  data_clim_reactive <- reactiveVal(NULL)

  # ════════════════════════════════════════════════════════════════════════════
  # PHYLO TREE — GARDEN
  # ════════════════════════════════════════════════════════════════════════════
  observeEvent(input$action, {
    withProgress(message = "Building garden tree…", value = 0, {
      req(length(input$Garden) > 0)
      input_code <- input$Garden

      taxonomy_merge <- apply_garden_filter(cover_genus_garden_full, input_code)
      taxonomy_merge$pres[is.na(taxonomy_merge$pres)] <- 0
      taxonomy_merge <- taxonomy_merge %>%
        dplyr::mutate(code_garden = na_if(code_garden, "")) %>%
        dplyr::filter(!is.na(ott_id.family))

      incProgress(0.4, detail = "Fetching phylogenetic tree…")
      my_tree <- rotl::tol_induced_subtree(ott_ids = taxonomy_merge$ott_id.family)
      # Nettoyage robuste des tip labels
      my_tree$tip.label <- gsub("^x_|\\(genus_in_kingdom_Archaeplastida\\)_|_ott[0-9]+", "",
                                my_tree$tip.label)
      g <- split(taxonomy_merge$family, taxonomy_merge$code_garden)

      incProgress(0.4, detail = "Rendering plot…")
      tree_plot <- ggtree::ggtree(my_tree, layout = "circular") + geom_tiplab(size = 4, offset = 0.5)
      g2 <- ggtree::groupOTU(tree_plot, g, "family") +
        aes(color = family) +
        scale_color_manual(name = "Garden", values = color_values,
                           labels = labels, breaks = family_levels) +
        theme(legend.title = element_text(size = 20),
              legend.text  = element_text(size = 15))

      output$treePlot <- renderPlot({ print(g2) })
      output$downloadFullPlot <- downloadHandler(
        filename = function() paste0("Garden_tree_", Sys.Date(), ".pdf"),
        content  = function(file) ggsave(file, g2, device = "pdf", width = 50, height = 50, units = "cm")
      )
    })
  })

  # ════════════════════════════════════════════════════════════════════════════
  # PHYLO TREE — FAMILY
  # ════════════════════════════════════════════════════════════════════════════
  observeEvent(input$actionfamily, {
    withProgress(message = "Building family tree…", value = 0, {
      req(length(input$Garden) > 0)

      # Reset outputs
      output$onlygenus  <- renderDT({ NULL })
      output$mytable    <- renderDT({ NULL })
      output$FamilyPlot <- renderPlot({ NULL })
      output$textgenus  <- renderText({ NULL })

      family_test  <- input$family
      genus_select <- input$genus_select
      input_code   <- input$Garden

      cover_genus_garden <- apply_garden_filter(cover_genus_garden_full, input_code) %>%
        dplyr::filter(family == family_test) %>%
        dplyr::mutate(pres = replace(pres, is.na(pres), 0),
                      code_garden = na_if(code_garden, ""),
                      pres = replace(pres, is.na(code_garden), 0)) %>%
        dplyr::filter(!is.na(ott_id.family)) %>%
        dplyr::mutate(genus = gsub("^x ", "", genus))

      incProgress(0.2, detail = "Checking genus count…")
      unique_genera <- dplyr::n_distinct(cover_genus_garden$genus)

      if (unique_genera < 2) {
        output$onlygenus <- DT::renderDT({
          cover_species_garden_full %>%
            dplyr::filter(family == family_test) %>%
            dplyr::select(dplyr::any_of(c("species","genus","family","garden","pres"))) %>%
            datatable(options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
        })
        output$textgenus <- renderText("Tree not available: only one genus in this family.")
        return()
      }

      valid_ott_ids <- unique(na.omit(cover_genus_garden$uid))
      if (length(valid_ott_ids) < 2) {
        showNotification("Not enough valid OTT IDs.", type = "error"); return()
      }
      if (length(valid_ott_ids) > 150) {
        valid_ott_ids <- sample(valid_ott_ids, 150)
        showNotification("Tree reduced to 150 genera to avoid memory issues.", type = "warning")
      }

      incProgress(0.3, detail = "Fetching phylogenetic tree…")
      tree <- tryCatch(
        rotl::tol_induced_subtree(ott_ids = valid_ott_ids),
        error = function(e) {
          showNotification("Tree too large to build — skipped.", type = "error", duration = 6)
          NULL
        }
      )
      if (is.null(tree)) return()

      tree$tip.label <- gsub("^x_|\\(genus_in_kingdom_Archaeplastida\\)_|_ott[0-9]+", "",
                             tree$tip.label)
      p <- ggtree(tree) + geom_tiplab()
      df_rangement <- data.frame(genus = get_taxa_name(p)) %>%
        merge(cover_genus_garden, by = "genus", all.x = TRUE, sort = FALSE)

      incProgress(0.3, detail = "Selecting priority genera…")
      final_best_df <- select_priority(df_rangement, "pres", genus_select)

      output$mytable <- DT::renderDT({
        priority <- final_best_df %>% dplyr::filter(pres == 3) %>% dplyr::select(genus)
        pad  <- (3 - nrow(priority) %% 3) %% 3
        mat  <- matrix(c(priority$genus, rep(NA, pad)), ncol = 3, byrow = TRUE)
        datatable(as.data.frame(mat), rownames = FALSE,
                  colnames = c("Genus 1","Genus 2","Genus 3"),
                  options = list(pageLength = 10, scrollX = TRUE))
      })
      output$downloadTable <- downloadHandler(
        filename = function() paste0("Priority_", family_test, ".csv"),
        content  = function(file) {
          final_best_df %>% dplyr::filter(pres == 3) %>%
            dplyr::select(genus) %>% write.csv(file, row.names = FALSE)
        }
      )

      genus_cover <- split(final_best_df$genus, final_best_df$pres)
      tree_family <- ggtree::ggtree(tree, layout = "circular") + geom_tiplab(size = 3, offset = 0.5)
      tree_family <- ggtree::groupOTU(tree_family, genus_cover, "species") + aes(color = species) +
        scale_color_manual(name = "Genus",
          values = c("0" = "orange","1" = "darkgreen","3" = "blue"),
          labels = c("Not available","Available","Priority"),
          breaks = c("0","1","3")) +
        labs(title = paste("Tree of", family_test)) +
        theme(legend.title = element_text(size = 20), legend.text = element_text(size = 15))

      output$FamilyPlot <- renderPlot({ print(tree_family) })
      output$downloadFamilyPlot <- downloadHandler(
        filename = function() paste0("FamilyTree_", family_test, ".pdf"),
        content  = function(file) ggsave(file, tree_family, device = "pdf", width = 40, height = 40, units = "cm")
      )
    })
  })

  # ════════════════════════════════════════════════════════════════════════════
  # PHYLO TREE — GENUS
  # ════════════════════════════════════════════════════════════════════════════
  observeEvent(input$actiongenus, {
    withProgress(message = "Building genus tree…", value = 0, {
      req(length(input$Garden) > 0)

      output$onlyspecies  <- renderDT({ NULL })
      output$mytableGenus <- renderDT({ NULL })
      output$GenusPlot    <- renderPlot({ NULL })
      output$textspecies  <- renderText({ NULL })

      genus_test      <- input$genus
      species_select  <- input$species_select
      input_code      <- input$Garden

      cover_species_garden <- apply_garden_filter(cover_species_garden_full, input_code) %>%
        dplyr::filter(genus == genus_test) %>%
        dplyr::mutate(
          species = trimws(gsub("[''].*$", "", species)),
          pres    = replace(pres, is.na(pres), 0),
          code_garden = na_if(code_garden, ""),
          pres    = replace(pres, is.na(code_garden), 0)
        )

      unique_sp <- dplyr::n_distinct(cover_species_garden$species)
      if (unique_sp < 2) {
        output$onlyspecies <- DT::renderDT({
          cover_species_garden_full %>%
            dplyr::filter(genus == genus_test) %>%
            dplyr::select(dplyr::any_of(c("species","genus","family","garden","pres"))) %>%
            datatable(options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
        })
        output$textspecies <- renderText("Tree not available: only one species in this genus.")
        return()
      }

      incProgress(0.25, detail = "Resolving OTT IDs…")
      resolved_ott <- tryCatch(
        rotl::tnrs_match_names(unique(cover_species_garden$species)),
        error = function(e) NULL
      )
      if (is.null(resolved_ott)) {
        showNotification("OTT name resolution failed.", type = "error"); return()
      }
      resolved_ott <- resolved_ott[!is.na(resolved_ott$ott_id), ]
      cover_species_garden <- merge(cover_species_garden, resolved_ott,
                                    by.x = "species", by.y = "unique_name", all.x = TRUE)
      cover_species_garden <- cover_species_garden[!is.na(cover_species_garden$ott_id), ]

      valid_ott_ids <- unique(na.omit(cover_species_garden$ott_id))
      if (length(valid_ott_ids) < 2) {
        showNotification("Not enough valid OTT IDs.", type = "error"); return()
      }
      if (length(valid_ott_ids) > 150) {
        valid_ott_ids <- sample(valid_ott_ids, 150)
        showNotification("Tree reduced to 150 species.", type = "warning")
      }

      incProgress(0.3, detail = "Fetching phylogenetic tree…")
      tree <- tryCatch(
        rotl::tol_induced_subtree(ott_ids = valid_ott_ids),
        error = function(e) {
          showNotification("Tree too large to build — skipped.", type = "error", duration = 6)
          NULL
        }
      )
      if (is.null(tree)) return()

      tree_tip_label <- tree$tip.label
      tree$tip.label <- gsub("^x_|\\(genus_in_kingdom_Archaeplastida\\)_|_ott[0-9]+", "",
                             tree$tip.label)

      df_rangement <- data.frame(species = gsub("_", " ", tree$tip.label)) %>%
        dplyr::mutate(species = sub(" [^ ]+$", "", species)) %>%
        merge(cover_species_garden, by = "species", all.x = TRUE, sort = FALSE)

      incProgress(0.25, detail = "Selecting priority species…")
      final_best_df <- select_priority(df_rangement, "pres", species_select)

      output$mytableGenus <- DT::renderDT({
        priority <- final_best_df %>% dplyr::filter(pres == 3) %>% dplyr::select(species)
        pad <- (3 - nrow(priority) %% 3) %% 3
        mat <- matrix(c(priority$species, rep(NA, pad)), ncol = 3, byrow = TRUE)
        datatable(as.data.frame(mat), rownames = FALSE,
                  colnames = c("Species 1","Species 2","Species 3"),
                  options = list(pageLength = 10, scrollX = TRUE))
      })
      output$downloadTable2 <- downloadHandler(
        filename = function() paste0("Priority_", genus_test, ".csv"),
        content  = function(file) {
          final_best_df %>% dplyr::filter(pres == 3) %>%
            dplyr::select(species) %>% write.csv(file, row.names = FALSE)
        }
      )

      # Correspondance tip labels complets ↔ noms simplifiés
      match_table <- data.frame(
        label_clean = tolower(gsub("_ott[0-9]+", "", gsub("_", " ", tree_tip_label))),
        label_full  = tree_tip_label
      )
      final_best_df$species <- match_table$label_full[
        match(tolower(final_best_df$species), match_table$label_clean)
      ]

      species_cover <- split(final_best_df$species, final_best_df$pres)
      tree_genus <- ggtree::ggtree(tree, layout = "circular") + geom_tiplab(size = 3, offset = 0.5)
      tree_genus <- ggtree::groupOTU(tree_genus, species_cover, "species") + aes(color = species) +
        scale_color_manual(name = "Species",
          values = c("0" = "orange","1" = "darkgreen","3" = "blue"),
          labels = c("Not available","Available","Priority"),
          breaks = c("0","1","3")) +
        labs(title = paste("Tree of genus", genus_test)) +
        theme(legend.title = element_text(size = 20), legend.text = element_text(size = 15))

      output$GenusPlot <- renderPlot({ print(tree_genus) })
      output$downloadGenusPlot <- downloadHandler(
        filename = function() paste0("GenusTree_", genus_test, ".pdf"),
        content  = function(file) ggsave(file, tree_genus, device = "pdf", width = 40, height = 40, units = "cm")
      )
    })
  })

  # ════════════════════════════════════════════════════════════════════════════
  # BARPLOT + PIE CHART COVERAGE
  # ════════════════════════════════════════════════════════════════════════════
  observeEvent(input$action, {
    req(length(input$Garden) > 0)
    withProgress(message = "Computing coverage…", value = 0, {
      input_values <- input$Garden

      cover_plot <- cover_species_garden_full %>%
        dplyr::select(species, genus, family, garden)

      incProgress(0.3, detail = "Filtering garden codes…")
      cover_plot$garden <- sapply(cover_plot$garden, filter_garden_code, selected = input_values)
      cover_plot <- dplyr::filter(cover_plot, !is.na(garden))

      recompose_code <- function(codes) {
        parts <- unique(unlist(strsplit(codes, "_")))
        valid <- intersect(parts, base_gardens)
        if (length(valid) == 0) return(NA_character_)
        paste(sort(valid), collapse = "_")
      }
      create_cover_df <- function(data, group_var) {
        data %>%
          dplyr::group_by(.data[[group_var]]) %>%
          dplyr::summarise(garden = recompose_code(unique(garden)), .groups = "drop") %>%
          dplyr::rename(group = 1)
      }
      add_padding <- function(tbl, total) {
        gap <- total - sum(tbl)
        if ("NA" %in% names(tbl)) tbl["NA"] <- tbl["NA"] + gap
        else tbl <- c(tbl, "NA" = gap)
        tbl
      }
      build_df <- function(tbl, label) {
        data.frame(type = label, garden = names(tbl), count = as.vector(tbl),
                   stringsAsFactors = FALSE)
      }

      incProgress(0.4, detail = "Building tables…")
      species_cover <- create_cover_df(cover_plot, "species")
      genus_cover   <- create_cover_df(cover_plot, "genus")
      family_cover  <- create_cover_df(cover_plot, "family")

      species_table <- add_padding(table(species_cover$garden), 390000)
      genus_table   <- add_padding(table(genus_cover$garden),   14282)
      family_table  <- add_padding(table(family_cover$garden),  508)

      table_full <- rbind(
        build_df(family_table,  "family"),
        build_df(genus_table,   "genus"),
        build_df(species_table, "species")
      )
      table_full$garden <- factor(table_full$garden, levels = names(color_values))

      incProgress(0.3, detail = "Rendering plots…")
      common_scale <- scale_fill_manual(
        values = color_values,
        labels = replacement_mapping[names(color_values) %in% table_full$garden],
        breaks = names(color_values)[names(color_values) %in% table_full$garden]
      )

      p_bar <- ggplot(table_full, aes(x = type, y = count, fill = garden)) +
        geom_bar(stat = "identity", position = "stack") +
        facet_wrap(~type, scales = "free") +
        labs(x = "Type", y = "Count", fill = "Garden",
             title = "Taxonomic Coverage per Garden Combination") +
        theme_minimal() + common_scale +
        theme(legend.title = element_text(size = 16), legend.text = element_text(size = 13))

      p_pie <- ggplot(table_full, aes(x = "", y = count, fill = garden)) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        coord_polar("y") + facet_wrap(~type, scales = "free") +
        labs(title = "Taxonomic Coverage per Garden Combination", fill = "Garden") +
        theme_void() + common_scale +
        theme(strip.text = element_text(size = 14, face = "bold"),
              legend.title = element_text(size = 14), legend.text = element_text(size = 12))

      output$coverplot <- renderPlot({ print(p_bar) })
      output$piechart  <- renderPlot({ print(p_pie) })
      output$downloadcoverplot <- downloadHandler(
        filename = function() paste0("Coverage_barplot_", Sys.Date(), ".jpg"),
        content  = function(file) ggsave(file, p_bar, device = "jpg", width = 14, height = 10)
      )
      output$dlpiechart <- downloadHandler(
        filename = function() paste0("Coverage_pie_", Sys.Date(), ".png"),
        content  = function(file) ggsave(file, p_pie, width = 10, height = 8)
      )
    })
  })

  # ════════════════════════════════════════════════════════════════════════════
  # VENN DIAGRAM
  # ════════════════════════════════════════════════════════════════════════════
  observeEvent(input$action, {
    req(length(input$Garden) > 0)
    input_code <- input$Garden

    filtered <- cover_species_garden_full %>%
      dplyr::filter(!is.na(species), !is.na(garden), !is.na(genus), !is.na(family),
                    garden %in% input_code)

    make_lists <- function(col) {
      setNames(lapply(input_code, function(g) filtered[[col]][filtered$garden == g]), input_code)
    }
    list_of_species <- make_lists("species")
    list_of_genus   <- make_lists("genus")
    list_of_family  <- make_lists("family")

    labels_venn <- replacement_mapping[input_code]
    colors_venn <- color_values[input_code]

    display_venn <- function(x, labs, cols, title) {
      old_wd <- getwd(); on.exit(setwd(old_wd))
      setwd(tempdir())
      grid.grabExpr({
        grid.newpage()
        pushViewport(viewport(width = 0.5, height = 0.5))
        vobj <- venn.diagram(x, filename = NULL, category.names = labs,
                             fill = unname(cols), lwd = 1, lty = "blank", cex = 0.8,
                             fontface = "italic", cat.cex = 0, cat.default.pos = "outer",
                             cat.dist = rep(0.05, length(labs)))
        grid.draw(vobj)
        grid.text(title, x = 0.3, y = 1.2, gp = gpar(fontsize = 12, fontface = "bold"))
        popViewport()
      })
    }

    venn_sp  <- display_venn(list_of_species, labels_venn, colors_venn, "Species")
    venn_gen <- display_venn(list_of_genus,   labels_venn, colors_venn, "Genus")
    venn_fam <- display_venn(list_of_family,  labels_venn, colors_venn, "Family")

    legend_grob <- grid.grabExpr({
      grid.newpage()
      grid.draw(legendGrob(labels_venn, pch = 15,
                           gp = gpar(col = colors_venn, fontsize = 12, fontface = "bold")))
    })

    final_venn <- arrangeGrob(
      arrangeGrob(venn_sp, venn_gen, venn_fam, ncol = 1),
      legend_grob, ncol = 2, widths = c(3, 1)
    )
    output$vennplot  <- renderPlot({ grid.draw(final_venn) })
    output$dlvenplot <- downloadHandler(
      filename = function() paste0("Venn_", Sys.Date(), ".jpg"),
      content  = function(file) {
        jpeg(file, width = 10, height = 8, units = "in", res = 150)
        grid.draw(final_venn)
        dev.off()
      }
    )
  })

  # ════════════════════════════════════════════════════════════════════════════
  # WHITTAKER — GARDEN
  # ════════════════════════════════════════════════════════════════════════════
  observeEvent(input$action, {
    withProgress(message = "Building Whittaker plot…", value = 0, {
      req(length(input$Garden) > 0)
      input_code <- input$Garden

      cover_whit <- gift_fusion
      if (length(input_code) == 1) {
        cover_whit <- cover_whit %>% dplyr::filter(grepl(input_code, code_garden))
        cover_whit$code_garden <- input_code
      } else {
        cover_whit <- cover_whit %>%
          dplyr::filter(sapply(code_garden, function(x) any(strsplit(x, "_")[[1]] %in% input_code)))
        cover_whit$code_garden <- sapply(cover_whit$code_garden, filter_garden_code, selected = input_code)
      }
      cover_whit <- dplyr::distinct(cover_whit, species, .keep_all = TRUE)
      data_clim_reactive(cover_whit)

      incProgress(0.7, detail = "Rendering…")

      valid_levels_whit <- intersect(family_levels, unique(as.character(cover_whit$code_garden)))

      p_whit <- plotbiomes::whittaker_base_plot() +
        geom_point(
          data = cover_whit,
          aes(x = temperature, y = precipitation,
              color = code_garden,
              text  = paste0(
                "<b>", ifelse(is.na(species), "unknown", species), "</b><br>",
                "Garden: ", replacement_mapping[code_garden], "<br>",
                "Temp: ", round(temperature, 1), " °C<br>",
                "Precip: ", round(precipitation, 0), " mm"
              )),
          size = 1.5, alpha = 0.8
        ) +
        scale_color_manual(
          name   = "Garden",
          values = color_values[valid_levels_whit],
          labels = labels[valid_levels_whit],
          breaks = valid_levels_whit,
          drop   = TRUE
        ) +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "white", color = NA),
              legend.title = element_text(size = 14), legend.text = element_text(size = 12)) +
        guides(color = guide_legend(override.aes = list(size = 5)))

      output$whitplot <- renderPlotly({
        ggplotly(p_whit, tooltip = "text") %>%
          plotly::layout(legend = list(tracegroupgap = 10))
      })
      output$dlwhitplot <- downloadHandler(
        filename = function() paste0("Whittaker_garden_", Sys.Date(), ".jpg"),
        content  = function(file) ggsave(file, p_whit, device = "jpg", width = 14, height = 10)
      )
    })
  })

  # ════════════════════════════════════════════════════════════════════════════
  # WHITTAKER — FAMILY (statique + plotly interactif)
  # ════════════════════════════════════════════════════════════════════════════
  # Whittaker Family — fond biomes + points interactifs fusionnés en un seul plotly
  output$whitplotFamily <- renderPlotly({
    cover_whit <- data_clim_reactive()
    req(!is.null(cover_whit))

    data_sub <- cover_whit %>%
      dplyr::filter(family == input$family,
                    !is.na(temperature), !is.na(precipitation),
                    is.finite(temperature), is.finite(precipitation)) %>%
      dplyr::mutate(code_garden = as.character(code_garden))

    req(nrow(data_sub) > 0)
    valid_levels <- intersect(family_levels, unique(data_sub$code_garden))

    p <- plotbiomes::whittaker_base_plot() +
      geom_point(
        data = data_sub,
        aes(x = temperature, y = precipitation,
            color = code_garden,
            text  = paste0(
              "<b>", ifelse(is.na(species), "unknown", species), "</b><br>",
              "Garden: ", replacement_mapping[code_garden], "<br>",
              "Temp: ", round(temperature, 1), " °C<br>",
              "Precip: ", round(precipitation, 0), " mm"
            )),
        size = 2, alpha = 0.85
      ) +
      scale_color_manual(
        name   = "Garden",
        values = color_values[valid_levels],
        labels = labels[valid_levels],
        breaks = valid_levels,
        drop   = TRUE
      ) +
      coord_cartesian(xlim = c(-15, 30), ylim = c(-5, 450)) +
      labs(x = "Mean annual temperature (°C)",
           y = "Mean annual precipitation (cm)") +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "white", color = NA),
            legend.title = element_text(size = 12),
            legend.text  = element_text(size = 10))

    ggplotly(p, tooltip = "text") %>%
      plotly::layout(legend = list(tracegroupgap = 10))
  })

  output$dlwhitplotFamily <- downloadHandler(
    filename = function() paste0("Whittaker_", input$family, ".jpg"),
    content  = function(file) {
      cover_whit <- data_clim_reactive()
      req(!is.null(cover_whit))
      data_sub <- cover_whit %>%
        dplyr::filter(family == input$family,
                      !is.na(temperature), !is.na(precipitation),
                      is.finite(temperature), is.finite(precipitation))
      valid_levels <- intersect(family_levels, unique(as.character(data_sub$code_garden)))
      p_dl <- plotbiomes::whittaker_base_plot() +
        geom_point(data = data_sub,
                   aes(x = temperature, y = precipitation, color = code_garden),
                   size = 2, shape = 16, alpha = 0.8) +
        scale_color_manual(name = "Garden", values = color_values[valid_levels],
                           labels = labels[valid_levels], breaks = valid_levels) +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "white", color = NA),
              legend.title = element_text(size = 14), legend.text = element_text(size = 12))
      ggsave(file, p_dl, device = "jpg", width = 14, height = 10)
    }
  )

  # ════════════════════════════════════════════════════════════════════════════
  # SPECIES DISTRIBUTION MAP
  # ════════════════════════════════════════════════════════════════════════════
  observe({
    updateSelectInput(session, "GPS_family", choices = sort(unique(all_species_taxo$family)))
  })
  filtered_taxo <- reactive({
    req(!is.null(input$GPS_family), nzchar(input$GPS_family))
    all_species_taxo %>% dplyr::filter(family == input$GPS_family)
  })
  observeEvent(input$GPS_family, {
    updateSelectInput(session, "GPS_genus",
                      choices = c("", sort(unique(filtered_taxo()$genus))))
  })
  observeEvent(input$GPS_genus, {
    req(input$GPS_genus != "")
    updateSelectInput(session, "GPS_species",
                      choices = c("", sort(unique(
                        filtered_taxo() %>%
                          dplyr::filter(genus == input$GPS_genus) %>%
                          dplyr::pull(species)
                      ))))
  })

  selected_species <- reactiveVal(character(0))
  observeEvent(input$addSpecies, {
    req(input$GPS_species)
    selected_species(unique(c(selected_species(), input$GPS_species)))
  })
  observeEvent(input$clearSelection, { selected_species(character(0)) })

  output$selected_species_ui <- renderUI({
    sp <- selected_species()
    if (length(sp) == 0) tags$p("No species selected yet.")
    else tags$ul(lapply(sp, tags$li))
  })

  observeEvent(input$goButton, {
    req(nrow(filtered_taxo()) > 0)
    family_map <- filtered_taxo()
    sp_sel <- selected_species()
    if (length(sp_sel) > 0) family_map <- dplyr::filter(family_map, species %in% sp_sel)

    all_gps_data <- data.frame()
    withProgress(message = "Downloading occurrence data…", value = 0, {
      n <- nrow(family_map)
      for (i in seq_len(n)) {
        tryCatch({
          sp_name <- family_map$species[i]
          # iNaturalist
          inat <- rinat::get_inat_obs(query = sp_name, maxresults = 100) %>%
            dplyr::filter(quality_grade == "research", captive_cultivated == "false") %>%
            dplyr::select(longitude, latitude) %>%
            dplyr::mutate(Source = "iNaturalist", species_gps = sp_name)
          # GBIF
          gbif_raw <- rgbif::occ_data(scientificName = sp_name, hasCoordinate = TRUE, limit = 100)
          if (!is.null(gbif_raw$data)) {
            gbif_sp <- SpatialPointsDataFrame(
              coords = gbif_raw$data[, c("decimalLongitude","decimalLatitude")],
              data   = gbif_raw$data,
              proj4string = CRS("+proj=longlat +datum=WGS84")
            )
            gbif_sp <- spTransform(gbif_sp, CRS("+init=epsg:4326"))
            gbif <- data.frame(longitude = coordinates(gbif_sp)[,1],
                               latitude  = coordinates(gbif_sp)[,2],
                               Source = "GBIF", species_gps = sp_name) %>%
              dplyr::distinct(longitude, latitude, .keep_all = TRUE)
          } else gbif <- data.frame()
          all_gps_data <- dplyr::bind_rows(all_gps_data, inat, gbif)
        }, error = function(e) NULL)
        incProgress(1/n)
      }
    })

    output$map <- renderLeaflet({
      pal <- colorFactor("viridis", domain = all_gps_data$species_gps)
      leaflet(all_gps_data) %>% addTiles() %>%
        addCircleMarkers(~longitude, ~latitude, color = ~pal(species_gps),
                         fillOpacity = 0.7, radius = 5, stroke = FALSE) %>%
        addLegend("bottomright", pal = pal, values = ~species_gps,
                  title = "Species", opacity = 1)
    })

    p_map <- ggplot() +
      geom_polygon(data = world, aes(x = long, y = lat, group = group),
                   fill = "lightgray", color = "white") +
      geom_point(data = all_gps_data,
                 aes(x = longitude, y = latitude, color = species_gps), size = 1.5) +
      scale_color_viridis_d() +
      labs(x = "Longitude", y = "Latitude", color = "Species") +
      theme_minimal() +
      coord_fixed(1.2, xlim = c(-180, 180), ylim = c(-90, 90))

    output$mapsSimple    <- renderPlot({ print(p_map) })
    output$downloaddistrib <- downloadHandler(
      filename = function() paste0("Distribution_", Sys.Date(), ".jpg"),
      content  = function(file) ggsave(file, p_map, device = "jpg", width = 40, height = 30,
                                       units = "in", limitsize = FALSE)
    )
  })

  # ════════════════════════════════════════════════════════════════════════════
  # SPECIES SELECTION (quick search)
  # ════════════════════════════════════════════════════════════════════════════
  cover_species_search <- local({
    cs <- cover_species_garden_full %>%
      dplyr::mutate(across(where(is.character), ~iconv(.x, to = "UTF-8", sub = "")),
                    garden = replacement_mapping[garden]) %>%
      dplyr::distinct(species, garden, .keep_all = TRUE) %>%
      dplyr::select(species, genus, family, garden, pres) %>%
      dplyr::rename(`individual available` = pres)

    all_sp <- all_species_taxo %>%
      dplyr::mutate(garden = "NA", pres = 0L) %>%
      dplyr::rename(`individual available` = pres)

    dplyr::bind_rows(all_sp, cs)
  })

  observe({
    updateSelectInput(session, "selected_family", choices = sort(unique(cover_species_search$family)))
  })
  observe({
    req(input$selected_family != "")
    updateSelectInput(session, "selected_genus",
                      choices = c("", sort(unique(
                        cover_species_search$genus[cover_species_search$family == input$selected_family]
                      ))))
  })
  observe({
    req(input$selected_family != "", input$selected_genus != "")
    updateSelectInput(session, "selected_species",
                      choices = c("", sort(unique(
                        cover_species_search$species[
                          cover_species_search$family == input$selected_family &
                          cover_species_search$genus  == input$selected_genus
                        ]
                      ))))
  })

  select_species_rv <- reactive({
    df <- cover_species_search
    if (!is.null(input$selected_family) && nzchar(input$selected_family))
      df <- df %>% dplyr::filter(family == input$selected_family)
    if (!is.null(input$selected_genus)  && nzchar(input$selected_genus))
      df <- df %>% dplyr::filter(genus  == input$selected_genus)
    if (!is.null(input$selected_species)&& nzchar(input$selected_species))
      df <- df %>% dplyr::filter(species == input$selected_species)
    df
  })
  output$selectedData       <- renderTable({ select_species_rv() })
  output$downloadTablespecies <- downloadHandler(
    filename = function() paste0("species_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(select_species_rv(), file, row.names = FALSE)
  )

  # ════════════════════════════════════════════════════════════════════════════
  # DBGI — API + données jardins
  # ════════════════════════════════════════════════════════════════════════════
  field_data <- reactive({
    res <- httr::GET("https://emi-collection.unifr.ch/directus/items/Field_Data?limit=10000")
    if (httr::status_code(res) != 200) {
      showNotification(paste("API error:", httr::status_code(res)), type = "error")
      return(NULL)
    }
    jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))$data
  })

  list_fr_rv <- reactive({
    read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/botanical_garden_list/list_fribourg.csv"), sep = ",") %>%
      dplyr::select(ipen, secteur, idTaxon, matched_name) %>%
      dplyr::mutate(idTaxon = sapply(strsplit(trimws(idTaxon), "\\s+"),
                                     function(x) paste(head(x, 2), collapse = " ")))
  })

  list_neu_rv <- reactive({
    neu2024 <- read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/botanical_garden_list/list_neu_2024.csv"), sep = ";")
    neu2023 <- read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/botanical_garden_list/list_neu_2023.csv"), sep = ";")
    neu2023$years <- 2023; neu2024$years <- 2024
    df <- dplyr::bind_rows(neu2023, neu2024)
    colnames(df) <- gsub("[^a-z0-9_]", "",
                         iconv(
                           gsub("_+", "_", gsub("[éè]", "e",
                                gsub(" ", "_", gsub("\\.", " ", tolower(colnames(df)))))),
                           "latin1", "ASCII", sub = ""))
    df$numero_de_specimen_cultive <- substr(df$numero_de_specimen_cultive, 1, 8)
    df <- df %>%
      dplyr::mutate(across(c(groupe, sous_groupe, genre, espece),
                           ~iconv(.x, to = "UTF-8", sub = "byte")),
                    species      = tolower(paste(genre, espece)),
                    groupe       = tolower(groupe)      %>% gsub(" ","_",.) %>% gsub("'","",.) %>% stringi::stri_trans_general("Latin-ASCII"),
                    sous_groupe  = tolower(sous_groupe) %>% gsub(" ","_",.) %>% gsub("'","",.) %>% stringi::stri_trans_general("Latin-ASCII")) %>%
      dplyr::mutate(sous_groupe = dplyr::coalesce(sous_groupe, groupe)) %>%
      dplyr::select(code_ipen, famille, genre, espece, groupe, sous_groupe, years, species) %>%
      dplyr::distinct(code_ipen, .keep_all = TRUE)
  })

  list_ch_rv <- reactive({
    read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/botanical_garden_list/list_champex.csv"), sep = ";") %>%
      dplyr::select(Famille, Genre_nouveau, Sps_nouveau) %>%
      dplyr::rename(Family = Famille, Genus = Genre_nouveau, Species = Sps_nouveau)
  })

  # Helpers de merge DBGI
  prep_field <- function(d, project_code) {
    d %>%
      dplyr::filter(grepl("dbgi", sample_id, ignore.case = TRUE),
                    qfield_project == project_code) %>%
      dplyr::select(taxon_name, sample_name, sample_id, x_coord, y_coord, qfield_project) %>%
      dplyr::mutate(
        taxon_name = ifelse(is.na(taxon_name), "", taxon_name),
        sample_name= ifelse(is.na(sample_name), "", sample_name),
        taxon_name = sapply(strsplit(trimws(paste(taxon_name, sample_name)), "\\s+"),
                            function(x) paste(head(x, 2), collapse = " ")),
        taxon_name = tolower(gsub("[^a-z0-9 ]", "", taxon_name))
      ) %>%
      dplyr::filter(nzchar(taxon_name))
  }

  jbuf_merged <- reactive({
    req(field_data()); d <- prep_field(field_data(), "jbuf")
    fr <- list_fr_rv() %>%
      dplyr::mutate(idTaxon = tolower(gsub("[^a-z0-9 ]", "", trimws(idTaxon))))
    merge(d, fr, by.x = "taxon_name", by.y = "idTaxon", all.x = TRUE) %>%
      dplyr::distinct(sample_id, .keep_all = TRUE)
  })
  jbn_merged <- reactive({
    req(field_data()); d <- prep_field(field_data(), "jbn") %>%
      dplyr::mutate(taxon_name = gsub("_", " ", taxon_name))
    merge(d, list_neu_rv(), by.x = "taxon_name", by.y = "species", all.x = TRUE)
  })
  jbc_merged <- reactive({
    req(field_data()); d <- prep_field(field_data(), "jbc")
    ch <- list_ch_rv() %>%
      dplyr::mutate(idTaxon = tolower(gsub("[^a-z0-9 ]", "",
                      iconv(paste(Genus, Species), from = "latin1", to = "UTF-8", sub = ""))))
    merge(d, ch, by.x = "taxon_name", by.y = "idTaxon", all.x = TRUE) %>%
      dplyr::distinct(sample_id, .keep_all = TRUE)
  })

  make_sf <- function(df, filter_sample_id, filter_taxon_name) {
    df <- df[!is.na(df$x_coord) & !is.na(df$y_coord) & df$x_coord != "" & df$y_coord != "", ]
    if (nzchar(filter_sample_id))  df <- df[grepl(filter_sample_id,  df$sample_id,  ignore.case = TRUE), ]
    if (nzchar(filter_taxon_name)) df <- df[grepl(filter_taxon_name, df$taxon_name, ignore.case = TRUE), ]
    sf::st_transform(sf::st_as_sf(df, coords = c("x_coord","y_coord"), crs = 2056, remove = FALSE), crs = 4326)
  }

  jbuf_sf <- reactive({ make_sf(jbuf_merged(), input$filter_sample_id, input$filter_taxon_name) })
  jbn_sf  <- reactive({ make_sf(jbn_merged(),  input$filter_sample_id, input$filter_taxon_name) })
  jbc_sf  <- reactive({ make_sf(jbc_merged(),  input$filter_sample_id, input$filter_taxon_name) })

  render_dt <- function(rv) {
    DT::renderDT({
      df <- rv()
      if (nzchar(input$filter_sample_id))  df <- df[grepl(input$filter_sample_id,  df$sample_id,  ignore.case=TRUE), ]
      if (nzchar(input$filter_taxon_name)) df <- df[grepl(input$filter_taxon_name, df$taxon_name, ignore.case=TRUE), ]
      datatable(df, options = list(pageLength = 10, scrollX = TRUE))
    })
  }
  output$table_jbuf <- render_dt(jbuf_merged)
  output$table_jbn  <- render_dt(jbn_merged)
  output$table_jbc  <- render_dt(jbc_merged)

  dl_csv <- function(rv, name) {
    downloadHandler(filename = function() paste0(name, "_", Sys.Date(), ".csv"),
                    content  = function(file) write.csv(rv(), file, row.names = FALSE))
  }
  output$download_jbuf <- dl_csv(jbuf_merged, "jbuf")
  output$download_jbn  <- dl_csv(jbn_merged,  "jbn")
  output$download_jbc  <- dl_csv(jbc_merged,  "jbc")

  render_leaf <- function(sf_rv, color) {
    renderLeaflet({
      sf <- sf_rv(); req(sf)
      leaflet(sf) %>% addTiles() %>%
        addCircleMarkers(radius = 4, color = color, stroke = FALSE, fillOpacity = 0.7,
                         popup = ~paste0("<b>Sample ID:</b> ", sample_id,
                                         "<br><b>Taxon:</b> ", taxon_name)) %>%
        addScaleBar(position = "bottomleft")
    })
  }
  output$leaflet_jbuf <- render_leaf(jbuf_sf, "blue")
  output$leaflet_jbn  <- render_leaf(jbn_sf,  "darkgreen")
  output$leaflet_jbc  <- render_leaf(jbc_sf,  "blue")

  # ── Sampling Progress ────────────────────────────────────────────────────
  progress_data <- reactive({
    req(list_neu_rv(), list_ch_rv(), list_fr_rv(), jbn_merged(), jbc_merged(), jbuf_merged())
    tibble::tibble(
      garden  = c("jbn","jbc","jbuf"),
      total   = c(nrow(list_neu_rv()), nrow(list_ch_rv()), nrow(list_fr_rv())),
      sampled = c(nrow(jbn_merged()),  nrow(jbc_merged()),  nrow(jbuf_merged()))
    ) %>% dplyr::mutate(percent = sampled / total * 100)
  })
  output$progress_plot <- renderPlot({
    df <- progress_data()
    ggplot(df, aes(x = garden, y = percent)) +
      geom_col(fill = "orange", width = 0.6) + coord_flip() +
      geom_text(aes(label = paste0(round(percent), "%")), hjust = 1.1, color = "black", size = 5) +
      labs(y = "Individuals sampled (%)") +
      theme_minimal(base_size = 16) +
      theme(legend.position = "none", axis.title.y = element_blank(),
            panel.grid.major.y = element_blank(), panel.grid.minor = element_blank())
  })

  # ════════════════════════════════════════════════════════════════════════════
  # MOST WANTED
  # ════════════════════════════════════════════════════════════════════════════
  make_most_wanted <- function(garden_code, garden_list, species_col, id_col = NULL) {
    reactive({
      result_fam  <- cover_family_garden_full %>% dplyr::filter(code_garden == garden_code)
      result_gen  <- cover_genus_garden_full  %>% dplyr::filter(code_garden == garden_code)
      sp_families <- cover_species_garden_full %>%
        dplyr::filter(family %in% unique(result_fam$family)) %>%
        dplyr::pull(species) %>% unique()
      sp_genera   <- garden_list %>%
        dplyr::filter(.data[[sub(".*\\$","",deparse(substitute(garden_list)))]] %in%  # genus col
                        unique(result_gen$genus)) %>%
        dplyr::pull(!!species_col) %>% unique()
      merged <- dplyr::bind_rows(
        data.frame(species = sp_families, target = "family"),
        data.frame(species = sp_genera,   target = "genus")
      ) %>% dplyr::distinct(species, .keep_all = TRUE) %>%
        dplyr::filter(!is.na(species))

      if (!is.null(id_col)) {
        garden_list %>%
          dplyr::filter(.data[[species_col]] %in% merged$species) %>%
          dplyr::left_join(merged, by = setNames("species", species_col)) %>%
          dplyr::group_by(.data[[id_col]]) %>%
          dplyr::filter(!(dplyr::n() > 1 & target == "genus")) %>%
          dplyr::ungroup()
      } else merged
    })
  }

  # Geneva
  gen_tax <- data.frame(
    species = paste(list_geneve$genre, list_geneve$espece),
    genus   = list_geneve$genre, family = list_geneve$famille, garden = "ge"
  )
  data_geneva <- reactive({
    fam_sp <- cover_species_garden_full %>%
      dplyr::filter(family %in% unique(dplyr::filter(cover_family_garden_full, code_garden=="ge")$family)) %>%
      dplyr::pull(species) %>% unique()
    gen_sp <- gen_tax %>%
      dplyr::filter(genus %in% unique(dplyr::filter(cover_genus_garden_full, code_garden=="ge")$genus)) %>%
      dplyr::pull(species) %>% unique()
    merged <- dplyr::bind_rows(
      data.frame(species = fam_sp, target = "family"),
      data.frame(species = gen_sp, target = "genus")
    ) %>% dplyr::distinct(species, .keep_all = TRUE)
    list_geneve %>%
      dplyr::mutate(species = paste(genre, espece)) %>%
      dplyr::inner_join(merged, by = "species") %>%
      dplyr::group_by(Code.ipen) %>%
      dplyr::filter(!(dplyr::n() > 1 & target == "genus")) %>%
      dplyr::ungroup()
  })

  # Prague
  prague_species <- iconv(list_prague$species, from = "", to = "UTF-8", sub = "")
  prague_family  <- iconv(list_prague$family,  from = "", to = "UTF-8", sub = "")
  pra_tax <- data.frame(
    species = prague_species,
    genus   = sub(" .*", "", prague_species),
    family  = sub(" .*", "", prague_family),
    garden  = "pr"
  ) %>% dplyr::mutate(across(where(is.character), ~iconv(.x, to = "UTF-8", sub = "byte")))

  data_prague <- reactive({
    fam_sp <- cover_species_garden_full %>%
      dplyr::filter(family %in% unique(dplyr::filter(cover_family_garden_full, code_garden=="pr")$family)) %>%
      dplyr::pull(species) %>% unique()
    gen_sp <- pra_tax %>%
      dplyr::filter(genus %in% unique(dplyr::filter(cover_genus_garden_full, code_garden=="pr")$genus)) %>%
      dplyr::pull(species) %>% unique()
    dplyr::bind_rows(
      data.frame(species = fam_sp, target = "family"),
      data.frame(species = gen_sp, target = "genus")
    ) %>% dplyr::distinct(species, .keep_all = TRUE) %>% dplyr::filter(!is.na(species))
  })

  # London
  lon_tax <- data.frame(
    species = list_london$TaxonomicName,
    genus   = list_london$Genus,
    family  = list_london$Family,
    garden  = "lo"
  )
  data_london <- reactive({
    fam_sp <- cover_species_garden_full %>%
      dplyr::filter(family %in% unique(dplyr::filter(cover_family_garden_full, code_garden=="lo")$family)) %>%
      dplyr::pull(species) %>% unique()
    gen_sp <- lon_tax %>%
      dplyr::filter(genus %in% unique(dplyr::filter(cover_genus_garden_full, code_garden=="lo")$genus)) %>%
      dplyr::pull(species) %>% unique()
    merged <- dplyr::bind_rows(
      data.frame(species = fam_sp, target = "family"),
      data.frame(species = gen_sp, target = "genus")
    ) %>% dplyr::distinct(species, .keep_all = TRUE)
    list_london %>%
      dplyr::inner_join(merged, by = c("Accepted.Name" = "species")) %>%
      dplyr::group_by(Catalogue.Number) %>%
      dplyr::filter(!(dplyr::n() > 1 & target == "genus")) %>%
      dplyr::ungroup()
  })

  output$table_mw_geneva <- DT::renderDataTable({ DT::datatable(data_geneva(), options=list(pageLength=10)) })
  output$table_mw_prague <- DT::renderDataTable({ DT::datatable(data_prague(), options=list(pageLength=10)) })
  output$table_mw_london <- DT::renderDataTable({ DT::datatable(data_london(), options=list(pageLength=10)) })

  output$download_table_mw_Geneva <- downloadHandler(
    filename = function() "most_wanted_geneva.csv",
    content  = function(file) write.csv(data_geneva(), file, row.names = FALSE))
  output$download_table_mw_Prague <- downloadHandler(
    filename = function() "most_wanted_prague.csv",
    content  = function(file) write.csv(data_prague(), file, row.names = FALSE))
  output$download_table_mw_London <- downloadHandler(
    filename = function() "most_wanted_london.csv",
    content  = function(file) write.csv(data_london(), file, row.names = FALSE))

} # end server

shinyApp(ui = ui, server = server)
