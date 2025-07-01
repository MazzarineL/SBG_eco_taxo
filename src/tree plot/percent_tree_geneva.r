barplot_data <- read.csv("D:/gitrepo/SBG_eco_taxo/data/cover_species_garden_500.csv", sep =",")
unique(barplot_data$family)
283 famille 

unique(barplot_data$genus)
2512 genre 



taxo_full <- read.csv("D:/gitrepo/SBG_eco_taxo/data/taxonomy_family_genus_full.csv", sep =",")
unique(taxo_full$name)
2512 / 12793 genre total


unique(taxo_full$name.family)
283/ 508 famille







unique_name_counts <- taxo_full %>%
  group_by(name.family) %>%
  summarise(unique_name_count = n_distinct(name))


unique_name <- barplot_data %>%
  group_by(family) %>%
  summarise(unique_name_obt = n_distinct(genus))


# Rename `name.family` to `family` for consistency
unique_name_counts <- unique_name_counts %>%
  rename(family = name.family)

# Perform a left join to include all rows from unique_name_counts
merged_data <- unique_name_counts %>%
  left_join(unique_name, by = "family") %>%
  # Replace NA in unique_name_obt with 0
  mutate(unique_name_obt = ifelse(is.na(unique_name_obt), 0, unique_name_obt)) %>%
  # Calculate the percentage
  mutate(percentage_obt = (unique_name_obt / unique_name_count) * 100)

# Conservez uniquement les colonnes family et percentage
df_svl <- merged_data %>%
  select(family, percentage_obt)
  df_svl <- data.frame(df_svl)


df_svl <- df_svl %>% filter(!is.na(family))
df_svl <- df_svl[!df_svl$family %in% list_delete, ]
rownames(df_svl) <- df_svl$family

df_svl <- df_svl %>%
  select(-family)



cover_genus_garden_full <- read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_app/main/data/cover_genus_garden_500.csv") )
cover_genus_garden_full <- cover_genus_garden_full[!is.na(cover_genus_garden_full$ott_id.family), ]
tr <- rotl::tol_induced_subtree(ott_ids = cover_genus_garden_full$ott_id.family)




###appeler la colonne div dans mu une fois els données de chemodiv obtenues


#modification de l'arbre et du df
tr <- as.phylo(tr)


sp_name <- gsub("_.*", "", tr$tip.label)
    tr$tip.label <- sp_name


setdiff(rownames(df_svl), tr$tip.label)


mrca_to_delete <- setdiff(tr$tip.label,rownames(df_svl))


# Ensure labels are in character format
mrca_to_delete <- as.character(mrca_to_delete)

# Remove tips from the tree if they exist in the tree's labels
labels_to_remove <- intersect(mrca_to_delete, tr$tip.label)
tr <- drop.tip(tr, labels_to_remove)


#rajout longueur de branches a l'arbre

tree_anole <- as.phylo(tr)

tree_anole$edge.length <- rep(2,length(tree_anole$edge[,1]))
tree_anole$tip.label



library(phytools)


#creation du df pour colorer les branches
svl <- as.matrix(df_svl)[,1]
fit <- phytools::fastAnc(tree_anole, svl, vars=TRUE, CI=TRUE)

td <- data.frame(node = nodeid(tree_anole, names(svl)),
               trait = svl)
nd <- data.frame(node = names(fit$ace), trait = fit$ace)

d <- rbind(td, nd)
d$node <- as.numeric(d$node)

#plot de l'arbre
tree_anole <- as_tibble(tree_anole)

d <- tibble(d)
d$trait2 <- rank(d$trait)
tree <- full_join(tree_anole, d, by = 'node')


tree <- as.treedata(tree)

#create color gradient for tree branch
circ <- ggtree(tree, layout = 'circular', branch.length = 'none', size = 1) +
  aes(color = trait) +
  scale_colour_gradient2(
    low = "red",
    mid = "orange",
    high = "green",
    midpoint = 30,  
    guide = "colorbar"
  ) +
  labs(color = "Percentage of genus available per family") +  
  theme(
    legend.position = 'bottom', 
    legend.key.width = unit(1, "cm"),
    legend.title = element_text(size = 20),  # Adjust size here
    legend.text = element_text(size = 20)    # Optional: Adjust text size here
  ) +
  geom_tiplab(aes(label = label), size = 3, align = TRUE, linetype = "dotted", linesize = 0.5, offset = 0.1)

print(circ)



ggsave(filename = "tree_gen_percent.pdf", plot = circ, device = "pdf", width = 50, height = 50, units = "cm")




