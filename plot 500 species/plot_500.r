source("D:/gitrepo/SBG_eco_taxo/src/plot 500 species/cover_garden_500.R")

library(ggplot2)
library(RColorBrewer)
library(scales)

### Genus ###

# Number of unique categories (including NA if present)
n_colors <- length(unique(garden_counts$code_garden))

# Custom palette (using hue_pal to generate n distinct colors)
color_palette <- hue_pal()(n_colors)

# If NA present → add grey for NA
if (any(is.na(garden_counts$code_garden))) {
  color_palette <- c("grey80", color_palette)
}

# Clean data: convert empty or blank codes to NA
taxonomy_merge_clean <- taxonomy_merge %>%
  mutate(code_garden = na_if(trimws(code_garden), ""))

# Count after cleaning
garden_counts <- taxonomy_merge_clean %>%
  count(code_garden, name = "count") %>%
  mutate(code_garden = as.factor(code_garden))

# Special color for NA
na_color <- "grey80"

# Barplot with NA in grey
plot1 <- ggplot(garden_counts, aes(x = code_garden, y = count, fill = code_garden)) +
  geom_col() +
  scale_fill_manual(values = color_palette, na.value = na_color) +
  labs(title = "Total number of occurrences per code_garden",
       x = "Code Garden",
       y = "Number of occurrences") +
  theme_minimal() +
  theme(legend.position = "none")

# Pie chart
plot2 <- ggplot(garden_counts, aes(x = "", y = count, fill = code_garden)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  scale_fill_manual(values = color_palette, na.value = na_color) +
  labs(title = "Distribution of occurrences by code_garden") +
  theme_void() +
  theme(legend.title = element_blank())



##### Family ###

# Clean data: trim spaces and convert empty codes to NA
taxonomy_merge_clean <- taxonomy_merge %>%
  mutate(
    code_garden = trimws(code_garden),             # remove spaces
    code_garden = ifelse(code_garden == "", NA, code_garden)  # replace "" with NA
  )

# Count unique families per garden
family_counts <- taxonomy_merge_clean %>%
  distinct(code_garden, family) %>%
  count(code_garden, name = "count") %>%
  mutate(code_garden = as.factor(code_garden))

# Dynamic colors
n_colors <- length(unique(family_counts$code_garden))
color_palette <- hue_pal()(n_colors)
if (any(is.na(family_counts$code_garden))) {
  color_palette <- c(color_palette, "grey80")
}

# Barplot
plot3 <- ggplot(family_counts, aes(x = code_garden, y = count, fill = code_garden)) +
  geom_col() +
  scale_fill_manual(values = color_palette, na.value = na_color) +
  labs(title = "Number of unique families per code_garden",
       x = "Code Garden",
       y = "Number of families") +
  theme_minimal() +
  theme(legend.position = "none")

# Pie chart of families per garden
plot4 <- ggplot(family_counts, aes(x = "", y = count, fill = code_garden)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  scale_fill_manual(values = color_palette, na.value = na_color) +
  labs(title = "Distribution of families by garden") +
  theme_void() +
  theme(legend.title = element_blank())

# Save plots as PNG (you probably want different file names for each plot)
ggsave("D:/gitrepo/SBG_eco_taxo/inst/coverage family genus plot result/occurrences_per_code_garden_barplot.png", plot = plot1,
       width = 8, height = 6, dpi = 300)
ggsave("D:/gitrepo/SBG_eco_taxo/inst/coverage family genus plot result/occurrences_per_code_garden_piechart.png", plot = plot2,
       width = 8, height = 6, dpi = 300)
ggsave("D:/gitrepo/SBG_eco_taxo/inst/coverage family genus plot result/unique_families_per_code_garden_barplot.png", plot = plot3,
       width = 8, height = 6, dpi = 300)
ggsave("D:/gitrepo/SBG_eco_taxo/inst/coverage family genus plot result/families_distribution_piechart.png", plot = plot4,
       width = 8, height = 6, dpi = 300)
