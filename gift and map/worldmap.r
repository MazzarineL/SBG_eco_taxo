source("D:/gitrepo/SBG_eco_taxo/src/preparation data/format_data.R")
#source("./src/format_data.R")
#source("./src/niche_optimum.R")
#very long unecessary if data_env_gift_complet and data_env_restant available

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



whit_part1 <- read.csv("D:/Thèse Metabolomique/Thèse Metabolomique/jardin botanique/data_env_gift_complet.csv",sep=",")
whit_part2 <- read.csv("D:/Thèse Metabolomique/Thèse Metabolomique/jardin botanique/data_env_restant.csv",sep=";")



world_dis_1 <- whit_part1[, c(3,4,14)]
world_dis_2 <- whit_part2[, c(1,2,3)]

world_dis_1 <- world_dis_1 %>%
  dplyr::select(species, longitude, latitude)


world_dis <- rbind(world_dis_1, world_dis_2)  

fix(world_dis)
summary(dn)

world_dis$longitude <- as.numeric(world_dis$longitude)
world_dis$latitude <- as.numeric(world_dis$latitude)
world_dis$species <- as.factor(world_dis$species)



dn <- na.omit(world_dis)

  sf_points <- data.frame(
    lat = as.numeric(c(dn$latitude,-85.00294,85.05113)),
    lon = as.numeric(c(dn$longitude,-180,180))
  ) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Creating an empty grid with value 1
empty_grid <- sf::st_bbox(sf_points) %>%
  sf::st_as_sfc() %>%
  sf::st_make_grid(cellsize = c(1, 1))  # Adjust cellsize as per your requirement

empty_grid_sf <- sf::st_sf(geometry = empty_grid)

centroids <- sf::st_centroid(empty_grid_sf)

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
colors <- gradient[c(30, 40, 50, 55, 60, 65, 70, 72,74,76,78, 80, 82, 84, 86, 88, 90, 92, 94, 96, 98)]
 colors <-  gradient[c(seq(1, 20, by = 5), seq(20, 50, by = 4), seq(50, 70, by = 2), seq(80, 99, by = 1))]
  
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

