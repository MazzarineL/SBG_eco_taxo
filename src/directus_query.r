library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(sf)
library(sp)
library(ggplot2)
library(rmapshaper)
library(nngeo)
library(dplyr)
library(RColorBrewer)
library(stringi)
library(stringr)
library(ggspatial)
library(leaflet)
# Effectuer la requête GET
response <- GET("https://emi-collection.unifr.ch/directus/items/Field_Data?limit=10000")

# Vérifier si la requête a réussi
if (status_code(response) == 200) {
  data_raw <- content(response, as = "text", encoding = "UTF-8")
  data_json <- fromJSON(data_raw)
  data <- data_json$data

} else {
  print(paste("Erreur :", status_code(response)))
}


  
  # Filtrer les résultats
  filtered_data <- data %>%
    filter(grepl("dbgi", sample_id, ignore.case = TRUE))

filtered_data$taxon_name <- ifelse(is.na(filtered_data$taxon_name), "", filtered_data$taxon_name)
filtered_data$sample_name <- ifelse(is.na(filtered_data$sample_name), "", filtered_data$sample_name)

filtered_data$taxon_name <- paste(filtered_data$taxon_name, filtered_data$sample_name)

filtered_data$taxon_name <- sapply(
  strsplit(trimws(filtered_data$taxon_name), "\\s+"),
  function(x) paste(head(x, 2), collapse = " ")
)


jbuf <- filtered_data[filtered_data$qfield_project == "jbuf", ]

jbuf <- jbuf[, c("taxon_name", "sample_id", "x_coord", "y_coord", "qfield_project")]


list_fr <- read.csv("D:/Thèse Metabolomique/Thèse Metabolomique/jardin botanique/fri/species_list_croisee_final.csv", sep = ",")
list_fr <- list_fr[, c("ipen", "secteur", "idTaxon", "matched_name")]

list_fr$idTaxon <- sapply(
  strsplit(trimws(list_fr$idTaxon), "\\s+"),
  function(x) paste(head(x, 2), collapse = " ")
)

head(list_fr$idTaxon)
jbuf <- jbuf[!is.na(jbuf$taxon_name) & jbuf$taxon_name != "", ]


# Nettoyer jbuf
jbuf$taxon_name <- tolower(jbuf$taxon_name)                          
jbuf$taxon_name <- gsub("[^a-z0-9 ]", "", jbuf$taxon_name)           
jbuf$taxon_name <- trimws(jbuf$taxon_name)                           

# Nettoyer list_fr
list_fr$idTaxon <- tolower(list_fr$idTaxon)
list_fr$idTaxon <- gsub("[^a-z0-9 ]", "", list_fr$idTaxon)
list_fr$idTaxon <- trimws(list_fr$idTaxon)


jbuf_merged <- merge(jbuf, list_fr, by.x = "taxon_name", by.y = "idTaxon", all.x = TRUE)
jbuf_merged <- jbuf_merged[!duplicated(jbuf_merged$sample_id), ]




jbn  <- filtered_data[filtered_data$qfield_project == "jbn", ]
jbn <- jbn[, c("taxon_name", "sample_id", "x_coord", "y_coord", "qfield_project")]

jbn <- jbn[!is.na(jbn$taxon_name) & jbn$taxon_name != "", ]
jbn$taxon_name <- tolower(jbn$taxon_name)                      # 1. minuscules
jbn$taxon_name <- gsub("_", " ", jbn$taxon_name)               # 2. _ → espace
jbn$taxon_name <- trimws(jbn$taxon_name)                       # 3. supprime espaces en début/fin
jbn$taxon_name <- sapply(strsplit(jbn$taxon_name, "\\s+"),     # 4. ne garder que 2 mots
                         function(x) paste(head(x, 2), collapse = " "))

# Nettoyer jbuf
jbn$taxon_name <- tolower(jbn$taxon_name)                          
jbn$taxon_name <- gsub("[^a-z0-9 ]", "", jbn$taxon_name)           
jbn$taxon_name <- trimws(jbn$taxon_name)  


list_neu <- read.csv("D:/plant_area_jbn/cultivatedExport.csv", sep = ";")
list_neu <- list_neu[, c("UID", "Numero.de.specimen.cultive", "Determination.acceptee", "Code.ipen","Groupe","Secteur")]

neu_cult2024 <- readxl::read_excel("D:/plant_area_jbn/cultivatedExport2024.xlsx")
neu_cult2023 <- readxl::read_excel("D:/plant_area_jbn/cultivatedExport2023.xlsx")


neu_cult2023$years <- 2023
neu_cult2024$years <- 2024

list_neu <- rbind(neu_cult2023,neu_cult2024)

#preparation data
colnames(list_neu) <- gsub(" ", "_", colnames(list_neu))
colnames(list_neu) <- tolower(colnames(list_neu))
colnames(list_neu) <- gsub("[éè]", "e", colnames(list_neu))
colnames(list_neu) <- gsub("[^a-z0-9_]", "", iconv(colnames(list_neu), "latin1", "ASCII", sub=""))
list_neu<- data.frame(list_neu)
list_neu$numero_de_specimen_cultive <- substr(list_neu$numero_de_specimen_cultive, 1, 8)

list_neu <- list_neu %>% dplyr::select(code_ipen,famille,genre,espece,groupe,sousgroupe,years)
list_neu$species <- paste(list_neu$genre, list_neu$espece, sep = " ")
list_neu$species <- tolower(list_neu$species)


list_neu$groupe <- tolower(list_neu$groupe)
list_neu$groupe <- gsub(" ", "_", list_neu$groupe)
list_neu$groupe <- gsub("'", "", list_neu$groupe)
list_neu$groupe <- stri_trans_general(list_neu$groupe, "Latin-ASCII")

list_neu$sousgroupe <- tolower(list_neu$sousgroupe)
list_neu$sousgroupe <- gsub(" ", "_", list_neu$sousgroupe)
list_neu$sousgroupe <- gsub("'", "", list_neu$sousgroupe)
list_neu$sousgroupe <- stri_trans_general(list_neu$sousgroupe, "Latin-ASCII")
list_neu$sousgroupe[is.na(list_neu$sousgroupe)] <- list_neu$groupe[is.na(list_neu$sousgroupe)]

list_neu <- list_neu[!duplicated(list_neu$code_ipen), ]

jbn_merged <- merge(jbn, list_neu, by.x = "taxon_name", by.y = "species", all.x = TRUE)












jbuf_merged <- jbuf_merged[!is.na(jbuf_merged$x_coord) &
                          !is.na(jbuf_merged$y_coord) &
                          jbuf_merged$x_coord != "" &
                          jbuf_merged$y_coord != "", ]
# 1. Créer un objet sf avec projection suisse (EPSG:2056)
jbuf_sf <- st_as_sf(jbuf_merged,
                    coords = c("x_coord", "y_coord"),
                    crs = 2056,  # CH1903+ / LV95
                    remove = FALSE)

# 2. Transformer en WGS84 pour affichage sur fond de carte
jbuf_sf <- st_transform(jbuf_sf, crs = 4326)  # WGS84

# 3. Plot des points sur fond de carte simple
leaflet(jbuf_sf) %>%
  addTiles() %>%  # fond OpenStreetMap (Google Maps nécessite API payante)
  addCircleMarkers(
    radius = 4,
    color = "blue",
    stroke = FALSE,
    fillOpacity = 0.7,
    popup = ~paste0("<b>Sample ID:</b> ", sample_id, "<br>",
                    "<b>Taxon:</b> ", taxon_name)
  ) %>%
  addScaleBar(position = "bottomleft")

















jbn_merged <- jbn_merged[!is.na(jbn_merged$x_coord) & !is.na(jbn_merged$y_coord) &
                        jbn_merged$x_coord != "" & jbn_merged$y_coord != "", ]



jbn_sf <- st_as_sf(jbn_merged,
                   coords = c("x_coord", "y_coord"),
                   crs = 2056,      # système suisse LV95
                   remove = FALSE)

jbn_sf <- st_transform(jbn_sf, crs = 4326)  # passage en lat-lon WGS84


leaflet(jbn_sf) %>%
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






















# Effectuer la requête GET
response <- GET("https://emi-collection.unifr.ch/directus/items/Field_Data?limit=10000")

# Vérifier si la requête a réussi
if (status_code(response) == 200) {
  data_raw <- content(response, as = "text", encoding = "UTF-8")
  data_json <- fromJSON(data_raw)
  data <- data_json$data

} else {
  print(paste("Erreur :", status_code(response)))
}


  
  # Filtrer les résultats
  filtered_data <- data %>%
    filter(grepl("dbgi", sample_id, ignore.case = TRUE))







library(tidyr)
library(dplyr)

advance <- data.frame(
  row.names = c("all", "sampled"),
  jbn = c(nrow(list_neu), nrow(jbn_merged)),
  jbuf = c(nrow(list_fr), nrow(jbuf_merged))
)

advance_long <- advance %>%
  tibble::rownames_to_column("status") %>%
  pivot_longer(cols = -status, names_to = "garden", values_to = "count")






library(shiny)
library(shinyWidgets)
library(tibble)
library(shiny)
library(ggplot2)
library(dplyr)

# Données
advance_long <- tibble::tribble(
  ~status,  ~garden, ~count,
  "all",     "jbn",     2230,
  "all",     "jbuf",    5225,
  "sampled", "jbn",     1634,
  "sampled", "jbuf",    2364
)

advance <- data.frame(
  row.names = c("all", "sampled"),
  jbn = c(nrow(list_neu), nrow(jbn_merged)),
  jbuf = c(nrow(list_fr), nrow(jbuf_merged))
)
# Préparation : calcul pour centrer sur 100% max (all) et sampled en proportion
advance_long <- advance_long %>%
  group_by(garden) %>%
  mutate(max_count = max(count),
         prop = count / max_count * 100) %>%
  ungroup()

ui <- fluidPage(
  titlePanel("Stacked Barplot style Barres de Chargement"),
  mainPanel(
    plotOutput("progress_plot", height = "300px")
  )
)

server <- function(input, output, session) {
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

shinyApp(ui, server)
































library(shiny)
library(shinydashboard)
library(httr)
library(jsonlite)
library(dplyr)
library(sf)
library(leaflet)
library(stringi)
library(readxl)
library(DT)  # Pour afficher les data tables

ui <- dashboardPage(
  dashboardHeader(title = "DBGI App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("DBGI", icon = icon("database"),
               menuSubItem("Data Frame", tabName = "data_frame", icon = icon("table")),
               menuSubItem("Botanical Garden Map", tabName = "bot_map", icon = icon("map-location-dot")),
               menuSubItem("Sampling", tabName = "sample", icon = icon("flask"))
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "data_frame",
              h2("Data Frames fusionnés"),
              h3("jbuf_merged"),
              DTOutput("table_jbuf"),
              hr(),
              h3("jbn_merged"),
              DTOutput("table_jbn")
      ),
      tabItem(tabName = "bot_map",
              h2("Cartes Botanique"),
              tabsetPanel(
                tabPanel("Carte jbuf", leafletOutput("leaflet_jbuf", height = 600)),
                tabPanel("Carte jbn", leafletOutput("leaflet_jbn", height = 600))
              )
      ),
      tabItem(tabName = "sample",
              h2("Onglet Sampling"),
              p("À compléter...")
      )
    )
  )
)

server <- function(input, output, session) {
  
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
    read.csv("D:/Thèse Metabolomique/Thèse Metabolomique/jardin botanique/fri/species_list_croisee_final.csv", sep = ",") %>%
      select(ipen, secteur, idTaxon, matched_name) %>%
      mutate(idTaxon = sapply(strsplit(trimws(idTaxon), "\\s+"), function(x) paste(head(x, 2), collapse = " ")))
  })
  
  list_neu <- reactive({
    neu_cult2024 <- readxl::read_excel("D:/plant_area_jbn/cultivatedExport2024.xlsx")
    neu_cult2023 <- readxl::read_excel("D:/plant_area_jbn/cultivatedExport2023.xlsx")
    
    neu_cult2023$years <- 2023
    neu_cult2024$years <- 2024
    
    df <- bind_rows(neu_cult2023, neu_cult2024)
    
    colnames(df) <- gsub(" ", "_", colnames(df))
    colnames(df) <- tolower(colnames(df))
    colnames(df) <- gsub("[éè]", "e", colnames(df))
    colnames(df) <- gsub("[^a-z0-9_]", "", iconv(colnames(df), "latin1", "ASCII", sub=""))
    
    df$numero_de_specimen_cultive <- substr(df$numero_de_specimen_cultive, 1, 8)
    
    df <- df %>%
      select(code_ipen, famille, genre, espece, groupe, sousgroupe, years) %>%
      mutate(
        species = tolower(paste(genre, espece)),
        groupe = tolower(groupe) %>% gsub(" ", "_", .) %>% gsub("'", "", .) %>% stri_trans_general("Latin-ASCII"),
        sousgroupe = tolower(sousgroupe) %>% gsub(" ", "_", .) %>% gsub("'", "", .) %>% stri_trans_general("Latin-ASCII")
      )
    
    df$sousgroupe[is.na(df$sousgroupe)] <- df$groupe[is.na(df$sousgroupe)]
    
    df <- df[!duplicated(df$code_ipen), ]
    df
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
  
}

shinyApp(ui, server)
