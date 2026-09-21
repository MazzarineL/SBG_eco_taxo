# install.packages(c("shiny", "shinydashboard", "plotly", "leaflet", "curl", "DT"))

library(shiny)
library(shinydashboard)
library(plotly)
library(curl)
library(leaflet)
library(DT)

# Chargé une seule fois au démarrage pour peupler les dropdowns statiques
cover_species_garden_full <- read.csv(
  curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/cover_species_garden.csv")
)

ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(style = "font-size: 18px", "Swiss Botanical Garden")
  ),

  dashboardSidebar(
    tags$style(HTML("
      .filters-section {
        background-color: #008d4c;
        padding: 10px;
        border-radius: 5px;
        margin-bottom: 15px;
      }
      .sidebar-menu { margin-top: 15px; }
    ")),

    sidebarMenu(
      div(class = "filters-section",
        menuItem("Filters", tabName = "filters", icon = icon("sliders"), selected = TRUE),

        tags$h5("Swiss Botanical Gardens"),
        # FIX: un seul checkboxGroupInput "Garden" avec tous les jardins
        checkboxGroupInput(
          inputId = "Garden",
          label   = NULL,
          choices = c(
            "Neuchâtel" = "ne",
            "Fribourg"  = "fr",
            "Lausanne"  = "la",
            "Geneva"    = "ge",
            "Champex"   = "ch",
            "Prague"    = "pr",
            "London"    = "lo"
          )
        ),

        actionButton(inputId = "action", label = "Go!", icon = icon("play"),
                     style = "color: #fff; background-color: #222c32;")
      ),

      menuItem("Phylogenetic", icon = icon("network-wired"),
        menuSubItem("Garden Tree",        tabName = "garden_tree",  icon = icon("tree")),
        menuSubItem("Family Tree",        tabName = "family_tree",  icon = icon("sitemap")),
        menuSubItem("Genus Tree",         tabName = "genus_tree",   icon = icon("sitemap")),
        menuSubItem("Taxonomic Coverage", tabName = "plot_cover",   icon = icon("project-diagram"))
      ),

      menuItem("Biome", icon = icon("globe-americas"),
        menuSubItem("Whittaker Garden Plot", tabName = "whit_garden_plot", icon = icon("chart-line")),
        menuSubItem("Whittaker Family Plot", tabName = "whit_family_plot", icon = icon("chart-pie"))
      ),

      menuItem("Quick Search", icon = icon("magnifying-glass"),
        menuSubItem("Species Selection",         tabName = "species_selection",  icon = icon("leaf")),
        menuSubItem("Species World Distribution",tabName = "species_distribution",icon = icon("globe"))
      ),

      menuItem("DBGI", icon = icon("database"),
        menuSubItem("Data Frame",          tabName = "data_frame",  icon = icon("table")),
        menuSubItem("Botanical Garden Map",tabName = "bot_map",     icon = icon("map-location-dot")),
        menuSubItem("Sampling",            tabName = "sample",      icon = icon("flask")),
        menuSubItem("Most Wanted",         tabName = "most_wanted", icon = icon("exclamation-circle"))
      )
    )
  ),

  dashboardBody(
    tags$head(tags$style(HTML('
      .skin-blue .main-header .logo              { background-color: #008d4c; }
      .skin-blue .main-header .navbar            { background-color: #00a75a; }
      .skin-blue .main-sidebar                   { background-color: #222c32; }
      .skin-blue .main-sidebar .sidebar .sidebar-menu .active a { background-color: #008d4c; }
      .skin-blue .main-sidebar .sidebar .sidebar-menu a          { background-color: #222c32; color: #ffffff; }
      .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover    { background-color: #008d4c; }
      .skin-blue .main-header .navbar .sidebar-toggle:hover      { background-color: #008d4c; }
      .tab-pane.active .box            { border-color: #00a75a; }
      .tab-pane.active .box .box-header{ background-color: #00a75a; color: #ffffff; }
      .tab-pane.active .btn-primary    { background-color: #00a75a; border-color: #00a75a; }
      .content   { background-color: transparent; padding: 0; margin: 0; }
      h1         { margin-top: 10px; margin-bottom: 10px; }
      .tab-pane  { background-color: #ffffff; padding: 10px; min-height: 100vh; width: 100% !important; margin: 0; }
      .help-block{ background-color: #ffffff; font-size: 15px; color: #333; }
      .dataTables_wrapper { margin-left: 20px; margin-right: 20px; }
    '))),

    tabItems(

      # ── FILTERS (page d'accueil) ───────────────────────────────────────────
      tabItem(tabName = "filters",
        tags$div(
          style = "position:fixed;top:0;left:0;right:0;bottom:0;
                   background-image:url('bg.jpg');background-size:cover;
                   background-position:center;padding:40px;
                   display:flex;flex-direction:column;justify-content:center;
                   align-items:center;text-align:center;",
          tags$div(
            style = "background-color:rgba(255,255,255,0.8);padding:30px;
                     border-radius:10px;max-width:900px;margin-top:50px;
                     font-weight:bold;font-size:20px;color:#000;",
            "Welcome to the Botanical Garden Coverage Application.",
            tags$br(), tags$br(),
            "To get started, select one or more gardens in the sidebar.",
            tags$br(), tags$br(),
            "Launch the analysis by clicking the 'Go!' button.",
            tags$br(), tags$br(),
            "Explore the various tabs to view your results. Some pages include multiple graphs — feel free to scroll.",
            tags$br(), tags$br(),
            "For bugs, suggestions, or to include your garden's data, contact: ",
            tags$a(href = "mailto:mazzarine.laboureau@unine.ch",
                   "mazzarine.laboureau@unine.ch",
                   style = "color:#3c8dbc;font-weight:bold;"),
            tags$br(), tags$br(),
            "All data and scripts are available on ",
            tags$a(href = "https://github.com/MazzarineL/SBG_eco_taxo/tree/main", "GitHub.")
          )
        )
      ),

      # ── GARDEN TREE ────────────────────────────────────────────────────────
      tabItem(tabName = "garden_tree",
        tags$h1("Garden Tree"),
        helpText(tags$strong("Phylogenetic tree of all families in the selected gardens.")),
        fluidRow(
          column(12, div(style = "text-align:right;margin-bottom:10px;",
            downloadButton("downloadFullPlot", "Download Garden Tree", class = "btn btn-primary")
          ))
        ),
        fluidRow(plotOutput("treePlot", height = "1500px"))
      ),

      # ── FAMILY TREE ────────────────────────────────────────────────────────
      tabItem(tabName = "family_tree",
        tags$h1("Family Tree"),
        helpText(tags$strong(
          "For the selected family, genera in the selected gardens are shown.
           Blue = priority genera to maximize taxonomic coverage.
           If no blue genera appear, the requested number is too high — try 'Select all'."
        )),
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
            selectInput("family", "Family",
                        choices = sort(unique(cover_species_garden_full$family)), selected = ""),
            sliderInput("genus_select", "Number of genera to select", min = 1, max = 30, value = 5),
            actionButton("actionfamily", "Apply", icon = icon("play"),
                         style = "color:#fff;background-color:#008d4c;border:none;"),
            textOutput("textgenus"),
            DT::dataTableOutput("onlygenus")
          )
        ),
        fluidRow(
          column(12, div(style = "text-align:right;margin-bottom:10px;",
            downloadButton("downloadFamilyPlot", "Download Family Tree", class = "btn btn-primary"),
            downloadButton("downloadTable",      "Download Priority Table", class = "btn btn-primary")
          ))
        ),
        fluidRow(
          plotOutput("FamilyPlot", height = "800px"),
          DT::dataTableOutput("mytable")
        )
      ),

      # ── GENUS TREE ─────────────────────────────────────────────────────────
      tabItem(tabName = "genus_tree",
        tags$h1("Genus Tree"),
        helpText(tags$strong(
          "For the selected genus, species in the selected gardens are shown.
           Blue = priority species to maximize taxonomic coverage."
        )),
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
            selectizeInput("genus", "Genus", choices = NULL, options = list(placeholder = "Select a genus…")),
            sliderInput("species_select", "Number of species to select", min = 1, max = 30, value = 5),
            actionButton("actiongenus", "Apply", icon = icon("play"),
                         style = "color:#fff;background-color:#008d4c;border:none;"),
            textOutput("textspecies"),
            DT::dataTableOutput("onlyspecies")
          )
        ),
        fluidRow(
          column(12, div(style = "text-align:right;margin-bottom:10px;",
            downloadButton("downloadGenusPlot", "Download Genus Tree",    class = "btn btn-primary"),
            downloadButton("downloadTable2",    "Download Priority Table", class = "btn btn-primary")
          ))
        ),
        fluidRow(
          plotOutput("GenusPlot", height = "800px"),
          DT::dataTableOutput("mytableGenus")
        )
      ),

      # ── TAXONOMIC COVERAGE ─────────────────────────────────────────────────
      tabItem(tabName = "plot_cover",
        tags$h1("Taxonomic Coverage"),
        helpText(tags$strong("Family, genus, and species coverage across selected gardens.")),
        fluidRow(column(12, div(style = "text-align:right;margin-bottom:10px;",
          downloadButton("downloadcoverplot", "Download Coverage Plot", class = "btn btn-primary")
        ))),
        fluidRow(plotOutput("coverplot", height = "1000px")),
        fluidRow(column(12, div(style = "text-align:right;margin-bottom:10px;",
          downloadButton("dlvenplot", "Download Venn Plot", class = "btn btn-primary")
        ))),
        fluidRow(plotOutput("vennplot", height = "850px")),
        fluidRow(column(12, div(style = "text-align:right;margin-bottom:10px;",
          downloadButton("dlpiechart", "Download Pie Chart", class = "btn btn-primary")
        ))),
        fluidRow(plotOutput("piechart", height = "600px"))
      ),

      # ── WHITTAKER GARDEN ───────────────────────────────────────────────────
      tabItem(tabName = "whit_garden_plot",
        tags$h1("Whittaker Garden Plot"),
        helpText(tags$strong("Plants from selected gardens placed in biomes by temperature and precipitation.")),
        fluidRow(column(12, div(style = "text-align:right;margin-bottom:10px;",
          downloadButton("dlwhitplot", "Download Whittaker Plot", class = "btn btn-primary")
        ))),
        fluidRow(plotlyOutput("whitplot", height = "1000px"))
      ),

      # ── WHITTAKER FAMILY ───────────────────────────────────────────────────
      tabItem(tabName = "whit_family_plot",
        tags$h1("Whittaker Family Plot"),
        helpText(tags$strong("Climatic niche of the selected family.")),
        fluidRow(column(12, div(style = "text-align:right;margin-bottom:10px;",
          downloadButton("dlwhitplotFamily", "Download Whittaker Family Plot", class = "btn btn-primary")
        ))),
        fluidRow(plotlyOutput("whitplotFamily", height = "1000px"))
      ),

      # ── SPECIES SELECTION ──────────────────────────────────────────────────
      tabItem(tabName = "species_selection",
        tags$h1("Species Selection"),
        helpText(tags$strong("Search a species by family → genus → species.")),
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
            selectInput("selected_family", "Family",
                        choices = sort(unique(cover_species_garden_full$family))),
            selectInput("selected_genus",   "Genus",   choices = c("", NULL)),
            selectInput("selected_species", "Species", choices = c("", NULL)),
            div(style = "text-align:right;margin-bottom:10px;",
              downloadButton("downloadTablespecies", "Download species table", class = "btn btn-primary")
            ),
            tableOutput("selectedData")
          )
        )
      ),

      # ── SPECIES DISTRIBUTION ───────────────────────────────────────────────
      tabItem(tabName = "species_distribution",
        tags$h1("Species Distribution"),
        helpText(tags$strong("Global distribution of selected species (iNaturalist + GBIF).")),
        fluidRow(
          box(width = 6, status = "primary", solidHeader = TRUE,
            selectInput("GPS_family",  "Family",  choices = NULL),
            selectInput("GPS_genus",   "Genus",   choices = c("", NULL)),
            selectInput("GPS_species", "Species", choices = c("", NULL), multiple = TRUE),
            actionButton("addSpecies", "Add to selection")
          ),
          box(width = 6, status = "info", solidHeader = TRUE,
            tags$h4("Selected Species"),
            uiOutput("selected_species_ui"),
            actionButton("clearSelection", "Clear selection", class = "btn btn-warning")
          )
        ),
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE,
            actionButton("goButton", "Go"),
            leafletOutput("map", width = "100%", height = "500px"),
            plotOutput("mapsSimple", height = "1000px"),
            div(style = "text-align:right;margin-top:10px;",
              downloadButton("downloaddistrib", "Download distribution map", class = "btn btn-primary")
            )
          )
        )
      ),

      # ── DATA FRAMES ────────────────────────────────────────────────────────
      tabItem(tabName = "data_frame",
        tags$h1("Data Frames"),
        fluidRow(box(width = 12, status = "primary", solidHeader = TRUE,
          helpText("Fribourg (jbuf) — samples merged with garden list."),
          DT::dataTableOutput("table_jbuf"),
          div(style = "text-align:right;margin-top:10px;",
            downloadButton("download_jbuf", "Download jbuf", class = "btn btn-primary"))
        )),
        fluidRow(box(width = 12, status = "primary", solidHeader = TRUE,
          helpText("Neuchâtel (jbn) — samples merged with garden list."),
          DT::dataTableOutput("table_jbn"),
          div(style = "text-align:right;margin-top:10px;",
            downloadButton("download_jbn", "Download jbn", class = "btn btn-primary"))
        )),
        fluidRow(box(width = 12, status = "primary", solidHeader = TRUE,
          helpText("Champex (jbc) — samples merged with garden list."),
          DT::dataTableOutput("table_jbc"),
          div(style = "text-align:right;margin-top:10px;",
            downloadButton("download_jbc", "Download jbc", class = "btn btn-primary"))
        ))
      ),

      # ── BOTANICAL GARDEN MAP ───────────────────────────────────────────────
      tabItem(tabName = "bot_map",
        tags$h1("Botanical Garden Maps"),
        helpText(tags$strong("GPS locations of collected samples.")),
        fluidRow(
          column(6, textInput("filter_sample_id",  "Filter by Sample ID")),
          column(6, textInput("filter_taxon_name", "Filter by Taxon Name"))
        ),
        fluidRow(
          box(width = 12,
            tabsetPanel(
              tabPanel("Fribourg",  leafletOutput("leaflet_jbuf", height = "800px")),
              tabPanel("Neuchâtel", leafletOutput("leaflet_jbn",  height = "800px")),
              tabPanel("Champex",   leafletOutput("leaflet_jbc",  height = "800px"))
            ),
            div(style = "text-align:right;margin-top:10px;",
              downloadButton("download_map_jbuf", "Download jbuf map", class = "btn btn-primary"),
              downloadButton("download_map_jbn",  "Download jbn map",  class = "btn btn-primary"),
              downloadButton("download_map_jbc",  "Download jbc map",  class = "btn btn-primary")
            )
          )
        )
      ),

      # ── SAMPLING PROGRESS ──────────────────────────────────────────────────
      tabItem(tabName = "sample",
        tags$h1("Sampling Progress"),
        helpText(tags$strong("Collection progress per botanical garden.")),
        fluidRow(plotOutput("progress_plot", height = "600px"))
      ),

      # ── MOST WANTED ────────────────────────────────────────────────────────
      tabItem(tabName = "most_wanted",
        tags$h1("Most Wanted"),
        fluidRow(box(width = 12, status = "primary", solidHeader = TRUE,
          helpText("Priority species to collect — Geneva."),
          DT::dataTableOutput("table_mw_geneva"),
          div(style = "text-align:right;margin-top:10px;",
            downloadButton("download_table_mw_Geneva", "Download Geneva list", class = "btn btn-primary"))
        )),
        fluidRow(box(width = 12, status = "primary", solidHeader = TRUE,
          helpText("Priority species to collect — Prague."),
          DT::dataTableOutput("table_mw_prague"),
          div(style = "text-align:right;margin-top:10px;",
            downloadButton("download_table_mw_Prague", "Download Prague list", class = "btn btn-primary"))
        )),
        fluidRow(box(width = 12, status = "primary", solidHeader = TRUE,
          helpText("Priority species to collect — London (Kew)."),
          DT::dataTableOutput("table_mw_london"),
          div(style = "text-align:right;margin-top:10px;",
            downloadButton("download_table_mw_London", "Download London list", class = "btn btn-primary"))
        ))
      )

    ) # end tabItems
  )   # end dashboardBody
)     # end dashboardPage