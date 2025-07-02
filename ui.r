#install.packages(c("shiny", "shinydashboard", "plotly", "leaflet", "curl"))


library(shiny)
library(shinydashboard)
library(plotly)
library(curl)
library(leaflet)

cover_species_garden_full <- read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/cover_species_garden.csv"))

ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(style = "font-size: 18px", "Cover Botanical Garden")
  ),
  dashboardSidebar(
    tags$style(HTML("
      .filters-section {
        background-color: #008d4c;
        padding: 10px;
        border-radius: 5px;
        margin-bottom: 15px;
      }
      .sidebar-menu {
        margin-top: 15px;
      }
    ")),
sidebarMenu(
  div(class = "filters-section",
      menuItem("Filters", tabName = "filters", icon = icon("sliders"), selected = TRUE),
      checkboxGroupInput(inputId = "Garden", label = "Select Garden",
                         choices = c("Neuchâtel" = "ne", "Fribourg" = "fr", "Lausanne" = "la", "Geneva" = "ge","Champex" = "ch")),
      actionButton(inputId = "action", label = "Go!", icon = icon("play"), style = "color: #fff; background-color: #222c32;")
  ),
  
  menuItem("Phylogenetic", icon = icon("network-wired"),
           menuSubItem("Garden Tree", tabName = "garden_tree", icon = icon("tree")),
           menuSubItem("Family Tree", tabName = "family_tree", icon = icon("sitemap")),
           menuSubItem("Phylogenetic Coverage", tabName = "plot_cover", icon = icon("project-diagram"))
  ),
  
  menuItem("Biome", icon = icon("globe-americas"),
           menuSubItem("Whittaker Garden Plot", tabName = "whit_garden_plot", icon = icon("chart-line")),
           menuSubItem("Whittaker Family Plot", tabName = "whit_family_plot", icon = icon("chart-pie"))
  ),
  
  menuItem("Quick Search", icon = icon("magnifying-glass"),
           menuSubItem("Species Selection", tabName = "species_selection", icon = icon("leaf")),
           menuSubItem("Species World Distribution", tabName = "species_distribution", icon = icon("globe"))
  ),
  
  menuItem("DBGI", icon = icon("database"),
           menuSubItem("Data Frame", tabName = "data_frame", icon = icon("table")),
           menuSubItem("Botanical Garden Map", tabName = "bot_map", icon = icon("map-location-dot")),
           menuSubItem("Sampling", tabName = "sample", icon = icon("flask"))
  )
)
  ),dashboardBody(
  tags$head(tags$style(HTML('
    /* Logo and top bar */
    .skin-blue .main-header .logo {
      background-color: #008d4c; /* Green */
    }
    .skin-blue .main-header .navbar {
      background-color: #00a75a; /* Light Green */
    }
    .skin-blue .main-sidebar {
      background-color: #222c32; /* Dark Green */
    }
    .skin-blue .main-sidebar .sidebar .sidebar-menu .active a {
      background-color: #008d4c;
    }
    .skin-blue .main-sidebar .sidebar .sidebar-menu a {
      background-color: #222c32;
      color: #ffffff;
    }
    .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {
      background-color: #008d4c;
    }
    .skin-blue .main-header .navbar .sidebar-toggle:hover {
      background-color: #008d4c;
    }
    .tab-pane.active .box {
      border-color: #00a75a;
    }
    .tab-pane.active .box .box-header {
      background-color: #00a75a;
      color: #ffffff;
    }
    .tab-pane.active .btn-primary {
      background-color: #00a75a;
      border-color: #00a75a;
    }
  '))),

  tabItems(
    tabItem(tabName = "filters",
fluidRow(
    tags$div(style = "
        background-color: rgba(255, 255, 255, 0.8);
        padding: 40px;
        border-radius: 10px;
        margin: 50px auto;
        width: 90%;
        max-width: 1000px;
        text-align: center;
      ",
      tags$div(style = "font-size: 30px; font-weight: bold; margin-bottom: 20px;",
        "Welcome to the Botanical Garden Coverage Application."
      ),
      tags$div(style = "font-size: 24px; font-weight: bold; margin-bottom: 20px;",
        "To get started, select one or more gardens."
      ),
      tags$div(style = "font-size: 24px; font-weight: bold; margin-bottom: 20px;",
        "Launch the script by clicking the 'Go' button."
      ),
      tags$div(style = "font-size: 24px; font-weight: bold; margin-bottom: 20px;",
        "Explore the various tabs to view your results. Some pages include multiple graphs, so feel free to scroll."
      ),
      tags$div(style = "font-size: 24px; font-weight: bold; margin-bottom: 20px;",
        "If you find a bug or have suggestions to improve the app’s usability, or if you work at a botanical garden and want your data included, please contact me at ",
        tags$a(href = "mailto:mazzarine.laboureau@unine.ch", "mazzarine.laboureau@unine.ch", style = "color: #3c8dbc; font-weight: bold;")
      ),
      tags$div(style = "font-size: 24px; font-weight: bold;",
        "All data and scripts are available on my ",
        tags$a(href = "https://github.com/MazzarineL/SBG_app", "GitHub page.")
        )
      )
    ),
    
    tabItem(tabName = "garden_tree",
      helpText(tags$strong("This section displays the Garden Tree plot.")),
      fluidRow(
        box(title = "Garden Tree", status = "primary", solidHeader = TRUE, width = 25,
          plotOutput(outputId = "treePlot", height = "1500px"),
          downloadButton("downloadFullPlot", "Download Garden Tree Plot", class = "btn btn-primary")
        )
      )
    ),
    
    tabItem(tabName = "family_tree",
      helpText(tags$strong("This section presents the Family Tree plot...")),
      fluidRow(
        box(title = "Family Selection", status = "primary", solidHeader = TRUE, width = 12,
          selectInput("family", "Family to Test", choices = sort(unique(cover_species_garden_full$family)), selected = ""),
          sliderInput("genus_select", "Number of genera to select", min = 1, max = 30, value = 5),
          actionButton("actionfamily", "Apply Family Filter", icon = icon("play"), style = "color: #fff; background-color: #222c32;"),
          textOutput("textgenus"),
          tableOutput("onlygenus"),
          plotOutput("FamilyPlot", height = "800px"),
          tableOutput("mytable"),
          downloadButton("downloadFamilyPlot", "Download Family Tree Plot", class = "btn btn-primary"),
          downloadButton("downloadTable", "Download Priority Table", class = "btn btn-primary")
        )
      )
    ),
    
    tabItem(tabName = "plot_cover",
      fluidRow(
        box(title = "Coverage plot", status = "primary", solidHeader = TRUE, width = 12,
          plotOutput("coverplot", height = "1000px"),
          downloadButton("downloadcoverplot", "Download Coverage Plot", class = "btn btn-primary")
        )
      ),
      fluidRow(
        box(title = "Venn plot", status = "primary", solidHeader = TRUE, width = 15,
          plotOutput("vennplot", height = "1200px"),
          downloadButton("dlvenplot", "Download Venn Plot", class = "btn btn-primary")
        )
      )
    ),
    
    tabItem(tabName = "whit_garden_plot",
      fluidRow(
        box(title = "Whittaker Garden Plot", status = "primary", solidHeader = TRUE, width = 12,
          plotOutput("whitplot", height = "1000px"),
          downloadButton("dlwhitplot", "Download Whittaker Plot", class = "btn btn-primary")
        )
      )
    ),
    
    tabItem(tabName = "whit_family_plot",
      fluidRow(
        box(title = "Whittaker Family Plot", status = "primary", solidHeader = TRUE, width = 12,
          plotOutput("whitplotFamily", height = "1000px"),
          downloadButton("dlwhitplotFamily", "Download Whittaker Family Plot", class = "btn btn-primary")
        )
      ),
      fluidRow(
        box(title = "Whittaker Family Plot Selection", status = "primary", solidHeader = TRUE, width = 12,
          plotlyOutput("whitplotSelect", height = "1000px")
        )
      )
    ),
    
    tabItem(tabName = "species_selection",
      fluidRow(
        box(title = "Species Selection", status = "primary", solidHeader = TRUE, width = 12,
          selectInput("selected_family", "Family", choices = sort(unique(cover_species_garden_full$family))),
          selectInput("selected_genus", "Genus", choices = c("", NULL)),
          selectInput("selected_species", "Species", choices = c("", NULL)),
          downloadButton("downloadTablespecies", "Download table of species", class = "btn btn-primary"),
          tableOutput("selectedData")
        )
      )
    ),
    
    tabItem(tabName = "species_distribution",
      fluidRow(
        box(title = "Species Distribution", status = "primary", solidHeader = TRUE, width = 12,
          selectInput("GPS_family", "Select a family", choices = NULL),
          selectInput("GPS_genus", "Select a genus:", choices = c("", NULL)),
          selectInput("GPS_species", "Select a species:", choices = c("", NULL), multiple = TRUE),
          actionButton("goButton", "Go"),
          leafletOutput("map", width = "100%", height = "500px"),
          plotOutput("mapsSimple", height = "1000px"),
          downloadButton("downloaddistrib", "Download Distribution map", class = "btn btn-primary")
        )
      )
    ),

    tabItem(tabName = "data_frame",
      fluidRow(
        box(title = "jbuf Merged Data Frame", status = "primary", solidHeader = TRUE, width = 12,
          DT::dataTableOutput("table_jbuf"),
          downloadButton("download_jbuf", "Download jbuf Data", class = "btn btn-primary")
        )
      ),
      fluidRow(
        box(title = "jbn Merged Data Frame", status = "primary", solidHeader = TRUE, width = 12,
          DT::dataTableOutput("table_jbn"),
          downloadButton("download_jbn", "Download jbn Data", class = "btn btn-primary")
        )
      )
    ),
    
    tabItem(tabName = "bot_map",
      fluidRow(
        box(title = "Botanical Garden Maps", status = "primary", solidHeader = TRUE, width = 12,
          tabsetPanel(
            tabPanel("jbuf Map", leafletOutput("leaflet_jbuf", height = "800px")),
            tabPanel("jbn Map", leafletOutput("leaflet_jbn", height = "800px"))
          ),
          downloadButton("download_map_jbuf", "Download jbuf Map", class = "btn btn-primary"),
          downloadButton("download_map_jbn", "Download jbn Map", class = "btn btn-primary")
        )
      )
    ),
    
    tabItem(tabName = "sample",
  fluidRow(
    box(title = "Progress Bar", status = "primary", solidHeader = TRUE, width = 12,
      plotOutput("progress_plot", height = "600px")
    )
  )
)
  )
)
)