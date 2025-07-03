#install.packages(c("shiny", "shinydashboard", "plotly", "leaflet", "curl"))


library(shiny)
library(shinydashboard)
library(plotly)
library(curl)
library(leaflet)

cover_species_garden_full <- read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_eco_taxo/refs/heads/main/data/cover_species_garden.csv"))

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
  ),
  dashboardBody(
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

    .main-bg {
      background-image: background-image: url("bg.jpg");
      background-size: cover;
      background-repeat: no-repeat;
      background-position: center center;
      min-height: 100vh;
      padding: 30px;
    }

    .main-bg div {
      background-color: rgba(255, 255, 255, 0.85);
      padding: 20px;
      border-radius: 10px;
    }

    .content {
     background-color: transparent; 
     padding: 0;                    
     margin: 0;
    }



 h1 {
    margin-top: 10px; /* ou même 0px */
    margin-bottom: 10px;
  }

     .tab-pane {
  background-color: #ffffff;
  padding: 10px;             /* un peu plus de padding si tu veux que le contenu "respire" */
  min-height: 100vh;         /* assure que ça prend toute la hauteur visible */
  width: 100% !important;    /* force la largeur à 100% */
  margin: 0;
}

    .help-block {
      background-color: #ffffff;
      font-size: 15px;
      color: #333;
    }



.dataTables_wrapper {
      margin-left: 20px;
      margin-right: 20px;
    }


  '))),

  
  tabItems(
tabItem(tabName = "filters",
  tags$div(
    style = "
      position: fixed;
      top: 0; left: 0; right: 0; bottom: 0;
      background-image: url('bg.jpg');
      background-size: cover;
      background-position: center center;
      min-height: 600px;
      padding: 40px;
      color: #000000;
      font-weight: bold;
      font-size: 30px;
      display: flex;
      flex-direction: column;
      justify-content: center;
      align-items: center;
      text-align: center;
    ",
    # couche semi-transparente pour améliorer la lisibilité du texte
    tags$div(
      style = "
        background-color: rgba(255, 255, 255, 0.8);
        padding: 30px;
        border-radius: 10px;
        max-width: 900px;
        margin-top: 50px;
      ",
      "Welcome to the Botanical Garden Coverage Application.",
      tags$br(), tags$br(),
      "To get started, select one or more gardens.",
      tags$br(), tags$br(),
      "Launch the script by clicking the 'Go' button.",
      tags$br(), tags$br(),
      "Explore the various tabs to view your results. Some pages include multiple graphs, so feel free to scroll.",
      tags$br(), tags$br(),
      "If you find a bug or have suggestions to improve the app’s usability, or if you work at a botanical garden and want your data included, please contact me at ",
      tags$a(
        href = "mailto:mazzarine.laboureau@unine.ch", 
        "mazzarine.laboureau@unine.ch",
        style = "color: #3c8dbc; font-weight: bold;"
      ),
      tags$br(), tags$br(),
      "All data and scripts are available on my ",
      tags$a(href = "https://github.com/MazzarineL/SBG_eco_taxo/tree/main", "GitHub page.")
    )
  )
),

    
tabItem(tabName = "garden_tree",
  tags$h1("Garden Tree"),
  helpText(tags$strong("This section displays the phylogenetic tree for each family present in the selected garden, allowing you to visualize the taxonomic coverage of the gardens.")),
  
  fluidRow(
    column(
      width = 12,
      div(style = "text-align: right; margin-bottom: 10px;",
          downloadButton("downloadFullPlot", "Download Garden Tree Plot", class = "btn btn-primary")
      )
    )
  ),
  fluidRow(
    plotOutput(outputId = "treePlot", height = "1500px")
  )
),


tabItem(tabName = "family_tree",
  tags$h1("Family Tree"),
  helpText(tags$strong("This section presents the Family Tree plot.
For the selected family, you will see the genera present in the selected garden. You can choose the number of genera to include, and the model will suggest which genera to prioritize in order to best cover the taxonomic diversity of the family.
Please note that the tree might display more blue branches than the number of genera you selected. This occurs because the model cannot always reach the exact number requested and instead aims to provide the closest match to maximize coverage.
If the tree does not display any selected genera, it means that the number requested is too high relative to what is available. In that case, please try selecting all genera.")),
  fluidRow(
    box(
      width = 12, status = "primary", solidHeader = TRUE,
      selectInput("family", "Family to Test", choices = sort(unique(cover_species_garden_full$family)), selected = ""),
      sliderInput("genus_select", "Number of genera to select", min = 1, max = 30, value = 5),
      actionButton("actionfamily", "Apply Family Filter", icon = icon("play"), 
                   style = "color: #fff; background-color: #008d4c; border: none;"),
      textOutput("textgenus"),
      DT::dataTableOutput("onlygenus")
    )
  ),
  fluidRow(
    column(
      width = 12,
      div(style = "text-align: right; margin-bottom: 10px;",
          downloadButton("downloadFamilyPlot", "Download Family Tree Plot", class = "btn btn-primary"),
          downloadButton("downloadTable", "Download Priority Table", class = "btn btn-primary")
      )
    )
  ),
  fluidRow(
    plotOutput("FamilyPlot", height = "800px"),
          DT::dataTableOutput("mytable")

  )
),

tabItem(tabName = "plot_cover",
  tags$h1("Taxonomic Coverage"),
  helpText(tags$strong("These graphs illustrate the taxonomic coverage at the family, genus, and species levels within the selected gardens, providing a detailed view of biodiversity representation.")),
  fluidRow(
    column(
      width = 12,
      div(style = "text-align: right; margin-bottom: 10px;",
          downloadButton("downloadcoverplot", "Download Coverage Plot", class = "btn btn-primary")
      )
    )
  ),
  fluidRow(
    plotOutput("coverplot", height = "1000px")
  ),
  fluidRow(
    column(
      width = 12,
      div(style = "text-align: right; margin-bottom: 10px;",
          downloadButton("dlvenplot", "Download Venn Plot", class = "btn btn-primary")
      )
    )
  ),
  fluidRow(
    plotOutput("vennplot", height = "800px")
  ),
  fluidRow(
  column(
    width = 12,
    div(style = "text-align: right; margin-bottom: 10px;",
        downloadButton("dlpiechart", "Download Pie Chart", class = "btn btn-primary")
    )
  )
),
fluidRow(
  plotOutput("piechart", height = "600px")  
)
),

tabItem(tabName = "whit_garden_plot",
  tags$h1("Whittaker Garden Plot"),
  helpText(tags$strong("Whittaker diagrams allow you to position plants within biomes according to their temperature and precipitation preferences. This plot shows the overall environmental coverage of all plants present in the selected gardens.")),
  fluidRow(
    column(
      width = 12,
      div(style = "text-align: right; margin-bottom: 10px;",
          downloadButton("dlwhitplot", "Download Whittaker Plot", class = "btn btn-primary")
      )
    )
  ),
  fluidRow(
    plotOutput("whitplot", height = "1000px")
  )
),

tabItem(tabName = "whit_family_plot",
  tags$h1("Whittaker Family Plot"),
  helpText(tags$strong("This diagram displays the environmental coverage of plants from the selected family, based on their climatic preferences.")),
  fluidRow(
    column(
      width = 12,
      div(style = "text-align: right; margin-bottom: 10px;",
          downloadButton("dlwhitplotFamily", "Download Whittaker Family Plot", class = "btn btn-primary")
      )
    )
  ),
  fluidRow(
    plotOutput("whitplotFamily", height = "1000px")
  ),
  fluidRow(
    plotlyOutput("whitplotSelect", height = "1000px")
  )
),

tabItem(tabName = "species_selection",
  tags$h1("Species Selection"),
  helpText(tags$strong("If you are looking for a specific species, you can find it here by selecting the corresponding family, then genus, and finally species.")),
  fluidRow(
    box(
      width = 12, status = "primary", solidHeader = TRUE,
      selectInput("selected_family", "Family", choices = sort(unique(cover_species_garden_full$family))),
      selectInput("selected_genus", "Genus", choices = c("", NULL)),
      selectInput("selected_species", "Species", choices = c("", NULL)),
      div(style = "text-align: right; margin-bottom: 10px;",
          downloadButton("downloadTablespecies", "Download table of species", class = "btn btn-primary")
      ),
      tableOutput("selectedData")
    )
  )
),
tabItem(tabName = "species_distribution",
  tags$h1("Species Distribution"),
  helpText(tags$strong("This section allows you to visualize the global distribution of the selected plant species.")),
  fluidRow(
    box(
      width = 6, status = "primary", solidHeader = TRUE,
      selectInput("GPS_family", "Select a family", choices = NULL),
      selectInput("GPS_genus", "Select a genus:", choices = c("", NULL)),
      selectInput("GPS_species", "Select a species:", choices = c("", NULL), multiple = TRUE),
      actionButton("addSpecies", "Add Species to Selection")
    ),
    box(
      width = 6, status = "info", solidHeader = TRUE,
      tags$h4("Selected Species"),
      uiOutput("selected_species_ui"),
      actionButton("clearSelection", "Clear Selected Species", class = "btn btn-warning")
    )
  ),
  fluidRow(
    box(
      width = 12, status = "primary", solidHeader = TRUE,
      actionButton("goButton", "Go"),
      leafletOutput("map", width = "100%", height = "500px"),
      plotOutput("mapsSimple", height = "1000px"),
      div(style = "text-align: right; margin-top: 10px;",
          downloadButton("downloaddistrib", "Download Distribution map", class = "btn btn-primary")
      )
    )
  )
),

tabItem(tabName = "data_frame",
  tags$h1("Data Frames"),
  fluidRow(
    box(
      width = 12, status = "primary", solidHeader = TRUE,
      helpText("This section displays the merged data frame linking the botanical gardens of Fribourg with our collected samples."),
      DT::dataTableOutput("table_jbuf"),
      div(style = "text-align: right; margin-top: 10px;",
          downloadButton("download_jbuf", "Download jbuf Data", class = "btn btn-primary")
      )
    )
  ),
  fluidRow(
    box(
      width = 12, status = "primary", solidHeader = TRUE,
      helpText("This section displays the merged data frame linking the botanical gardens of Neuchâtel with our collected samples."),
      DT::dataTableOutput("table_jbn"),
      div(style = "text-align: right; margin-top: 10px;",
          downloadButton("download_jbn", "Download jbn Data", class = "btn btn-primary")
      )
    )
  ),
  fluidRow(
    box(
      width = 12, status = "primary", solidHeader = TRUE,
      helpText("This section displays the merged data frame linking the botanical gardens of Champex with our collected samples."),
      DT::dataTableOutput("table_jbc"),
      div(style = "text-align: right; margin-top: 10px;",
          downloadButton("download_jbc", "Download jbc Data", class = "btn btn-primary")
      )
    )
  ),
),

tabItem(tabName = "bot_map",
  tags$h1("Botanical Garden Maps"),
  helpText(tags$strong("This section shows the map with the geographic locations of our collected samples from the Fribourg and Neuchâtel botanical gardens.")),
  fluidRow(
  column(6, textInput("filter_sample_id", "Filter Sample ID")),
  column(6, textInput("filter_taxon_name", "Filter Taxon Name"))
),
fluidRow(
  box(
    width = 12,
    tabsetPanel(
      tabPanel("Fribourg", leafletOutput("leaflet_jbuf", height = "800px")),
      tabPanel("Neuchâtel", leafletOutput("leaflet_jbn", height = "800px")),
      tabPanel("Champex", leafletOutput("leaflet_jbc", height = "800px"))
    ),
    div(style = "text-align: right; margin-top: 10px;",
          downloadButton("download_map_jbuf", "Download jbuf Map", class = "btn btn-primary"),
          downloadButton("download_map_jbn", "Download jbn Map", class = "btn btn-primary"),
          downloadButton("download_map_jbc", "Download jbc Map", class = "btn btn-primary")
      )
  )
)
),

tabItem(tabName = "sample",
  tags$h1("Sampling Progress"),
  helpText(tags$strong("This graph shows the progress of our collections across the selected botanical gardens.")),
  fluidRow(
    plotOutput("progress_plot", height = "600px")
  )
)



  ))
)



