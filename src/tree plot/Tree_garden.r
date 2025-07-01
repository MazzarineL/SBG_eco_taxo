cover_family_garden <- read.csv("D:/gitrepo/SBG_eco_taxo/data/cover_family_garden_500.csv", sep=";")
taxonomy_family_full = read.csv("D:/gitrepo/SBG_eco_taxo/data/taxonomy_family_genus_full.csv")



taxonomy_merge <-
      left_join(taxonomy_family_full,
        cover_family_garden,
        by = c("name.family" = "family"))


taxonomy_merge <- taxonomy_merge %>%
  mutate(
    garden = if_else(twice != 0, 3,
                     if_else(neu != 0, 1,
                             if_else(fri != 0, 2, 0)))
  )


    taxonomy_merge <-  taxonomy_merge %>% 
                                           mutate(garden = ifelse(garden == "2", "0", garden))
                                           ,
  taxonomy_merge <-   taxonomy_merge %>% 
                                           mutate(garden = ifelse(garden == "1", "0", garden))
                            


taxonomy_merge$pres[is.na(taxonomy_merge$pres)] <- 0
taxonomy_merge <- taxonomy_merge[!is.na(taxonomy_merge$ott_id.family),]


    my_tree <- tol_induced_subtree(ott_ids = taxonomy_merge$ott_id.family)

    sp_name <- gsub("_.*", "", my_tree$tip.label)

    my_tree$tip.label <- sp_name

    species <- taxonomy_merge$name.family
    g <- split(species, taxonomy_merge$garden)


tree_plot <- ggtree::ggtree(my_tree, layout = "circular") +
  geom_tiplab(size = 1.5, offset = 0.5)

g1 <- ggtree::groupOTU(tree_plot, g, "species") + 
  aes(color = species) +
  theme(legend.position = "right") +
  scale_color_manual(name = "Family", 
                     values = c("0" = "orange", "1" = "darkgreen", "2" = "darkblue", "3" = "purple"), 
                     labels = c("Not available", "Available in Neuchâtel", "Available in Fribourg", "Available in both"),
                     breaks = c("0", "1", "2", "3"))
g1





















# Loading data ----
cover_family_garden <- read.csv("D:/gitrepo/SBG_eco_taxo/data/cover_family_garden_500.csv", sep=";")
cover_genus_garden <- read.csv("D:/gitrepo/SBG_eco_taxo/data/cover_genus_family_garden.csv", sep=";")
taxonomy_family_full <- read.csv("D:/gitrepo/SBG_eco_taxo/data/taxonomy_family_genus_full.csv")

# ui.R ----
ui <- fluidPage(
  titlePanel("Cover Botanical Garden"),
  setBackgroundColor("#ECECEC"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "family", label = "1. Family to test", choices = cover_family_garden$family, selected = ""),
      actionButton(inputId = "action", label = "Go!"),
      sliderInput(inputId = "step", label = "2. Step", min = 1, max = 10, value = 5),
      sliderInput(inputId = "window", label = "3. Window", min = 1, max = 10, value = 5),
      downloadButton(outputId = "downloadData", label = "Download Priority Data"),
      downloadButton(outputId = "downloadPlot", label = "Download Plot as PDF")
    ),
    mainPanel(
      plotOutput(outputId = "distPlot"),
      tableOutput("mytable")
    )
  )
)

# server.R ----
server <- function(input, output, session) {
  # Reactive value to store intermediate data
  intermediate_data <- reactiveVal(NULL)
  
  # Event triggered by the action button
  observeEvent(input$action, {
    family_test <- input$family

    tree <- rotl::tol_induced_subtree(ott_ids = taxonomy_family_full$uid)
    tree$tip.label <- gsub("^x_|\\(genus_in_kingdom_Archaeplastida\\)_|_.*", "", tree$tip.label)
    
    cover_genus_select <- cover_genus_garden %>%
      filter(family == family_test)
    cover_genus_select$genus <- gsub("^x ", "", cover_genus_select$genus)
    
    p <- ggtree(tree) + geom_tiplab() + geom_hilight(node = 12, extendto = 2.5)

    intermediate_data(list(tree = tree, cover_genus_select = cover_genus_select, p = p))
  })

  # Reactive expression to update the data based on slider input
  df_rangement <- reactive({
    req(intermediate_data())
    step <- input$step
    before <- input$before
    after <- input$after
    p <- intermediate_data()$p
    cover_genus_select <- intermediate_data()$cover_genus_select

    df_rangement <- data.frame(genus = get_taxa_name(p))
    df_rangement <- merge(df_rangement, cover_genus_select, by = "genus", all.x = TRUE, sort = FALSE)
    df_rangement <- df_rangement %>%
      dplyr::mutate(date = as.Date('2023-01-01') + row_number() - 1) %>%
       dplyr::mutate(
        reg_7day = slide_dbl(
          pres, 
          .f = ~sum(.x, na.rm = TRUE), 
          .before = window,
          .after = window,
          .step = step
        )
      ) %>%
       dplyr::mutate(pres = if_else(reg_7day == 0 & pres == 0, 3, pres))

    df_rangement
  })

  # Render the plot
  output$distPlot <- renderPlot({
    req(intermediate_data(), df_rangement())
    tree <- intermediate_data()$tree
    genus_cover <- split(df_rangement()$genus, df_rangement()$pres)
    
    tree_plot <- ggtree(tree, layout = "circular") +
      theme(legend.position = "right", legend.key.size = unit(3, "lines")) + 
      geom_tiplab(size = 2, offset = 0.5)
    
    g1 <- ggtree::groupOTU(tree_plot, genus_cover, "species") + aes(color = species) +
      theme(
        legend.position = "right",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14)
      ) + 
      scale_color_manual(name = "Family", values = c("orange", "darkgreen", "blue"), labels = c("Not available", "Available", "Priority"))
    
    print(g1)
  })

  # Render the table
  output$mytable <- renderTable({
    req(df_rangement())
    df_rangement_priority <- df_rangement() %>% filter(pres == 3) %>% select(genus)
    df_rangement_priority
  })

  # Download handler for df_rangement_priority
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("df_rangement_priority", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      df_rangement_priority <- df_rangement() %>% filter(pres == 3) %>% select(genus)
      write.csv(df_rangement_priority, file, row.names = FALSE)
    }
  )
  
  # Download handler for the plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("Tree_family", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      req(intermediate_data(), df_rangement())
      tree <- intermediate_data()$tree
      genus_cover <- split(df_rangement()$genus, df_rangement()$pres)
      
      tree_plot <- ggtree(tree, layout = "circular") +
        theme(legend.position = "right", legend.key.size = unit(3, "lines")) + 
        geom_tiplab(size = 2, offset = 0.5)
      
      g1 <- ggtree::groupOTU(tree_plot, genus_cover, "species") + aes(color = species) +
        theme(
          legend.position = "right",
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12)
        ) + 
        scale_color_manual(name = "Family", values = c("orange", "darkgreen", "blue"), labels = c("Not available", "Available", "Priority"))
      
      ggsave(file, plot = g1, device = "pdf")
    }
  )
}

# Run the app ----
shinyApp(ui = ui, server = server)



