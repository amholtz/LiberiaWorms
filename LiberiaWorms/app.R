library(shiny)
library(ggplot2)
library(dplyr)
library(sf)
library(tmap)
library(leaflet)



#Import cleaned Data from Final_WO Master File
liberia_sp <- read.csv("data/liberia_sp.csv", header=TRUE)

liberia_sp <- liberia_sp %>% 
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)


choices_admin <- dput(as.character(unique(liberia_sp$ADMIN2)))
choices_species <- c("HK_prevalence", "TT_prevalence", "Asc_prevalence")

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
    
    # Application title
    titlePanel("LiberiaWorms"),
    
    wellPanel(
        helpText("The dataset downloaded from ESPEN portal compiles school-based surveys conducted throughout Liberia between 2012 and 2015. The number of children examined per school is variable and highly dependent on the school size, althouth WHO guidelines recommend that around 50 children (5-15 years old) are expected to be surveyed per school. In this surveys, students are randomly selected and invited to provide a single stool sample, which is analysted using Kato-Katz test for the presence of STH eggs. The provided dataset include the survey outcomes (number of examined, positive and school prevalence) and geographic coordinates (longitude and latitude) for the surveyed schools.
        To view prevalence of data for all Liberia use the first map."),
        helpText("To view prevalence data by district use the second map")),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("species",
                        "Select Species",
                        choices_species),
            selectInput("admin", "Selection ADMIN 2 District", choices_admin)
            
        
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("lib_spec", height = 800),
            headerPanel(('\n')),
            headerPanel(('\n')),
            leafletOutput("lib_spec_admin", height = 800)
            
        )
    )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
    
    output$lib_spec <- renderLeaflet({
        
        species <- input$species
        text <- input$text_admin
        
        breaks <- c(0, 0.2, 0.5, 1)
        
        
        lib_spec <- tm_basemap(server = "OpenStreetMap") +
            tm_shape(liberia_sp) +
            tm_symbols(col = species,
                       breaks = breaks,
                       size = .2,
                       palette = "YlGnBu") +
            tm_compass(north = 0) +
            tm_layout(
                "Prevalence of Soil-Transmitted Helminths in Liberia",
                legend.width = 1000,
                legend.height= 1000
            ) + tm_scale_bar()
            
            tmap_leaflet(lib_spec)
        
        
    })
    
    output$lib_spec_admin <- renderLeaflet({

    species <- input$species
    admin <- input$admin
    
    
    liberia_sp <- liberia_sp %>% 
        filter(ADMIN2 == admin)
    
    breaks <- c(0, 0.2, 0.5, 1)
    
    
    lib_spec_admin <- tm_basemap(server = "OpenStreetMap") +
        tm_shape(liberia_sp) +
        tm_symbols(col = species,
                   breaks = breaks,
                   size = .2,
                   palette = "YlGnBu") +
        tm_compass(north = 0) +
        tm_scale_bar()
    
    tmap_leaflet(lib_spec_admin)
    
    
    })
    
    
    
})



# Run the application
shinyApp(ui = ui, server = server)