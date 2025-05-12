
# Clear
rm(list = ls())
options(dplyr.summarise.inform=F)

# Setup
################################################################################

# Packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(RColorBrewer)

# Directories
datadir <- "data" # for actual app
codedir <- "code"  # for actual app
# datadir <- "shiny_app/data" # when testing
# codedir <- "shiny_app/code" # when testing

# Source code
sapply(list.files(codedir), function(x) source(file.path(codedir, x)))

# Read data
data_orig <- readRDS(file.path(datadir, "2015_2023_WC_dcrab_closures.Rds"))

# Statuses
statuses <- c( "Season open", "Out-of-season", "Body condition delay", 
               "Body condition/domoic acid delay", "Domoic acid delay", "Evisceration order",                                
               "Evisceration order (+depth/gear restriction)", "Whale entanglement closure",                            
               "30-fa depth restriction", "40-fa depth restriction",                           
               "40-fa depth restriction/20% gear reduction", "33% gear reduction", "50% gear reduction" )

# Format data
levels(data_orig$status)
data <- data_orig %>% 
  mutate(status=as.character(status),
         # Recode to match final levels above
         status=recode(status, 
                       "Evisceration order (+depth restriction/gear reduction)"="Evisceration order (+depth/gear restriction)",
                       "30-fathom depth restriction" = "30-fa depth restriction",                          
                       "40-fathom depth restriction" = "40-fa depth restriction",                          
                       "40-fathom depth restriction/20% gear reduction"="40-fa depth restriction/20% gear reduction"),
         status=factor(status, levels=statuses))


# User interface
################################################################################

# User interface
ui <- fluidPage(

  # Title
  titlePanel("Dungeness crab season status viewer"),
  
  # Side panel
  sidebarLayout(
    sidebarPanel(width = 3,
      
      # Latitude slider
      sliderInput("lat_range", "Select latitude range (°N):",
                  min = 35, max = 49, value = c(35, 49), step = 0.25),
      
      # Season slider
      sliderInput("season_range", "Select season range:",
                  min = 2014, max = 2023, value = c(2014, 2023), step = 1, sep=""),
      
      # Download instructuons
      helpText("To download the plot, right click on the plot and select 'Save image as'.")
      
    ),
    
    # Main panel
    mainPanel(width = 9,
      plotOutput("statusPlot", height = "500px", width="1000px")
    )
  )

)


# Server
################################################################################

# Server
server <- function(input, output, session){
  
  # Build plot
  output$statusPlot <- renderPlot({
    g <- plot_data(data=data, 
                   lat_min = input$lat_range[1],
                   lat_max = input$lat_range[2],
                   date_min = input$season_range[1],
                   date_max = input$season_range[2])
    g
  })

}

shinyApp(ui = ui, server = server)
