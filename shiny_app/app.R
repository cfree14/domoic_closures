
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

# Read zones
zones_orig <- readxl::read_excel(file.path(datadir, "WC_dcrab_da_mgmt_zones.xlsx"))

# Get land
usa <- readRDS(file=file.path(datadir, "usa.Rds"))
foreign <- readRDS(file=file.path(datadir, "foreign.Rds"))


# Format closure data
################################################################################

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


# Format zones
################################################################################

# Build zones dataframe
zones_df <- zones_orig %>%
  filter(!is.na(lat_dd_north)) %>%
  select(state, lat_dd_north) %>%
  rename(y=lat_dd_north) %>%
  mutate(x1=recode(state,
                   "Washington"="2014-10-01",
                   "Oregon"="2017-11-01",
                   "California"="2020-11-01") %>% ymd(),
         x2=recode(state,
                   "Washington"="2023-09-15",
                   "Oregon"="2023-08-14",
                   "California"="2023-07-15"),
         x2=ifelse(y<38.3, "2023-06-30", x2),
         x2=ymd(x2))

# Build zones
zones <- zones_orig %>%
  mutate(lat_dd_avg=(lat_dd_north+lat_dd_south)/2) %>%
  # Alter Zone I lat
  mutate(lat_dd_avg=ifelse(zone_id=="H", 35.3, lat_dd_avg)) %>% 
  filter(!is.na(lat_dd_avg))


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
      
      # Plot management zones?
      checkboxInput("show_mgmt", "Plot detailed management zones?", value = TRUE),
      
      # Download instructuons
      helpText("To download the plot, right click on the plot and select 'Save image as'.")
      
    ),
    
    # Main panel
    mainPanel(width = 9,
      plotOutput("statusPlot", height = "500px", width="1100px")
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
                   usa = usa,
                   foreign = foreign,
                   zones = zones,
                   zones_df = zones_df,
                   lat_min = input$lat_range[1],
                   lat_max = input$lat_range[2],
                   date_min = input$season_range[1],
                   date_max = input$season_range[2],
                   show_mgmt=input$show_mgmt)
    g
  })

}

shinyApp(ui = ui, server = server)
