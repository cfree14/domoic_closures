
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
datadir <- "shiny_app/data" # when testing
codedir <- "shiny_app/code" # when testing

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")


# Export
saveRDS(usa, file=file.path(datadir, "usa.Rds"))
saveRDS(foreign, file=file.path(datadir, "foreign.Rds"))
