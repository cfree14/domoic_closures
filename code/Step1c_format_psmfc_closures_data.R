
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)
library(googlesheets4)

# Directories
indir <- "data/raw"
outdir <- "data/processed"
plotdir <- "data/closures/figures"

# OR-WA base url
googlesheets4::gs4_auth_configure(api_key = "")
base_url <- "https://docs.google.com/spreadsheets/d/1wz18kDz6u19GbGoZrW_q8Dz-kZq16IKF33DZb5Sd4-w/edit#gid=0"

# Read data
data_orig <- googlesheets4::read_sheet(ss=base_url, sheet="PSMFC_DungCrab")

# Read commercial Dungeness crab season key
season_key_orig <- readxl::read_excel(file.path(indir, "season_key.xlsx"))


# Format data
################################################################################

# Step 1. Basic formatting
data1 <- data_orig %>%
  # Column names
  janitor::clean_names("snake") %>%
  rename(action=action_close_open, fishery=fishery_type, comm_name=species_parts, lat_s=s_latitude_n, lat_n=n_latitude_n,
         action_type=type_of_action) %>%
  # Format common name
  mutate(comm_name=recode(comm_name, "Dungeness Crab"="Dungeness crab")) %>%
  # Format fishery
  mutate(fishery=stringr::str_to_sentence(fishery),
         fishery=recode(fishery, "Commercial/recreational"="Commercial/Recreational")) %>%
  # Convert dates
  mutate(date=ymd(date),
         release_date=ymd(release_date)) %>%
  # Format reason
  mutate(reason=ifelse(grepl("evisceration|33", action_type), action_type, reason),
         reason=recode(reason, "body condition/biotoxin"="body condition/domoic acid"))

# Inspect data
str(data1)
table(data1$comm_name)
table(data1$fishery)
table(data1$action_type)
table(data1$action)
table(data1$reason)


# Step 2. One row per fishery
data2 <- purrr::map_df(1:nrow(data1), function(x) {

  # Get row
  row <- data1 %>% slice(x)

  # Identify species represented in row
  fisheries_in_row_string <- row %>% pull(fishery)
  fisheries_in_row_list <- strsplit(fisheries_in_row_string, split="/")
  fisheries_in_row_cvec <- unlist(fisheries_in_row_list)

  # Duplicate row, if necessary
  nfisheries <- length(fisheries_in_row_cvec)
  if(nfisheries>1){
    row_out <- row %>%
      slice(rep(1:n(), each=nfisheries)) %>%
      mutate(fishery=fisheries_in_row_cvec)
  }else{
    row_out <- row
  }

})

# Step 3. Final formatting (for plotting)
data3 <- data2 %>%
  # Remove non-latitudinal closures (islands, bays, etc)
  filter(!is.na(lat_s)) %>%
  # Select and arrange columns
  arrange(comm_name, fishery, date) %>%
  select(comm_name, fishery, date, action, reason, lat_s, lat_n) %>%
  # Remove SMA stuff
  filter(!reason %in% c("head-start", "SMA", "summer extension"))

# Format for export
events <- data2 %>%
  # Remove non-latitudinal closures (islands, bays, etc)
  filter(!is.na(lat_s)) %>%
  # Select and arrange columns
  arrange(comm_name, fishery, date) %>%
  select(comm_name, fishery, year, date, action, reason, lat_s, lat_n, where, link, notes)

# Step 4. Expand season grid to match date range
range(data3$date, na.rm=T)
seasons <- 2004:2023
season_key_expanded <- purrr::map_df(seasons, function(x){

  # Build open/close date for season
  season_key1 <- season_key_orig %>%
    mutate(open=ymd(paste(x, month(open_dummy), day(open_dummy), sep="-")),
           close=ymd(paste(x+1, month(close_dummy), day(close_dummy), sep="-"))) %>%
    select(-c(open_dummy, close_dummy))

})



# Function to build data
################################################################################

# Function to build grid
# data <- data3; species <- "Dungeness crab"; fishery <- "Commercial"; season_key <- season_key_expanded
build_closure_grid <- function(data, species, fishery, season_key){

  # Subset data
  fishery_do <- fishery
  sdata <- data %>%
    filter(comm_name==species & fishery==fishery_do) %>%
    mutate(action_use=ifelse(action=="close", reason, action))

  # Subset season key
  season_key_use <- season_key %>%
    filter(comm_name==species & fishery==fishery_do)

  # Build empty grid
  date1_grid <- min(season_key$open)
  date2_grid <- max(season_key$close)
  dates <- seq(date1_grid, date2_grid, by="1 day")
  lat1 <- min(season_key$lat_s)
  lat2 <- max(season_key$lat_n)
  lats <- seq(lat1, lat2, 0.01)
  closure_grid <- expand.grid(date=dates, lat_dd=lats) %>%
    as.data.frame() %>%
    mutate(status="open",
           comm_name=species,
           fishery=fishery_do) %>%
    select(comm_name, fishery, date, lat_dd, status) %>%
    arrange(date, lat_dd)

  # Loop through announcements
  for(i in 1:nrow(sdata)){

    # Get announcement
    date1 <- sdata %>% slice(i) %>% pull(date)
    lat_s <- sdata %>% slice(i) %>% pull(lat_s)
    lat_n <- sdata %>% slice(i) %>% pull(lat_n)
    status_new <- sdata %>% slice(i) %>% pull(action_use)

    # Apply announcement
    closure_grid <- closure_grid %>%
      mutate(status=ifelse(lat_dd>=lat_s & lat_dd <= lat_n & date>=date1, status_new, status))

  }

  # Apply out-of-season label
  # Loop through management regions and close
  regions <- sort(unique(season_key_expanded$state_region))
  for(i in 1:length(regions)){

    # Region to do
    region_do <- regions[i]
    season_key_region <- season_key_use %>%
      filter(state_region==region_do)

    # Open dates
    open_dates <- purrr::map_df(1:nrow(season_key_region), function(x) {
      date1 <- season_key_region$open[x]
      date2 <- season_key_region$close[x]
      dates <- tibble(date=seq(date1, date2, by="day"))
    })

    # # Dates to close
    # closed_dates <- purrr::map_df(2:nrow(season_key_region), function(x) {
    #   # If middle seasons
    #   if(x!=nrow(season_key_region)){
    #     date1 <- season_key_region$close[x-1]+1
    #     date2 <- season_key_region$open[x]-1
    #     dates <- tibble(date=seq(date1, date2, by="day"))
    #   # If last season
    #   }else{
    #     date1 <- season_key_region$close[x]+1
    #     date2 <- date2_grid
    #     dates <- tibble(date=seq(date1, date2, by="day"))
    #   }
    #   dates
    # })

    # Apply out-of-season closures
    lat_s_region <- season_key_region$lat_s %>% unique()
    lat_n_region <- season_key_region$lat_n %>% unique()
    closure_grid <- closure_grid %>%
      mutate(status=ifelse(lat_dd >= lat_s_region & lat_dd <= lat_n_region & !(date %in% open_dates$date), "out-of-season", status))

  }

  # Make factor
  closure_grid <- closure_grid %>%
    mutate(status=recode_factor(status,
                                "open"="Season open",
                                "out-of-season"="Out-of-season",
                                "body condition"="Body condition delay",
                                "body condition/domoic acid"="Body condition/domoic acid delay",
                                "domoic acid"="Domoic acid delay",
                                "evisceration order"="Evisceration order",
                                "oil spill"="Oil spill",
                                "33% gear reduction"="33% gear reduction"))
                                # "head-start"="Head-start",
                                # "SMA"="SMA",
                                # "Summer extension"="summer extension"))

  # Plot closure grid
  # title <- paste(species, tolower(fishery_do), "fishery")
  # g <- ggplot(closure_grid, aes(x=date, y=lat_dd, fill=status)) +
  #   # Plot raster
  #   geom_raster() +
  #   # Plot events
  #   geom_segment(data=sdata, mapping=aes(x=date, xend=date, y=lat_s, yend=lat_n, linetype=action, ), inherit.aes = F) +
  #   # Axis
  #   scale_x_date(date_breaks="1 year", date_labels = "%Y") +
  #   scale_y_continuous(breaks=35:48) +
  #   # Labels
  #   labs(x="", y="Latitude (°N)", title=title) +
  #   # Legends
  #   scale_fill_manual(name="Season status", values=c("grey80", "white", "pink", "orange", "darkred", "coral", "navy", "purple3"), drop=F) +
  #   # scale_fill_manual(name="Season status", values=c("grey80", "white", "pink", "orange", "darkred", "coral", "navy", "lightblue", "purple", "yellow"), drop=F) +
  #   # Theme
  #   theme_bw()
  # print(g)

  # Return
  return(closure_grid)

}


# Plot indidivually
#data <- wa_dcrabs_comm
plot_closures <- function(data){

  # Species fishery
  species <- unique(data$comm_name)
  fishery <- unique(data$fishery)

  # Setup theme
  my_theme <- theme(axis.text=element_text(size=6),
                    axis.title=element_text(size=8),
                    plot.title=element_text(size=10))
  
  # Data to plot
  data_plot <- data %>% 
    filter(lat_dd >= 46.25	& lat_dd <= 48.43333 & date>="2011-11-15")

  # Plot data
  title <- paste(species, tolower(fishery), "fishery")
  g <- ggplot(data_plot, aes(x=date, y=lat_dd, fill=status)) +
    # Plot raster
    geom_raster() +
    # Axis
    scale_x_date(date_breaks="1 year", date_labels = "%Y") +
    scale_y_continuous(breaks=32:42) +
    # Labels
    labs(x="", y="Latitude (°N)", title=title) +
    # Legends
    scale_fill_manual(name="Season status", values=c("grey80", "white", "pink", "orange", "darkred", "coral", "navy", "purple4"), drop=F) +
    # Theme
    theme_bw() + my_theme
  print(g)

  # Export plot
  # outfig <- paste0(tolower(species) %>% gsub(" ", "_", .), "_",
  #                  tolower(fishery), "_closures.png")
  # ggsave(g, filename=file.path(plotdir, outfig),
  #        width=6.5, height=3, units="in", dpi=600)

}


# Apply and export
################################################################################

# Apply
wa_dcrabs_comm <- build_closure_grid(data=data3, species="Dungeness crab", fishery="Commercial", season_key=season_key_expanded)

plot_closures(wa_dcrabs_comm)

# Export
saveRDS(wa_dcrabs_comm, file=file.path(outdir, "PSMFC_2005_2023_comm_dcrab_closures.Rds"))


