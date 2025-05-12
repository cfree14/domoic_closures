
# Function to plot data
# lat_min=40; lat_max=42; date_min=2014; date_max=2023
plot_data <- function(data, lat_min, lat_max, date_min, date_max){
  
  # Build status/color key
  statuses <- c( "Season open", "Out-of-season", "Body condition delay", 
               "Body condition/domoic acid delay", "Domoic acid delay", "Evisceration order",                                
               "Evisceration order (+depth/gear restriction)", "Whale entanglement closure",                            
               "30-fa depth restriction", "40-fa depth restriction",                           
               "40-fa depth restriction/20% gear reduction", "33% gear reduction", "50% gear reduction" )
  colors <- c("grey85", "white", "pink", "orange", "darkred", "coral", "purple3",
              "navy", "dodgerblue3", "dodgerblue1", "dodgerblue", "lightblue", "lightblue1")
  status_df <- tibble(order=1:length(statuses), 
                      status=statuses,
                      color=colors)
  
  # Prepare dates
  date1 <- paste0(date_min, "-10-01") %>% lubridate::ymd()
  date2 <- paste0(date_max+1, "-10-01") %>% lubridate::ymd()
  
  # Subset data
  sdata <- data %>%
    # Recode statuses
    # mutate(status=factor(status, labels=statuses)) %>% 
    # Filter to lats of interest
    filter(lat_dd>=lat_min & lat_dd <=lat_max) %>% 
    # Filter to seasons of interest
    filter(date>=date1 & date <= date2)
  
  # Identify status/colors to use
  status_use <- status_df %>% 
    filter(status %in% sdata$status)
  colors_use <- status_use$color

  # Order data by statuses used
  sdata_ordered <- sdata %>% 
    mutate(status=factor(status, levels=status_use$status))
  
  # Sonoma-Mendocino county line
  son_mend_county <- 38+46.125/60
  
  # State label date value
  date_min_do <- min(sdata$date)
  
  # Determine ybreaks
  lat_diff <- lat_max-lat_min
  if(lat_diff>5){ybreaks <- 35:48}
  if(lat_diff<=5){ybreaks <- seq(35, 48, 0.5)}
  if(lat_diff<=2){ybreaks <- seq(35, 48, 0.25)}
  
  # Theme
  my_theme <-  theme(axis.text=element_text(size=13),
                     axis.title=element_text(size=14),
                     legend.text=element_text(size=13),
                     legend.title=element_text(size=14),
                     plot.title=element_blank(),
                     # Gridlines
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key.size = unit(0.6, "cm"),
                     legend.background = element_rect(fill=alpha('blue', 0)))
  
  # Plot data
  label_size <- 4.5
  g <- ggplot(sdata_ordered, aes(x=date, y=lat_dd, fill=status)) +
    # Plot raster
    geom_raster() +
    # State/region lines
    geom_hline(yintercept=c(48.43333, 46.25000, 42.00000), linewidth=0.2) +
    geom_hline(yintercept = son_mend_county, linetype="dashed", linewidth=0.2) + # Sonoma/Mendocino
    # Label state lines
    annotate(geom="text", x=date_min_do, y=48.48, hjust=0, vjust=1.5, label="Washington", color="black", size=label_size) +
    annotate(geom="text", x=date_min_do, y=46.25, hjust=0, vjust=1.5, label="Oregon", color="black", size=label_size) +
    annotate(geom="text", x=date_min_do, y=42, hjust=0, vjust=1.5, label="N. California", color="black", size=label_size) +
    annotate(geom="text", x=date_min_do, y=son_mend_county, hjust=0, vjust=1.5, label="C. California", color="black", size=label_size) +
    # Axis
    scale_x_date(date_breaks="1 year", date_labels = "%Y") +
    scale_y_continuous(breaks=ybreaks, lim=c(lat_min, lat_max)) +
    # Labels
    labs(x="Date", y="Latitude (°N)") +
    # Legends
    scale_fill_manual(name="Season status", 
                      values=colors_use, 
                      drop=F) +
    # Theme
    theme_bw() + my_theme
  g
  
}