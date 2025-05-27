
# Function to plot data
# lat_min=35; lat_max=49; date_min=2014; date_max=2023; show_mgmt <- T
plot_data <- function(data, usa, foreign, 
                      zones_df, zones,
                      lat_min, lat_max, date_min, date_max, show_mgmt){
  
  # Prepare data
  ##############################################################################
  
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
  mgmt_zones <- c(48.43333, 46.25000, 42.00000, son_mend_county)
  
  # State label date value
  date_min_do <- min(sdata$date)
  
  # Determine ybreaks
  lat_diff <- lat_max-lat_min
  if(lat_diff>5){ybreaks <- 35:48}
  if(lat_diff<=5){ybreaks <- seq(35, 48, 0.5)}
  if(lat_diff<=2){ybreaks <- seq(35, 48, 0.25)}
  
  # Format zones
  zones_df1 <- zones_df %>% 
    mutate(x2=paste0(date_max+1, substr(x2, 5, 10)) %>% lubridate::ymd())
  
  # Plot data
  ##############################################################################
  
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
  
  # Plot map
  if(show_mgmt){
    g1 <- ggplot() + 
      # Management zone lines
      geom_hline(yintercept=zones_df$y, color="grey50", linewidth=0.2, linetype="dashed") +
      # Plot major mgmt lines
      geom_hline(yintercept=c(48.43333, 46.25000, 42.00000), linewidth=0.2) +
      geom_hline(yintercept = son_mend_county, linetype="dashed", linewidth=0.2) +
      # Plot land
      geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3) +
      geom_sf(data=usa, fill="grey90", color="white", lwd=0.3) +
      # Mgmt labels
      geom_text(data=zones, mapping=aes(y=lat_dd_avg, label=zone_id),
                x=-125.5, hjust=0, size=2.4, inherit.aes = F, color="grey20") +
      # Labels
      labs(x="", y="") +
      # Crop
      coord_sf(xlim = c(-125.5, -119), ylim = c(lat_min, lat_max)) +
      # Theme
      theme_bw() + my_theme +
      theme(axis.text.x=element_text(color="white"))
    g1
 
  }else{
    g1 <- ggplot() + 
      # Management zone lines
      # geom_hline(yintercept=zones_df$y, color="grey50", linewidth=0.2, linetype="dashed") +
      # geom_text(data=zones, mapping=aes(y=lat_dd_avg, label=zone_id),
      #           x=-124, hjust=0, size=1.4, inherit.aes = F, color="grey20") +
      # Plot major mgmt lines
      geom_hline(yintercept=c(48.43333, 46.25000, 42.00000), linewidth=0.2) +
      geom_hline(yintercept = son_mend_county, linetype="dashed", linewidth=0.2) +
      # Plot land
      geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3) +
      geom_sf(data=usa, fill="grey90", color="white", lwd=0.3) +
      # Labels
      labs(x="", y="") +
      # Crop
      coord_sf(xlim = c(-125.5, -119), ylim = c(lat_min, lat_max)) +
      # Theme
      theme_bw() + my_theme +
      theme(axis.text.x=element_text(color="white"))
    g1
  }
  
  # Plot data
  label_size <- 4.5
  if(show_mgmt){
    g2 <- ggplot(sdata_ordered, aes(x=date, y=lat_dd, fill=status)) +
      # Plot raster
      geom_raster() +
      # Management zone lines
      geom_segment(data=zones_df1, mapping=aes(x=x1, xend=x2, y=y, yend=y),
                   inherit.aes = F, color="grey50", linewidth=0.2) +
      geom_text(data=zones, mapping=aes(y=lat_dd_avg, label=zone_id),
                x=date2, hjust=0, size=2.4, inherit.aes = F, color="grey50") +
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
    g2
  }else{
    g2 <- ggplot(sdata_ordered, aes(x=date, y=lat_dd, fill=status)) +
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
    g2
    
  }

  
  # Merge
  g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.25, 0.75))
  
}
