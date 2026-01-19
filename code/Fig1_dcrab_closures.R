
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
indir <- "data/raw"
outdir <- "data/processed"
plotdir <- "figures"

# Read data
data_orig <- readRDS(data, file=file.path(outdir, "2015_2024_WC_dcrab_closures.Rds"))

# Format data
levels(data_orig$status)
levels_use <- c( "Season open", 
                 "Out-of-season", 
                 "Body condition delay", 
                 "Body condition/domoic acid delay", 
                 "Domoic acid delay", 
                 "Evisceration order",                                
                 "Evisceration order (+depth/gear restriction)", 
                 "Whale entanglement/domoic acid delay",
                 "Whale entanglement closure",                            
                 "30-fathom depth restriction", 
                 "40-fathom depth restriction",                           
                 "25% gear reduction", 
                 "33% gear reduction", 
                 "50% gear reduction",
                 "30-fathom depth constraint/25% gear reduction",
                 "30-fathom depth constraint/50% gear reduction",
                 "40-fathom depth restriction/20% gear reduction")

data1 <- data_orig %>%
  mutate(status = factor(status, labels = levels_use))


# Plot data
################################################################################

# Sonoma-Mendocino county line
son_mend_county <- 38+46.125/60
date_min_do <- min(data_orig$date)

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.4, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data1, aes(x=date, y=lat_dd, fill=status)) +
  # Plot raster
  geom_raster() +
  # State/region lines
  geom_hline(yintercept=c(48.43333, 46.25000, 42.00000), linewidth=0.2) +
  geom_hline(yintercept = son_mend_county, linetype="dashed", linewidth=0.2) + # Sonoma/Mendocino
  # Label state lines
  annotate(geom="text", x=date_min_do, y=48.48, hjust=0, vjust=1.5, label="Washington", color="black", size=2.5) +
  annotate(geom="text", x=date_min_do, y=46.25, hjust=0, vjust=1.5, label="Oregon", color="black", size=2.5) +
  annotate(geom="text", x=date_min_do, y=42, hjust=0, vjust=1.5, label="N. California", color="black", size=2.5) +
  annotate(geom="text", x=date_min_do, y=son_mend_county, hjust=0, vjust=1.5, label="C. California", color="black", size=2.5) +
  # Axis
  scale_x_date(date_breaks="1 year", date_labels = "%Y") +
  scale_y_continuous(breaks=35:48, lim=c(35,NA)) +
  # Labels
  labs(x="Date", y="Latitude (°N)") +
  # Legends
  scale_fill_manual(name="Season status", 
                    values=c("grey85", # Season open
                             "white", # Out-of-season
                             "pink", # Body condition delay
                             "orange", # Body condition/domoic acid delay
                             "darkred", # Domoic acid delay
                             "coral", # Evisceration order
                             "purple2", # Evisceration order (+depth/gear restriction)
                             "purple4", # Whale entanglement/domoic acid delay
                             "navy", # Whale entanglement closure
                             "dodgerblue3", # 30-fathom depth restriction
                             "dodgerblue1", # 40-fathom depth restriction
                             "lightblue1", # 25% gear reduction 
                             "lightblue2", # 33% gear reduction
                             "lightblue3", # 50% gear reduction
                             "springgreen1",  # 30-fathom depth constraint/25% gear reduction
                             "springgreen3",  # 30-fathom depth constraint/50% gear reduction
                             "springgreen4" # 40-fathom depth restriction/20% gear reduction
                             ),
                    drop=F) +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_dcrab_closures.png"),
       width=6.5, height=3.25, units="in", dpi=600)


# Plot data - simple
################################################################################

# Recode
data2 <- data_orig %>% 
  mutate(status=as.character(status) %>% as.vector(),
         status=recode_factor(status, 
                              "Season open"="Season open", 
                              "Out-of-season"="Out-of-season", 
                              "Body condition delay"="Body condition delay", 
                              "Body condition/domoic acid delay"="Body condition/domoic acid delay", 
                              "Domoic acid delay"="Domoic acid delay", 
                              "Evisceration order"="Evisceration order",                               
                              "Evisceration order (+depth restriction/gear reduction)"="Evisceration order (+depth/gear restriction)", 
                              "Whale entanglement/domoic acid delay" = "Whale entanglement/domoic acid delay",
                              "Whale entanglement closure" = "Whale entanglement closure",                            
                              "30-fathom depth restriction" = "Depth restriction", 
                              "40-fathom depth restriction" = "Depth restriction",                           
                              "25% gear reduction" = "Gear reduction", 
                              "33% gear reduction" = "Gear reduction",
                              "50% gear reduction" = "Gear reduction",
                              "40-fathom depth restriction/20% gear reduction" = "Depth restriction/gear reduction",
                              "30-fathom depth constraint/25% gear reduction" = "Depth restriction/gear reduction",
                              "30-fathom depth constraint/50% gear reduction" = "Depth restriction/gear reduction"))
levels(data2$status)

# Plot data
g2 <- ggplot(data2, aes(x=date, y=lat_dd, fill=status)) +
  # Plot raster
  geom_raster() +
  # State/region lines
  geom_hline(yintercept=c(48.43333, 46.25000, 42.00000), linewidth=0.2) +
  geom_hline(yintercept = son_mend_county, linetype="dashed", linewidth=0.2) + # Sonoma/Mendocino
  # Label state lines
  annotate(geom="text", x=date_min_do, y=48.48, hjust=0, vjust=1.5, label="Washington", color="black", size=2.5) +
  annotate(geom="text", x=date_min_do, y=46.25, hjust=0, vjust=1.5, label="Oregon", color="black", size=2.5) +
  annotate(geom="text", x=date_min_do, y=42, hjust=0, vjust=1.5, label="N. California", color="black", size=2.5) +
  annotate(geom="text", x=date_min_do, y=son_mend_county, hjust=0, vjust=1.5, label="C. California", color="black", size=2.5) +
  # Axis
  scale_x_date(date_breaks="1 year", date_labels = "%Y") +
  scale_y_continuous(breaks=35:48, lim=c(35,NA)) +
  # Labels
  labs(x="Date", y="Latitude (°N)") +
  # Legends
  scale_fill_manual(name="Season status", 
                    values=c("grey85", # Season open
                             "white", # Out-of-season
                             "pink", # Body condition delay
                             "orange", # Body condition/domoic acid delay
                             "darkred", # Domoic acid delay
                             "coral", # Evisceration order
                             "purple2", # Evisceration order (+depth/gear restriction)
                             "purple4", # Whale entanglement/domoic acid delay
                             "navy", # Whale entanglement closure 
                             "lightblue1", # Depth restriction
                             "dodgerblue", # Gear reduction
                             "dodgerblue3"), # Depth restriction/gear reduction
                    drop=F) +
  # Theme
  theme_bw() + my_theme
g2

# Export plot
ggsave(g2, filename=file.path(plotdir, "FigX_dcrab_closures_simple.png"),
       width=6.5, height=3.25, units="in", dpi=600)


# Plot data - last 5 years
################################################################################

date_min_do1 <- ymd("2020-09-20")

# Plot data
g <- ggplot(data1 %>% filter(date >= date_min_do1), aes(x=date, y=lat_dd, fill=status)) +
  # Plot raster
  geom_raster() +
  # State/region lines
  geom_hline(yintercept=c(48.43333, 46.25000, 42.00000), linewidth=0.2) +
  geom_hline(yintercept = son_mend_county, linetype="dashed", linewidth=0.2) + # Sonoma/Mendocino
  # Label state lines
  annotate(geom="text", x=date_min_do1, y=48.48, hjust=0, vjust=1.5, label="Washington", color="black", size=2.5) +
  annotate(geom="text", x=date_min_do1, y=46.25, hjust=0, vjust=1.5, label="Oregon", color="black", size=2.5) +
  annotate(geom="text", x=date_min_do1, y=42, hjust=0, vjust=1.5, label="N. California", color="black", size=2.5) +
  annotate(geom="text", x=date_min_do1, y=son_mend_county, hjust=0, vjust=1.5, label="C. California", color="black", size=2.5) +
  # Axis
  scale_x_date(date_breaks="1 year", date_labels = "%Y") +
  scale_y_continuous(breaks=35:48, lim=c(35,NA)) +
  # Labels
  labs(x="Date", y="Latitude (°N)") +
  # Legends
  scale_fill_manual(name="Season status", 
                    values=c("grey85", # Season open
                             "white", # Out-of-season
                             "pink", # Body condition delay
                             "orange", # Body condition/domoic acid delay
                             "darkred", # Domoic acid delay
                             "coral", # Evisceration order
                             "purple2", # Evisceration order (+depth/gear restriction)
                             "purple4", # Whale entanglement/domoic acid delay
                             "navy", # Whale entanglement closure
                             "dodgerblue3", # 30-fathom depth restriction
                             "dodgerblue1", # 40-fathom depth restriction
                             "lightblue1", # 25% gear reduction 
                             "lightblue2", # 33% gear reduction
                             "lightblue3", # 50% gear reduction
                             "springgreen1",  # 30-fathom depth constraint/25% gear reduction
                             "springgreen3",  # 30-fathom depth constraint/50% gear reduction
                             "springgreen4" # 40-fathom depth restriction/20% gear reduction
                    ),
                    drop=F) +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_dcrab_closures_5yrs.png"),
       width=6.5, height=3.25, units="in", dpi=600)


# Plot data - last 
################################################################################

# Plot data
g2 <- ggplot(data2 %>% filter(date >= date_min_do1), aes(x=date, y=lat_dd, fill=status)) +
  # Plot raster
  geom_raster() +
  # State/region lines
  geom_hline(yintercept=c(48.43333, 46.25000, 42.00000), linewidth=0.2) +
  geom_hline(yintercept = son_mend_county, linetype="dashed", linewidth=0.2) + # Sonoma/Mendocino
  # Label state lines
  annotate(geom="text", x=date_min_do1, y=48.48, hjust=0, vjust=1.5, label="Washington", color="black", size=2.5) +
  annotate(geom="text", x=date_min_do1, y=46.25, hjust=0, vjust=1.5, label="Oregon", color="black", size=2.5) +
  annotate(geom="text", x=date_min_do1, y=42, hjust=0, vjust=1.5, label="N. California", color="black", size=2.5) +
  annotate(geom="text", x=date_min_do1, y=son_mend_county, hjust=0, vjust=1.5, label="C. California", color="black", size=2.5) +
  # Axis
  scale_x_date(date_breaks="1 year", date_labels = "%Y") +
  scale_y_continuous(breaks=35:48, lim=c(35,NA)) +
  # Labels
  labs(x="Date", y="Latitude (°N)") +
  # Legends
  # Legends
  scale_fill_manual(name="Season status", 
                    values=c("grey85", # Season open
                             "white", # Out-of-season
                             "pink", # Body condition delay
                             "orange", # Body condition/domoic acid delay
                             "darkred", # Domoic acid delay
                             "coral", # Evisceration order
                             "purple2", # Evisceration order (+depth/gear restriction)
                             "purple4", # Whale entanglement/domoic acid delay
                             "navy", # Whale entanglement closure 
                             "lightblue1", # Depth restriction
                             "dodgerblue", # Gear reduction
                             "dodgerblue3"), # Depth restriction/gear reduction
                    drop=F) +
  # Theme
  theme_bw() + my_theme
g2

# Export plot
ggsave(g2, filename=file.path(plotdir, "FigX_dcrab_closures_simple_5yrs.png"),
       width=6.5, height=3.25, units="in", dpi=600)
