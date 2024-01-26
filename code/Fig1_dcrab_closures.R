
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
data <- readRDS(data, file=file.path(outdir, "2015_2023_WC_dcrab_closures.Rds"))


# Plot data
################################################################################

# Sonoma-Mendocino county line
son_mend_county <- 38+46.125/60
date_min_do <- min(data$date)

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
g <- ggplot(data, aes(x=date, y=lat_dd, fill=status)) +
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
                    values=c("grey85", "white", "pink", "orange", "darkred", "coral", 
                             "navy", "dodgerblue3", "dodgerblue1", "lightblue"), 
                    drop=F) +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_dcrab_closures.png"),
       width=6.5, height=3.25, units="in", dpi=600)





