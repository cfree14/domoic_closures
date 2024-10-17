
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
datadir <- "data"
indir <- "data/raw"
outdir <- "data/processed"
plotdir <- "figures"

# Read data
data_orig <- readRDS(data, file=file.path(outdir, "2015_2023_WC_dcrab_closures.Rds"))

# Read zones
zones_orig <- readxl::read_excel(file.path(datadir, "WC_dcrab_da_mgmt_zones.xlsx"))


# Format data
################################################################################

# Inspect status
table(data_orig$status)

# Format data
data <- data_orig %>% 
  # Make entanglement regs same
  mutate(status=recode(status, 
                       "Whale entanglement closure"="Entanglement-related restriction",
                       "30-fathom depth restriction"="Entanglement-related restriction",
                       "40-fathom depth restriction"="Entanglement-related restriction",
                       "50% gear reduction"="Entanglement-related restriction"))

# Format data
table(data$status)


# Build zones data
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
zones1 <- zones_orig %>%
  filter(state=="Washington") %>%
  mutate(season="2015-16 season") %>%
  select(season, everything())
zones2 <- zones_orig %>%
  mutate(season="2020-21 season") %>%
  select(season, everything())
zones <- bind_rows(zones1, zones2) %>%
  mutate(lat_dd_avg=(lat_dd_north+lat_dd_south)/2) %>%
  # Alter Zone I lat
  mutate(lat_dd_avg=ifelse(zone_id=="H", 35.3, lat_dd_avg))



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
  # Management zone lines
  geom_segment(data=zones_df, mapping=aes(x=x1, xend=x2, y=y, yend=y),
               inherit.aes = F, color="grey50", linewidth=0.2) +
  geom_text(data=zones, mapping=aes(y=lat_dd_avg, label=zone_id),
            x=ymd("2023-10-01"), hjust=0, size=1.4, inherit.aes = F, color="grey50") +
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
                    values=c("grey85", "white", "pink", "orange", "red3", "coral", 
                             "dodgerblue3"), # "navy", "dodgerblue3", "dodgerblue1", "lightblue"
                    drop=F) +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigX_dcrab_closures_for_steph.png"),
       width=6.5, height=3.25, units="in", dpi=600)
ggsave(g, filename=file.path(plotdir, "FigX_dcrab_closures_for_steph.pdf"),
       width=6.5, height=3.25, units="in", dpi=600)




