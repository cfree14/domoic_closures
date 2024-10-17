
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
plotdir <- "figures"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "OR razor clam closures.xlsx"))

# Read landmarks
landmarks_orig <- readxl::read_excel(file.path(indir, "OR razor clam closures.xlsx"), sheet="Common landmarks")


# Format data
################################################################################

# Clean landmarks
landmarks <- landmarks_orig %>% 
  janitor::clean_names("snake") %>% 
  select(landmark, latitude, longitude) %>% 
  filter(!is.na(landmark))

# Step 1. Basic formatting
data <- data_orig %>%
  # Rename
  janitor::clean_names("snake") %>% 
  rename(action=action_close_open,
         lat_n=n_latitude_n,
         lat_s=s_latitude_n) %>% 
  # Format action
  mutate(action=recode(action,
                       "Closed"="Close",
                       "Closure"="Close")) %>% 
  # Format reason
  mutate(reason=stringr::str_to_sentence(reason)) %>% 
  # Format date
  mutate(date_of_action=ymd(date_of_action),
         date_of_news=ymd(date_of_news)) %>% 
  # Format action for grid step
  mutate(action_use=ifelse(action=="Close", reason, action))

# Inspect data
str(data)
range(data$date_of_action)
range(data$date_of_news)
table(data$species)
table(data$fishery_type)
table(data$action)
table(data$reason)
table(data$action_use)


# Build gridded data
################################################################################

# Build empty grid
date1 <- ymd("2010-01-01")
date2 <- ymd("2024-10-31")
dates <- seq(date1, date2, by="1 day")
lat1 <- 42
lat2 <- 46.3
lats <- seq(lat1, lat2, 0.01)
closure_grid <- expand.grid(date=dates, lat_dd=lats) %>%
  as.data.frame() %>%
  mutate(status="Open",
         comm_name="Razor clam") %>%
  select(comm_name, date, lat_dd, status) %>%
  arrange(date, lat_dd)
  
# Loop through announcements
i <- 1
for(i in 1:nrow(data)){
  
  # Get announcement
  print(paste(i, "of", nrow(data)))
  date1 <- data %>% slice(i) %>% pull(date_of_action)
  lat_s <- data %>% slice(i) %>% pull(lat_s)
  lat_n <- data %>% slice(i) %>% pull(lat_n)
  status_new <- data %>% slice(i) %>% pull(action_use)
  
  # Apply announcement
  closure_grid <- closure_grid %>%
    mutate(status=ifelse(lat_dd>=lat_s & lat_dd <= lat_n & date>=date1, status_new, status))
  
}

# Format closure grid
closure_grid2 <- closure_grid %>% 
  mutate(status=factor(status, levels=c("Open", "Annual conservation closure", "Domoic acid closure")))


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = "top",
                   legend.key.size = unit(0.3, "cm"),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0))) 

# Plot closure grid
g <- ggplot(closure_grid2, aes(x=date, y=lat_dd, fill=status)) +
  # Plot raster
  geom_raster() +
  # Plot events
  geom_segment(data=data, mapping=aes(x=date_of_action, 
                                      xend=date_of_action, 
                                      y=lat_s, yend=lat_n, 
                                      linetype=action), inherit.aes = F) +
  # Axis
  scale_x_date(date_breaks="1 year", date_labels = "%Y") +
  scale_y_continuous(breaks=42:46,
                     sec.axis = sec_axis(
                       transform = ~ .,
                       name = "", 
                       breaks = landmarks$latitude,
                       labels = landmarks$landmark
                     )) +
  # Labels
  labs(x="", y="Latitude (°N)") +
  # Legends
  scale_fill_manual(name="Season status", values=c("white", "grey80", "coral"), drop=F) +
  scale_linetype_discrete(name="Action type") +
  guides(fill = guide_legend( title.position = "top", title.hjust = 0.5),
         linetype = guide_legend( title.position = "top", title.hjust = 0.5)) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "2010_2024_or_razor_clam_closures.png"), 
       width=6.5, height=4.5, units="in", dpi=600)

# Zoom in on 2016 forwards

# Plot closure grid
g <- ggplot(closure_grid2 %>% filter(date>=lubridate::ymd("2016-01-01")), 
            aes(x=date, y=lat_dd, fill=status)) +
  # Plot raster
  geom_raster() +
  # Plot events
  geom_segment(data=data %>% filter(date_of_action>=lubridate::ymd("2016-01-01")), 
               mapping=aes(x=date_of_action, 
                          xend=date_of_action, 
                          y=lat_s, yend=lat_n, 
                          linetype=action), inherit.aes = F) +
  # Axis
  scale_x_date(date_breaks="1 year", date_labels = "%Y") +
  scale_y_continuous(breaks=42:46,
                     sec.axis = sec_axis(
                       transform = ~ .,
                       name = "", 
                       breaks = landmarks$latitude,
                       labels = landmarks$landmark
                     )) +
  # Labels
  labs(x="", y="Latitude (°N)") +
  # Legends
  scale_fill_manual(name="Season status", values=c("white", "grey80", "coral"), drop=F) +
  scale_linetype_discrete(name="Action type") +
  guides(fill = guide_legend( title.position = "top", title.hjust = 0.5),
         linetype = guide_legend( title.position = "top", title.hjust = 0.5)) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "2016_2024_or_razor_clam_closures.png"), 
       width=6.5, height=4.5, units="in", dpi=600)


