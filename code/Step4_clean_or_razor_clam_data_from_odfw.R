
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
data_orig <- readxl::read_excel(file.path(indir, "Oregon Historic Biotoxin Closures.xlsx"))

# Read landmarks
zones <- readxl::read_excel(file.path(indir, "or_landmarks.xlsx"), sheet="Zones")
landmarks <- readxl::read_excel(file.path(indir, "or_landmarks.xlsx"), sheet="Landmarks") %>% 
  select(-long_dd)



# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names() %>% 
  rename(date1=start,
         date2=end) %>% 
  # Remove empty row
  slice(2:nrow(.)) %>% 
  # Fill year
  fill(year, .direction = "down") %>% 
  # Format species
  mutate(species=stringr::str_to_sentence(species)) %>% 
  # Fix dates
  mutate(month1=month(date1),
         day1=day(date1),
         date1=paste(year, month1, day1, sep="-") %>% ymd(.),
         month2=month(date2),
         day2=day(date2),
         date2=paste(year, month2, day2, sep="-") %>% ymd(.)) %>% 
  select(-c(month1:day2)) %>% 
  # Check days closed
  mutate(days_closed_calc=difftime(date2, date1, units="days") %>% as.numeric(),
         days_closed_check=days_closed_calc-days_closed) %>% 
  # Use calculated days closed
  mutate(days_closed=days_closed_calc) %>% 
  select(-c(days_closed_calc, days_closed_check)) %>% 
  # Add zones
  left_join(zones, by=c("area")) %>% 
  # Add landmarks
  left_join(landmarks, by=c("landmark1"="landmark")) %>% 
  rename(lat_dd1=lat_dd) %>% 
  left_join(landmarks, by=c("landmark2"="landmark")) %>% 
  rename(lat_dd2=lat_dd) %>% 
  # Add id
  mutate(id=1:nrow(.)) %>% 
  # Format toxin
  mutate(biotoxin=recode(biotoxin,
                         "DA"="Domoic acid",
                         "PST"="Paralytic shellfish toxin"))


# Inspect data
str(data)
table(data$biotoxin)
table(data$species)


# Plot data
################################################################################

# Subset
sdata <- data %>% 
  filter(species=="Razor clams")

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
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
ggplot(data, aes(xmin=date1, xmax=date2, ymin=lat_dd1, ymax=lat_dd2, fill=biotoxin, group=id)) +
  geom_rect() +
  # Labels
  labs(y="Latitude (°N)") +
  # Axis
  scale_x_date(lim=c(ymd("2000-01-01"), NA),
               breaks = seq(ymd("1970-01-01"), ymd("2030-01-01"), by = "5 years"),  
               date_labels = "%Y") + 
  # Legend
  scale_fill_discrete(name="Closure type") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "top")








