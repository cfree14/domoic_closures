
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

# Read commercial Dungeness crab season key
season_key_orig <- readxl::read_excel(file.path(indir, "season_key.xlsx"))

# Read CA data
data_ca_orig <- readRDS(file=file.path(outdir, "CDFW_2015_2023_comm_dcrab_closures.Rds"))

# Read OR data
data_or_orig <- readRDS(file=file.path(outdir, "ODFW_2011_2023_comm_dcrab_closures.Rds"))

# Read WA data
data_wa_orig <- readRDS(file=file.path(outdir, "PSMFC_2005_2023_comm_dcrab_closures.Rds"))


# Format and merge
################################################################################

# Format CA data
data_ca <- data_ca_orig %>%
  # Reduce to CA
  filter(lat_dd <= 42 & date>= "2015-01-01") %>%
  mutate(status=as.character(status),
         status=recode(status,
                       "30-fathom depth constraint"="30-fathom depth restriction",
                       "40-fathom depth constraint"="40-fathom depth restriction")) %>%
  # Fix a mistake spotted by Christy
  mutate(status=ifelse(lat_dd>=(41+8/60) & date>="2018-12-01" & date < "2019-01-25", "Body condition/domoic acid delay", status))
         # status=ifelse(lat_dd>=(41+8/60) & date>="2019-01-15" & date <= "2019-01-25", "Domoic acid delay", status))

sort(unique(data_ca$status))

# Format OR data
data_or <- data_or_orig %>%
  # Reduce to OR
  filter(lat_dd>42.00000 & lat_dd<46.25000 & date>= "2015-01-01") %>%
  # Recode status
  mutate(status=as.character(status),
         status=recode(status,
                       "body condition"="Body condition delay",
                       "body condition/domoic acid"="Body condition/domoic acid delay",
                       "domoic acid"="Domoic acid delay",
                       "open"="Season open",
                       "out-of-season"="Out-of-season",
                       "evisceration order"="Evisceration order",
                       "evisceration order-OR vessels in WA waters"="Evisceration order"))

sort(unique(data_or$status))


# Format WA data
data_wa <- data_wa_orig %>%
  # Reduce to WA
  filter(lat_dd >= 46.25000 & date>= "2015-01-01") %>%
  # Convert to character
  mutate(status=as.character(status))

sort(unique(data_wa$status))


# Merge data
data <- bind_rows(data_ca, data_or, data_wa) %>%
  # Factor
  mutate(status=factor(status,
                       levels=c("Season open",
                                "Out-of-season",
                                "Body condition delay", # coral
                                "Body condition/domoic acid delay", # darkorange
                                "Domoic acid delay", # darkred,
                                "Evisceration order",
                                "Whale entanglement closure",
                                "30-fathom depth restriction",
                                "40-fathom depth restriction",
                                "50% gear reduction")))

# Export
saveRDS(data, file=file.path(outdir, "2015_2023_WC_dcrab_closures.Rds"))


# Plot data
ggplot(data, aes(x=date, y=lat_dd, fill=status)) +
  # Plot raster
  geom_raster() +
  # Axis
  scale_x_date(date_breaks="1 year", date_labels = "%Y") +
  scale_y_continuous(breaks=35:48, lim=c(35,NA)) +
  # Labels
  labs(x="Date", y="Latitude (°N)") +
  # Legends
  scale_fill_manual(name="Season status", 
                    values=c("grey80", "white", "coral", "darkorange", "darkred", "pink", 
                             "navy", "dodgerblue3", "dodgerblue", "lightblue"), 
                    drop=F) +
  # Theme
  theme_bw()







