
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
data_ca_orig <- readRDS(file=file.path(outdir, "CDFW_2015_2024_comm_dcrab_closures.Rds"))

# Read OR data
data_or_orig <- readRDS(file=file.path(outdir, "ODFW_2011_2024_comm_dcrab_closures.Rds"))

# Read WA data
data_wa_orig <- readRDS(file=file.path(outdir, "PSMFC_2005_2024_comm_dcrab_closures.Rds"))


# Format and merge
################################################################################

# Format CA data
data_ca <- data_ca_orig %>%
  # Reduce to CA
  filter(lat_dd <= 42 & date>= "2015-01-01") %>%
  # Fix last season status
  mutate(status=as.character(status),
        status=ifelse(date>"2025-07-15" | (date>"2025-06-30" & lat_dd<=38.76875), "Out-of-season", status)) %>% 
  # Recode status
  mutate(status=recode(status,
                       "30-fathom depth constraint"="30-fathom depth restriction",
                       "40-fathom depth constraint"="40-fathom depth restriction",
                       "Whale/domoic acid closure"="Whale entanglement/domoic acid delay")) %>%
  # Fix a mistake spotted by Christy
  mutate(status=ifelse(lat_dd>=(41+8/60) & date>="2018-12-01" & date < "2019-01-25", "Body condition/domoic acid delay", status))
         # status=ifelse(lat_dd>=(41+8/60) & date>="2019-01-15" & date <= "2019-01-25", "Domoic acid delay", status))

sort(unique(data_ca$status))

# Format OR data
table(data_or_orig$status)
data_or <- data_or_orig %>%
  # Reduce to OR
  filter(lat_dd>42.00000 & lat_dd<46.25000 & date>= "2015-01-01") %>%
  # Fix last season status
  mutate(status=as.character(status)) %>% 
  mutate(status=ifelse(date>"2025-08-14","out-of-season", status)) %>% 
  # Recode status
  mutate(status=recode(status,
                       "body condition"="Body condition delay",
                       "body condition/domoic acid"="Body condition/domoic acid delay",
                       "domoic acid"="Domoic acid delay",
                       "open"="Season open",
                       "out-of-season"="Out-of-season",
                       "evisceration order"="Evisceration order",
                       "evisceration order-OR vessels in WA waters"="Evisceration order",
                       "evisceration order (+depth restriction/gear reduction)"="Evisceration order (+depth restriction/gear reduction)",
                       "40-fathom depth restriction/20% gear reduction"="40-fathom depth restriction/20% gear reduction"))

sort(unique(data_or$status))


# Format WA data
data_wa <- data_wa_orig %>%
  # Reduce to WA
  filter(lat_dd >= 46.25000 & date>= "2015-01-01") %>%
  # Fix last season status
  mutate(status=as.character(status)) %>% 
  mutate(status=ifelse(date>"2025-09-15","Out-of-season", status))

sort(unique(data_wa$status))


# Merge data
data <- bind_rows(data_ca, data_or, data_wa) 

# Status
sort(unique(data$status))

# Order data
data_ordered <- data %>%
  # Factor
  mutate(status=factor(status,
                       levels=c("Season open",
                                "Out-of-season",
                                "Body condition delay", # coral
                                "Body condition/domoic acid delay", # darkorange
                                "Domoic acid delay", # darkred,
                                "Evisceration order",
                                "Evisceration order (+depth restriction/gear reduction)",
                                "Whale entanglement closure",
                                "Whale entanglement/domoic acid delay",
                                "30-fathom depth restriction",
                                "40-fathom depth restriction",
                                "25% gear reduction",
                                "33% gear reduction",
                                "50% gear reduction",
                                "30-fathom depth constraint/25% gear reduction",        
                                "30-fathom depth constraint/50% gear reduction",
                                "40-fathom depth restriction/20% gear reduction")))

# Makre sure all status levels are complete
freeR::complete(data_ordered)

# Export
saveRDS(data_ordered, file=file.path(outdir, "2015_2024_WC_dcrab_closures.Rds"))


# Plot data
ggplot(data_ordered, aes(x=date, y=lat_dd, fill=status)) +
  # Plot raster
  geom_raster() +
  # Axis
  scale_x_date(date_breaks="1 year", date_labels = "%Y") +
  scale_y_continuous(breaks=35:48, lim=c(35,NA)) +
  # Labels
  labs(x="Date", y="Latitude (°N)") +
  # Legends
  scale_fill_manual(name="Season status", 
                    values=c("grey80", # Season open
                             "white", # Out-of-season
                             "coral", # Body condition delay
                             "darkorange", # Body condition/domoic acid delay
                             "darkred", # Domoic acid delay
                             "pink", # Evisceration order
                             "purple4", # Evisceration order (+depth restriction/gear reduction)
                             "navy", # Whale entanglement closure
                             "purple", # Whale entanglement/domoic acid delay
                             "dodgerblue4", # 30-fathom depth restriction
                             "dodgerblue3", # 40-fathom depth restriction
                             "dodgerblue2", # 25% gear reduction
                             "dodgerblue1", # 33% gear reduction
                             "lightblue", # 50% gear reduction
                             "green", # 30-fathom depth constraint/25% gear reduction
                             "green2", # 30-fathom depth constraint/50% gear reduction
                             "green3"), # 40-fathom depth restriction/20% gear reduction 
                    drop=F) +
  # Theme
  theme_bw()


