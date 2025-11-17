
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(rerddap)
library(tidyverse)

# Directories
outdir <- "data/calhabmap/processed"
plotdir <- "data/calhabmap/figures"

# Base URL
base_url <- "https://erddap.sccoos.org/erddap/"

# Datasets
out <- ed_search(query = "HABS-", which = "tabledap", url = base_url)
dataset_ids <- out$info$dataset_id

# To-do list
# 1) Fully understand variables and adjust variable names in long
# 2) Order categories
# 3) Order variables within categories
# 4) Get sizing of figure/habitat right


# Merge data
################################################################################

# Loop through datasets and merge
data_orig <- purrr::map_df(dataset_ids, function(x){
  
  # Download
  df <- tabledap(x = x,
                 url = base_url)
  df_out <- df %>% 
    # Add dataset id
    mutate(dataset_id=x) %>% 
    # Format sample id
    mutate(SampleID=as.character(SampleID))
  
  
})


# Build data
################################################################################

# Build data
colnames(data_orig)
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(lat_dd=latitude,
         long_dd=longitude, 
         temp_c=temp,
         air_temp_c=air_temp,
         # Productivity
         chl_volume_filtered_ml=chl_volume_filtered,
         chl1_mg_m3=chl1,
         chl2_mg_m3=chl2,
         chl_avg_mg_m3=avg_chloro,
         phaeo1_mg_m3=phaeo1,
         phaeo2_mg_m3=phaeo2,
         phaeo_avg_mg_m3=avg_phaeo,
         # Nutrients
         phosphate_uM=phosphate,
         silicate_uM=silicate,
         nitrite_uM=nitrite,
         nitrite_nitrate_uM = nitrite_nitrate,
         ammonium_uM = ammonium,
         nitrate_uM = nitrate,
         # Domoic acid
         da_volume_filtered_ml=da_volume_filtered,
         pda_ng_ml=p_da,
         dda_ng_ml=d_da,
         tda_ng_ml=t_da,
         # Phytoplankton
         volume_settled_for_counting_ml = volume_settled_for_counting,         
         akashiwo_sanguinea_cells_l = akashiwo_sanguinea,                  
         alexandrium__cells_l = alexandrium_spp,                    
         dinophysis_cells_l = dinophysis_spp,                      
         lingulodinium_polyedra_cells_l = lingulodinium_polyedra,             
         prorocentrum__cells_l = prorocentrum_spp,                   
         pseudo_nitzschia_delicatissima_cells_l = pseudo_nitzschia_delicatissima_group,
         pseudo_nitzschia_seriata_cells_l = pseudo_nitzschia_seriata_group,    
         ceratium_cells_l = ceratium_spp,                       
         cochlodinium_cells_l  = cochlodinium_spp,                   
         gymnodinium_cells_l = gymnodinium_spp,                    
         other_diatoms_cells_l = other_diatoms,                      
         other_dinoflagellates_cells_l = other_dinoflagellates,             
         total_phytoplankton_cells_l = total_phytoplankton) %>% 
  # Add date
  mutate(date=substr(time, 1, 10) %>% lubridate::ymd(.),
         year=lubridate::year(date),
         month=lubridate::month(date)) %>% 
  # Add location 
  mutate(location=gsub("HABs-", "", dataset_id) %>% 
           str_replace_all(., "(?<!^)([A-Z])", " \\1")) %>% 
  # Arrange
  select(dataset_id, location, location_code:sample_id, 
         year, month, date, time, everything())

# Site key
site_key <- data %>% 
  group_by(dataset_id, location, location_code, lat_dd, long_dd) %>% 
  summarize(nyr=n_distinct(year),
            nobs=n()) %>% 
  ungroup() %>% 
  arrange(lat_dd)


# Export
saveRDS(data, file=file.path(outdir, "calhabmap_data.Rds"))
saveRDS(site_key, file=file.path(outdir, "calhabmap_sites.Rds"))


# Long version
################################################################################

# Data long
data_long <- data %>% 
  # Gather
  gather(key="variable", value="value", 12:ncol(.)) %>% 
  # Categorize
  mutate(category=case_when(grepl("cells_l|settled", variable) ~ "Phytoplankton\n(cells/L)",
                            grepl("mg_m3|chl_", variable) ~ "Productivity\n(mg/m3)",
                            grepl("uM", variable) ~ "Nutrients\n(uM)",
                            grepl("_ng_ml|da_", variable) ~"Domoic\nacid (ng/ml)",
                            grepl("temp|salinity", variable) ~ "Physical",
                            T ~ NA)) %>% 
  # Format variables
  mutate(variable=gsub("_cells_l|_uM|_mg_m3|_ng_ml", "", variable),
         variable=gsub("_", " ", variable) %>% stringr::str_to_sentence(.) %>% stringr::str_squish(),
         variable=recode(variable,
                         "Pda"="Particulate domoic acid",
                         "Tda"="Total domoic acid",
                         "Da volume filtered ml"="DA volume filtered (ml)",
                         "Nitrite nitrate"="Nitrite-nitrate",
                         # Prod
                         "Average chlorophyll"="Chl avg",
                         "Chlorophyll-1"="Chl1",
                         "Chlorophyll-2"="Chl2",
                         # Physcial
                         "Air temp c"="Air temperature (°C)",
                         "Temp c"="Water temperature (°C)",
                         # Phytoplanton
                         "Alexandrium"="Alexandrium spp.",
                         "Ceratium"="Ceratium spp.",
                         "Cochlodinium"="Cochlodinium spp.",
                         "Dinophysis"="Dinophysis spp.",
                         "Gymnodinium"="Gymnodinium spp.",
                         "Prorocentrum"="Prorocentrum spp.",
                         "Pseudo nitzschia delicatissima"="Pseudo-nitzschia delicatissima",
                         "Pseudo nitzschia seriata"="Pseudo-nitzschia seriata",
                         "Volume settled for counting ml"="Phytoplankton volume settled (ml)"))

# Variable key
variable_key <- data_long %>% 
  filter(!is.na(value)) %>% 
  group_by(category, location, variable) %>% 
  summarize(nobs=n(),
            nyrs=n_distinct(year)) %>% 
  ungroup()


# Map data
################################################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
world <- rnaturalearth::ne_countries(country = c("Mexico", "Canada"), returnclass = "sf", scale="small")

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_blank(),
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

# Plot land
g1 <- ggplot() +
  geom_sf(data=world, fill="grey80", color="white", lwd=0.2, inherit.aes = F) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.2, inherit.aes = F) +
  # Point
  geom_point(site_key, mapping=aes(x=long_dd, y=lat_dd)) +
  ggrepel::geom_text_repel(site_key, mapping=aes(x=long_dd, y=lat_dd, label=location), 
            hjust=0, size=2.5) +
  # Labels
  labs(x="", y="", tag="A") +
  # Crop
  coord_sf(xlim=c(-116,-125), ylim=c(32.5, 42)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g1

# Plot data
g2 <- ggplot(variable_key, mapping = aes(y=factor(location, levels=site_key$location), 
                                   x=variable, fill=nyrs)) +
  facet_grid(~category, space="free_x", scales="free_x") +
  geom_tile() +
  # Labels
  labs(x="", y="", tag="B") +
  # Legend
  scale_fill_gradientn(name="# of\nyears", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.3, 0.7))

# Export
ggsave(g, filename=file.path(plotdir, "calhabmap_site_map_and_variables.png"), 
       width=9.5, height=4.5, units="in", dpi=600)


# Plot data
################################################################################

ggplot(data, aes(y=factor(location, levels=site_key$location), 
                          x=date, size=temp_c, color=temp_c)) +
  geom_point() +
  # Labels
  labs(x="Date", y="") +
  #  Legend
  scale_size_continuous(name="Temp (°C)") +
  scale_color_gradientn(name="Temp (°C)", colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Theme
  theme_bw()

ggplot(data, aes(y=factor(location, levels=site_key$location), 
                 x=date, size=pda_ng_ml, color=pda_ng_ml)) +
  geom_point() +
  # Labels
  labs(x="Date", y="") +
  #  Legend
  scale_size_continuous(name="pDA (ng/mL)") +
  scale_color_gradientn(name="pDA (ng/mL)", colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Theme
  theme_bw()


