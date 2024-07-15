# Exporting MAP and GADM global admins as ESRI shapefiles
source("./data_and_libraries.R")

# read in data
MAP_admin1 <- readRDS("./files/world_1.rds")
GADM_admin0 <- readRDS("./files/GADM_world_0.rds")

# assign a variable for a random number
MAP_admin1$rnumber <-  sample(1:100, nrow(MAP_admin1), replace = TRUE)
GADM_admin0$rnumber <-  sample(1:100, nrow(GADM_admin0), replace = TRUE)

# plot
ggplot(data = MAP_admin1) +
  geom_sf(aes(fill = rnumber), linewidth = 0.05)

ggplot(data = GADM_admin0) +
  geom_sf(aes(fill = rnumber), linewidth = 0.05)

# save as shapefile
st_write(MAP_admin1, paste0("./files/shp results/MAP_admin1.shp"), append = FALSE, overwrite = TRUE)
st_write(GADM_admin0, paste0("./files/shp results/GADM_admin0.shp"), append = FALSE, overwrite = TRUE)
