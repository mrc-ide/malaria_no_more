# Download country shapefiles from GADM
source("./data_and_libraries.R")

# load data
countries <- readRDS("./files/countries.rds")
dat <- readRDS("./site_output_compiled.rds")
match <- read_csv("./admin1 match.csv")

# read in vector surfaces
CI_surface <- raster("./2024_GBD2023_Global_Pf_Incidence_Rate_2000.tif")

# mask the raster to the exact shape of the sf object
extent <- st_bbox(countries)
extent_r <- extent(c(extent$xmin, extent$xmax, extent$ymin, extent$ymax))
r_cropped <- crop(CI_surface, extent_r)
r_masked <- mask(r_cropped, as(countries, "Spatial"))

# simplify dataset to year 2000
dat_2000 <- dat |> filter(year == 2000) |>
  group_by(country, site_name, year) |>
  summarize(cases = sum(cases, na.rm = TRUE),
            cohort_size = sum(cohort_size, na.rm = TRUE)) |>
  mutate(clinical = cases / cohort_size)

# check that there is only one entry per admin1
dat_2000 |> dplyr::select(country, site_name) |> distinct() |> nrow() 

# join to countries 
dat_join <- dat_2000 |> left_join(match, by = c("country" = "ICL_country", "site_name" = "ICL_site_name"))
countries_s <- countries |> left_join(dat_join, by = c("GID_0" = "GADM_GID_0", "NAME_1" = "GADM_NAME_1")) 

match <- countries_s |> filter(is.na(clinical))

write_csv(dat_2000, "./match1.csv")
write_csv(countries |> st_drop_geometry(), "./match2.csv")

test <- read_csv("./match2.csv")


# make centroids
library(lwgeom)
sf_object <- st_make_valid(countries_s)
centroids <- st_centroid(sf_object)

A <- ggplot() + 
  geom_sf(data = countries, fill = NA, color = "darkgrey", size = .2) + 
  geom_sf(data = countries_s |> filter(!is.na(clinical)), aes(fill = clinical), size = 1, show.legend = FALSE) +
  labs(x = "", y = "", fill = "clinical incidence", title = "polygons") +
  theme_bw(base_size = 12) +
  scale_fill_gradientn(colors = met.brewer("Tam", direction = -1)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

B <- ggplot() + 
  geom_sf(data = countries, fill = NA, color = "darkgrey", size = .2) + 
  geom_sf(data = centroids, aes(color = clinical), size = 1, show.legend = FALSE) +
  labs(x = "", y = "", color = "clinical incidence", title = "centroids") +
  theme_bw(base_size = 12) +
  scale_color_gradientn(colors = met.brewer("Tam", direction = -1)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

C <- ggplot() + 
  geom_sf(data = countries, fill = NA, color = "darkgrey", size = .2) + 
  geom_raster(data = as.data.frame(rasterToPoints(r_masked)),
              aes(x = x, y = y, fill = X2024_GBD2023_Global_Pf_Incidence_Rate_2000_1), alpha = 0.7, show.legend = FALSE) +
  scale_x_continuous(limits = c(bbox[1], bbox[3])) +
  scale_y_continuous(limits = c(bbox[2], bbox[4])) +
  labs(x = "", y = "", fill = "", title = "raster") +
  theme_bw(base_size = 12) +
  scale_fill_gradientn(colors = met.brewer("Tam", direction = -1)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

# save
p <- A + B + C 

ggsave("./plotcompare.pdf", plot = p, width = 8, height = 4)


