# Download country shapefiles from GADM
source("./data_and_libraries.R")

# download country shapefiles by ISO
# also downloadable at: https://geodata.ucdavis.edu/gadm/gadm4.1/pck/
all_countries <- readRDS("./files/countries_GADM.rds") |>
  dplyr::select(ISO3) |>
  as_vector()

africa <- c("DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CMR", "CPV", "CAF", "TCD",
            "COM", "COG", "COD", "CIV", "DJI", "EGY", "GNQ", "ERI", "ETH", "GAB",
            "GMB", "GHA", "GIN", "GNB", "KEN", "LSO", "LBR", "LBY", "MDG", "MLI",
            "MWI", "MRT", "MUS", "MYT", "MAR", "MOZ", "NAM", "NER", "NGA", "REU",
            "RWA", "STP", "SEN", "SYC", "SLE", "SOM", "ZAF", "SSD", "SDN", "SWZ",
            "TZA", "TGO", "TUN", "UGA", "ESH", "ZMB", "ZWE")


# save a copy of GADM SpatVector as .rds in the data folder
import_gadm <- function(ISO, level){

  geodata::gadm(country = ISO, level = level,
                path = HPCpath, version = "4.0") # version from site files

}

# run through import function
map2(all_countries, 0, import_gadm) # download SWZ from website manually, getting error
map2(africa, 1, import_gadm)


# create a list of all countries
admin0 <- lapply(all_countries, function(x){
  
  list.files(path = paste0(HPCpath, "gadm"),
             pattern = paste0("*", x , "_0_pk.rds"), full.names = TRUE)
  
}) |> unlist()

admin1 <- lapply(africa, function(x){

  list.files(path = paste0(HPCpath, "gadm"),
             pattern = paste0("*", x , "_1_pk.rds"), full.names = TRUE)

}) |> unlist()


# unpack shapefiles
unpack_gadm <- function(file){

  object <- readRDS(file) # read in object
  object <- terra::vect(sf::st_as_sf(object)) # unpack SpatVector
  st_as_sf(object) # transform to sf object

}

GADM_world_0 <- map_dfr(admin0, unpack_gadm) # loop over each country
GADM_SSA_1 <- map_dfr(admin1, unpack_gadm) # loop over each country

st_crs(GADM_world_0); st_crs(GADM_SSA_1) # view CRS

# save
saveRDS(GADM_world_0, "./files/GADM_world_0.rds") # save
saveRDS(GADM_SSA_1, "./files/GADM_SSA_1.rds") # save

# quick look
plot(GADM_world_0["COUNTRY"])

