# Download country shapefiles from GADM
source("./data_and_libraries.R")

# download country shapefiles by ISO
# also downloadable at: https://geodata.ucdavis.edu/gadm/gadm4.1/pck/
africa <- c("DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CMR", "CPV", "CAF", "TCD",
            "COM", "COG", "COD", "CIV", "DJI", "EGY", "GNQ", "ERI", "ETH", "GAB",
            "GMB", "GHA", "GIN", "GNB", "KEN", "LSO", "LBR", "LBY", "MDG", "MLI",
            "MWI", "MRT", "MUS", "MYT", "MAR", "MOZ", "NAM", "NER", "NGA", "REU",
            "RWA", "STP", "SEN", "SYC", "SLE", "SOM", "ZAF", "SSD", "SDN", "SWZ",
            "TZA", "TGO", "TUN", "UGA", "ESH", "ZMB", "ZWE")


# save a copy of GADM SpatVector as .rds in the data folder
import_gadm <- function(ISO){

  geodata::gadm(country = ISO, level = 1,
                path = "./", version = "4.1")

}

map(africa, import_gadm)


# create a list of all countries
admin1 <- lapply(africa, function(x){

  list.files(path = "./gadm",
             pattern = paste0("*", x , "_1_pk.rds"), full.names = TRUE)

}) |> unlist()

# unpack shapefiles
unpack_gadm <- function(file){

  object <- readRDS(file) # read in object
  object <- terra::vect(sf::st_as_sf(object)) # unpack SpatVector
  st_as_sf(object) # transform to sf object

}

countries <- map_dfr(admin1, unpack_gadm) # loop over each country
st_crs(countries) # view CRS

# save
saveRDS(countries, './files/countries.rds') # save

plot(countries["COUNTRY"])
