# Allocate annual WMR cases and deaths over the year 
source("./data_and_libraries.R")


# read in data
dat <- read_csv("M:/Lydia/VIMC_malaria/montagu/central-burden-est-no-vaccination.csv")

country = "BFA"
weights <- readRDS(paste0("./files/seasonal weights/", country, ".rds"))

year <- seq(2000, 2010, 1)

weights <- crossing(weights, year)

BFA <- dat |> filter(country == "BFA" & year %in% seq(2000, 2020, 1)) |>
  group_by(year, country, country_name) |>
  summarize(cohort_size = sum(cohort_size),
            annual_cases = sum(cases),
            annual_deaths = sum(deaths)) |>
  left_join(weights, by = "year") |>
  mutate(cases = annual_cases * sum,
         deaths = annual_deaths * sum) |>
  mutate(case_inc = cases / cohort_size,
         death_inc = deaths / cohort_size) |>
  select(country, country_name, year, month, cases, deaths, case_inc, death_inc) |>
  rename(name = country_name)
  
  
ggplot(data = BFA) + 
  geom_line(aes(x = month, y = case_inc, color = year, group = year))


# link to shapefile
geodata::gadm(country = country, level = 0,
              path = "./", version = "4.1")

file <- list.files(path = "./gadm",
           pattern = paste0("*", country , "_0_pk.rds"), full.names = TRUE)

object <- readRDS(file) # read in object
object <- terra::vect(sf::st_as_sf(object)) # unpack SpatVector
object <- st_as_sf(object) # transform to sf object


BFA_shp <- object

plot(BFA_shp)



# save wide files:
shp_wide <- function(yearstart, yearend){
  
  BFA_wide <- BFA |>
    filter(year %in% seq(yearstart, yearend, 1)) |>
    select(country, name, year, month, case_inc, death_inc) |>
    rename(c = case_inc, d = death_inc) |>
    pivot_wider(names_from = c(year, month), values_from = c(c, d)) 
  
  
  BFA_all <- BFA_shp |> left_join(BFA_wide, by = c("COUNTRY" = "name")) |>
    rename(ISO = country) |>
    select(-GID_0)
  
  st_write(BFA_all, paste0("./files/shp results/output_", yearstart, "_", yearend, ".shp"), append = FALSE, overwrite = TRUE)
  
}


shp_wide(2000, 2005)
shp_wide(2006, 2010)


# save short files:
shp_bymonth <- function(year, month){
  
  BFA_short <- BFA |>
    filter(year == {{year}} & month == {{month}}) |>
    select(country, name, year, month, case_inc, death_inc) 
  
  BFA_all <- BFA_shp |> left_join(BFA_short, by = c("COUNTRY" = "name")) |>
    rename(ISO = country) |>
    select(-GID_0)
  
  st_write(BFA_all, paste0("./files/shp results/output_", year, "_", month, ".shp"), append = FALSE, overwrite = TRUE)
  
  print(paste0("Ran year ", year, " and month ", month))
  
}

year <- seq(2000, 2010, 1)
month <- seq_len(12)
combo <- crossing(year, month)

map2(combo$year, combo$month, shp_bymonth)
