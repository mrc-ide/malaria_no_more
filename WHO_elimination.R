# WHO country elimination maps
source("./data_and_libraries.R")

# read in country / year of elimination doc
# data source: https://www.who.int/teams/global-malaria-programme/elimination/countries-and-territories-certified-malaria-free-by-who
WHO <- read_csv("./files/WHO_elimination.csv") |>
  mutate(country = case_when(country == "R\xe9union" ~ "Réunion", 
                             TRUE ~ country)) |>
  mutate(year = year_elim, 
         year = case_when(is.na(year) ~ year_disappear,
                          TRUE ~ year))

# read in WHO country list doc
# source: https://data.who.int/countries
WHO_countries <- read_csv("./files/WHO_countries.csv") |> 
  mutate(GADM = case_when(GADM == "C\xf4te d'Ivoire" ~ "Côte d'Ivoire", 
                          GADM == "S\xe3o\xa0Tom\xe9\xa0and\xa0Pr\xedncipe" ~ "São Tomé and Príncipe",
                          GADM == "T\xfcrkiye" ~ "Türkiye", 
                          TRUE ~ GADM)) 

# read in country shapefile
GADM_world_0 <- readRDS("./files/GADM_world_0.rds")

# look at country names which don't match and adjust csv ^
WHO |> anti_join(GADM_world_0 |> st_drop_geometry(), by = c("country" = "COUNTRY"))

WHO_countries |> anti_join(GADM_world_0 |> st_drop_geometry(), by = c("GADM" = "COUNTRY"))
GADM_world_0 |> st_drop_geometry() |> anti_join(WHO_countries, by = c("COUNTRY" = "GADM"))

# look at range of years of elimination
summary(WHO$year)

# match to country shapefile
# filter to WHO countries
dat <- GADM_world_0 |> 
  inner_join(WHO_countries, by = c("COUNTRY" = "GADM")) |>
  left_join(WHO, by = c("COUNTRY" = "country")) |>
  mutate(decade = ceiling(year / 10) * 10) |>
  mutate(decade = case_when(decade > 2020 ~ 2024,
                            TRUE ~ decade)) 

# countries which are not part of WHO
dat_NA <- GADM_world_0 |> 
  anti_join(WHO_countries, by = c("COUNTRY" = "GADM"))

# compile dataset of countries which have eliminated for each decade
compiledat <- function(decadeval){
  
  dat |> filter(decade <= decadeval) |>
    mutate(elim = decadeval)
  
}

compile_dat <- map_dfr(c(1970, 1980, 1990, 2010, 2020, 2024), compiledat)

# add in 1960
compile_dat <- compile_dat |> 
  mutate(elim = factor(elim, 
                       levels = c(1960, 1970, 1980, 1990, 2010, 2020, 2024), 
                       labels = c(1960, 1970, 1980, 1990, 2010, 2020, 2024)))

test <- compile_dat |> st_drop_geometry()

# plot
p <- ggplot() + 
  geom_sf(data = dat, fill = "red", color = "lightgrey", linewidth = 0.05) + 
  geom_sf(data = dat_NA, fill = "darkgrey", color = "lightgrey", linewidth = 0.05) + 
  geom_sf(data = compile_dat, aes(fill = as.factor(elim)), color = "lightgrey", show.legend = FALSE, linewidth = 0.05) + 
  facet_wrap( ~ elim, nrow = 3, drop = FALSE) + # to include 1960 even though no countries have eliminated 
  scale_fill_manual(values = c("white", "white", "white", "white", "white", "white",  "white")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank())

# save plot as PNG / PDF 
ggsave(filename = paste0(HPCpath, "plots/elimination_decades.png"), plot = p, width = 6, height = 8)

# output dataset





