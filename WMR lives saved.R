# estimates of lives saved using World Malaria Report estimates
source("./data_and_libraries.R")


# read in WMR 2023 Annex 4F (modified for just WHO Africa region + Sudan + Somalia)
dat <- read_csv("./files/WMR2023_Annex_4F_Africa.csv") |>
  fill(country, .direction = "down")

head(dat)

# read in country shapefiles
GADM_world_0 <- readRDS("./files/GADM_world_0.rds") |> st_drop_geometry() 


# calculate population weighted 2000 cases and cumulative deaths averted
summary <- dat |> 
  group_by(country) |>
  mutate(MR_2000 = first(deaths) / first(population)) |> # mortality rate 2000
  rowwise() |>
  mutate(deaths_2000 = MR_2000 * population) |> # population adjusted cases 2000
  mutate(deaths_diff = deaths_2000 - deaths) |> # difference between 2000 deaths and current year
  mutate(deaths_diff_pos = case_when(deaths_diff < 0 ~ 0, # difference setting negative values to 0
                                     TRUE ~ deaths_diff)) |>
  ungroup() |>  group_by(country) |>
  mutate(deaths_averted_cumulative = cumsum(deaths_diff), # calculating cumulative deaths averted
         deaths_averted_cumulative_pos = cumsum(deaths_diff_pos)) # ^ without negatives

head(summary)

# make sure things are trending in the right direction
ggplot(data = summary) + 
  geom_hline(yintercept = 0, lty = 2) + 
  geom_line(aes(x = year, y = deaths_averted_cumulative)) + 
  facet_wrap(~ country)  


# get counts for 2015 and 2022
sum(summary[summary$year == 2015,]$deaths)
sum(summary[summary$year == 2015,]$deaths_averted_cumulative)
sum(summary[summary$year == 2015,]$deaths_averted_cumulative_pos)

sum(summary[summary$year == 2022,]$deaths)
sum(summary[summary$year == 2022,]$deaths_averted_cumulative)
sum(summary[summary$year == 2022,]$deaths_averted_cumulative_pos)



# see which country names need to be modified
summary |> left_join(GADM_world_0 |> st_drop_geometry(), by = c("country" = "COUNTRY")) |>
  filter(is.na(ID_0) & year == 2000) |> dplyr::select(country)


# clean data
summary <- summary |>
  dplyr::select(country, year, population, deaths, cases, deaths_averted_cumulative) |>
  rename(lives_saved = deaths_averted_cumulative,
         annual_deaths = deaths,
         annual_cases = cases) |>
  mutate(country = case_when(country == "Congo" ~ "Republic of the Congo",
                             country == "C\xf4te d\x92Ivoire" ~ "Côte d'Ivoire",
                             country == "Eswatini" ~ "Swaziland",
                             country == "Sao Tome and Principe" ~ "São Tomé and Príncipe",
                             country == "United Republic of Tanzania" ~ "Tanzania",
                             TRUE ~ country)) |>
  mutate(lives_saved = round(lives_saved)) |>
  left_join(GADM_world_0, by = c("country" = "COUNTRY")) |>
  arrange(country, year)



# write out results into a csv file
output_2015 <- summary |> filter(year == 2015) |> dplyr::select(-population, -annual_cases)
output_2022 <- summary |> filter(year == 2022) |> dplyr::select(-population, -annual_cases)

# aggregate lives saved across Africa per year
lives_saved_per_year <- summary |> group_by(year) |>
  summarize(population = sum(population),
            annual_deaths = sum(annual_deaths),
            lives_saved = sum(lives_saved))

sum(output_2015$lives_saved); sum(output_2022$lives_saved)
sum(output_2015$annual_deaths); sum(output_2022$annual_deaths)

saveRDS(summary, "./files/WMR_lives_saved.RDS")
write_csv(output_2015, "./files/WMR_lives_saved_2015.csv")
write_csv(output_2022, "./files/WMR_lives_saved_2022.csv")
write_csv(lives_saved_per_year, "./files/WMR_lives_saved_aggregate_per_year.csv")

# number of deaths in children under 5
# WMR 2022: 78.1% of all deaths in this region were among children aged under 5 years in 2022
sum(output_2022$annual_deaths) * .781
# WMR 2022: 76.0% of all deaths globally were among children aged under 5 years in 2022
608000 * .781

