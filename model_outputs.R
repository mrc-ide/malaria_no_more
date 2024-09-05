# visualize model output
source("./data_and_libraries.R")

# read in data
annual_total <- readRDS("M:/Lydia/malaria_no_more/outputs/updated_run_annual.rds") # there should be 157645 rows; 5 scenarios
annual_u5 <- readRDS("M:/Lydia/malaria_no_more/outputs/updated_run_annual_u5.rds") # there should be 157645 rows; 5 scenarios
head(annual_total); head(annual_u5)

table(annual_total$scenario)

# tidy
annual_total <- annual_total |>
  dplyr::select(-clinical, -mortality, -lives_saved)

annual_u5 <- annual_u5 |>
  rename(cases_u5 = cases,
         deaths_u5 = deaths,
         pop_u5 = pop) |>
  dplyr::select(-clinical, -mortality)


head(annual_total); head(annual_u5)

# merge u5 and total pop files
annual <- annual_total |> left_join(annual_u5, by = c("country", "country_name", "site_name", "urban_rural", "scenario", "year", "description"))

# revise country names so they will match with GADM file
annual <- annual |>
  mutate(site_name = case_when(country == "AGO" & site_name == "Bie" ~ "Bié",
                               country == "AGO" & site_name == "Huila" ~ "Huíla",
                               country == "AGO" & site_name == "Uige" ~ "Uíge",
                               country == "BEN" & site_name == "Oueme" ~ "Ouémé",
                               country == "CAF" & site_name == "Kemo" ~ "Kémo",
                               country == "CAF" & site_name == "Mambere-Kadei" ~ "Mambéré-Kadéï",
                               country == "CAF" & site_name == "Nana-Grebizi" ~ "Nana-Grébizi",
                               country == "CAF" & site_name == "Nana-Mambere" ~ "Nana-Mambéré",
                               country == "CAF" & site_name == "Ouham-Pende" ~ "Ouham-Pendé",
                               country == "CAF" & site_name == "Sangha-Mbaere" ~ "Sangha-Mbaéré",
                               country == "CIV" & site_name == "Comoe" ~ "Comoé",
                               country == "CIV" & site_name == "Denguele" ~ "Denguélé",
                               country == "CIV" & site_name == "Goh-Djiboua" ~ "Gôh-Djiboua",
                               country == "CIV" & site_name == "Sassandra-Marahoue" ~ "Sassandra-Marahoué",
                               country == "CIV" & site_name == "Vallee du Bandama" ~ "Vallée du Bandama",
                               country == "CMR" & site_name == "Extreme-Nord" ~ "Extrême-Nord",
                               country == "COD" & site_name == "Equateur" ~ "Équateur",
                               country == "COD" & site_name == "Kasai" ~ "Kasaï",
                               country == "COD" & site_name == "Kasai-Central" ~ "Kasaï-Central",
                               country == "COD" & site_name == "Kasai-Oriental" ~ "Kasaï-Oriental",
                               country == "COG" & site_name == "Lekoumou" ~ "Lékoumou",
                               country == "GIN" & site_name == "Boke" ~ "Boké",
                               country == "GIN" & site_name == "Labe" ~ "Labé",
                               country == "GIN" & site_name == "Nzerekore" ~ "Nzérékoré",
                               country == "GNB" & site_name == "Bafata" ~ "Bafatá",
                               country == "GNB" & site_name == "Gabu" ~ "Gabú",
                               country == "MLI" & site_name == "Segou" ~ "Ségou",
                               country == "NER" & site_name == "Tillabery" ~ "Tillabéry",
                               country == "TCD" & site_name == "Guera" ~ "Guéra",
                               country == "TCD" & site_name == "Ouaddai" ~ "Ouaddaï",
                               country == "TCD" & site_name == "Tandjile" ~ "Tandjilé",
                               country == "GAB" & site_name == "Haut-Ogooue" ~ "Haut-Ogooué",
                               country == "GAB" & site_name == "Moyen-Ogooue" ~ "Moyen-Ogooué",
                               country == "GAB" & site_name == "Ngounie" ~ "Ngounié",
                               country == "GAB" & site_name == "Ogooue-Ivindo" ~ "Ogooué-Ivindo",
                               country == "GAB" & site_name == "Ogooue-Lolo" ~ "Ogooué-Lolo",
                               country == "GAB" & site_name == "Ogooue-Maritime" ~ "Ogooué-Maritime",
                               country == "GNQ" & site_name == "Kie-Ntem" ~ "Kié-Ntem",
                               country == "SEN" & site_name == "Sedhiou" ~ "Sédhiou",
                               country == "SEN" & site_name == "Thies" ~ "Thiès",
                               
                               TRUE ~ site_name
  ))


# combine urban and rural designations
# use Zou, Benin as a reference group to see if changes are working correctly
annual |> filter(site_name == "Zou" & year == 2000)

annual <- annual |>
  group_by(country, country_name, site_name, scenario, year) |>
  summarize(cases = sum(cases),
            deaths = sum(deaths),
            pop = sum(pop),
            cases_u5 = sum(cases_u5),
            deaths_u5 = sum(deaths_u5),
            pop_u5 = sum(pop_u5)) |>
  rowwise() |>
  mutate(clinical = cases / pop,
         mortality = deaths / pop,
         clinical_u5 = cases_u5 / pop_u5,
         mortality_u5 = deaths_u5 / pop_u5) |> ungroup()

annual |> filter(site_name == "Zou" & year == 2000)
  
  
# check that there is only one urban / rural classification per site
annual |>
  filter(year == 2000) |>
  group_by(scenario, country, site_name) |>
  summarize(n = n()) |>
  filter(n > 1)

# plot, line graph
ggplot(annual) + 
  geom_line(aes(x = year, y = clinical, color = scenario, 
                group = interaction(site_name, scenario)), alpha = 0.2, 
            show.legend = FALSE) + 
  facet_wrap(~ country) + 
  theme_classic()


# smooth out 3-year cycles
annual <- annual |>
  group_by(scenario, country, site_name) |>
  mutate(across(clinical:mortality_u5, ~ rollapply(.x, 3, function(x) mean(x), partial = TRUE), .names = "mean_{.col}"))

# let year 2000 stay at its original value and year 2040 at 2/3 of its original value
annual <- annual |>
  mutate(mean_clinical = case_when(year == 2000 ~ clinical,
                                   year == 2040 ~ lag(mean_clinical, n = 1),
                                   TRUE ~ mean_clinical)) |>
  ungroup()

# try it
# x <- c(3, 5, 2, 5, 6)
# rollmean(x, 3, na.pad = TRUE, align = "center", partial = TRUE)
# rollapply(x, 3, function(x) mean(x), partial = TRUE)

# look at the difference in scenarios for BFA
ggplot(annual |> filter(country == "BFA")) + 
  geom_line(aes(x = year, y = mean_clinical, color = scenario), show.legend = FALSE) + 
  facet_wrap(~ site_name, scales = "free") + 
  theme_classic()


# look at the difference in scenarios for ETH
ggplot(annual |> filter(country == "ETH")) + 
  geom_line(aes(x = year, y = mean_clinical, color = scenario), show.legend = FALSE) + 
  facet_wrap(~ site_name, scales = "free") + 
  theme_classic()


# map
# pull in admin1 shapefile
admin1 <- readRDS("./files/GADM_SSA_1.rds")

# merge model data with shapefile
mapdat <- annual |> 
  left_join(admin1 |> dplyr::select(ID_0, NAME_1, geometry), 
            by = c("country" = "ID_0", "site_name" = "NAME_1")) |>
  st_as_sf()

class(mapdat)

# what admin1s are not matching? 
missing <- anti_join(annual |> dplyr::select(country, country_name, site_name) |> distinct(), admin1 |> dplyr::select(ID_0, NAME_1, geometry), 
                     by = c("country" = "ID_0", "site_name" = "NAME_1")) 

missing


# plot clinical incidence
p1 <- ggplot(data = mapdat |> 
               filter(year %in% c(2000, 2010, 2020, 2030, 2040)) |>
               filter(scenario %in% c("itn_change", "vaccine_scaleup", "worst_case")) |> 
               mutate(scenario = case_when(scenario == "itn_change" ~ "new_tools",
                                            TRUE ~ scenario))) + 
  geom_sf(aes(fill = mean_clinical), color = "lightgrey", show.legend = TRUE, linewidth = 0.005) + 
  facet_wrap(scenario ~ year, nrow = 3) + 
  scale_fill_distiller(palette = "PuOr", direction = -1) + # "RdYlBu"
  # scale_fill_gradient(low = "#ffcccc", high = "#990000") + 
  labs(fill = "clinical incidence") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank())


# save plot as PNG / PDF 
ggsave(filename = "map_clinical.png", plot = p1, width = 10, height = 14)
beep(1)

# plot mortality incidence
p2 <- ggplot(data = mapdat |> filter(year %in% c(2000, 2010, 2020, 2030, 2040))) + 
  geom_sf(aes(fill = mean_mortality), color = "lightgrey", show.legend = TRUE, linewidth = 0.005) + 
  facet_wrap(scenario ~ year, nrow = 4) + 
  scale_fill_distiller(palette = "PuOr", direction = -1) + 
  # scale_fill_gradient(low = "#ffcccc", high = "#990000") + 
  labs(fill = "mortality incidence") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank())


# save plot as PNG / PDF 
ggsave(filename = "map_mortality.png", plot = p2, width = 10, height = 10)
beep(1)

# plot clinical incidence all years
p3 <- ggplot(data = mapdat |> filter(year >= 2023) |> filter(scenario == "itn_change")) +
  geom_sf(aes(fill = mean_clinical), color = "lightgrey", show.legend = TRUE, linewidth = 0.005) +
  facet_wrap(scenario ~ year, nrow = 3) +
  scale_fill_distiller(palette = "PuOr", direction = -1) + # "RdYlBu"
  # scale_fill_gradient(low = "#ffcccc", high = "#990000") +
  labs(fill = "clinical incidence") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank())


# save plot as PNG / PDF 
ggsave(filename = "map_clinical_allyears.png", plot = p3, width = 10, height = 10)
beep(1)


# replace new_tools with updates from itn_60
mapdat <- mapdat |> 
  filter(scenario != "new_tools") |>
  mutate(scenario = case_when(scenario == "itn_change" ~ "new_tools",
                              TRUE ~ scenario))

table(mapdat$scenario)

# save data as shapefiles
# function to output shapefile for each variable and year
var_shp <- function(year, scenario){ 
  
  d <- mapdat |>
    rename(iso = country, 
           country = country_name, 
           admin1 = site_name) |>
    mutate(clinical = case_when(scenario %in% c("new_tools", "best_case") ~ mean_clinical,
                                TRUE ~ clinical),
           mortality = case_when(scenario %in% c("new_tools", "best_case") ~ mean_mortality,
                                 TRUE ~ mortality)) |>
    dplyr::filter(year == {{year}} & scenario == {{scenario}}) |>
    mutate(across(where(is.character), iconv, from = "UTF-8", to = "ISO-8859-1")) |>
    dplyr::select(iso, country, admin1, scenario, year, cases, deaths, pop, clinical, mortality)
  
  st_write(d, paste0("./files/model_output/", year, "_", scenario, ".shp"), append = FALSE, overwrite = TRUE)
  
  print(paste0("Wrote shapefile for ", year, " ", scenario))
  
}


# run function
year <- seq(2023, 2040, 1) 
scenario <- c("new_tools", "vaccine_scaleup")

vars <- crossing(year, scenario)

map2(vars$year, vars$scenario, var_shp)



# calculate deaths averted within the model and vs. WMR
# pivot model data from long to wide
annual_country <- annual |>
  group_by(country, country_name, scenario, year) |>
  summarize(pop = sum(pop), 
            cases = sum(cases),
            deaths = sum(deaths),
            cases_u5 = sum(cases_u5),
            deaths_u5 = sum(deaths_u5),
            pop_u5 = sum(pop_u5)) |>
  pivot_wider(names_from = scenario, values_from = c(cases, deaths, pop, cases_u5, deaths_u5, pop_u5)) |>
  ungroup()

# check that the population is the same under both scenarios 
all(annual_country$pop_new_tools == annual_country$pop_worst_case)
all(annual_country$pop_new_tools == annual_country$pop_vaccine_scaleup)
all(annual_country$pop_new_tools == annual_country$pop_itn_change)

# u5 populations are similar but not the same - it should be okay for lives saved
all(annual_country$pop_u5_new_tools == annual_country$pop_u5_worst_case)
all(annual_country$pop_u5_new_tools == annual_country$pop_u5_vaccine_scaleup)
all(annual_country$pop_u5_new_tools == annual_country$pop_itn_change)

# add in WMR data
WMR <- readRDS("./files/WMR_lives_saved.RDS")

combined <- annual_country |> 
  mutate(country_name = case_when(country_name == "Côte d’Ivoire" ~ "Côte d'Ivoire",
                                  country_name == "Congo - Kinshasa" ~ "Democratic Republic of the Congo",
                                  country_name == "Congo - Brazzaville" ~ "Republic of the Congo",
                                  TRUE ~ country_name)) |>
  left_join(WMR, by = c("country_name" = "country", "year"))

# check if countries in our model are not in the WMR
combined |> filter(is.na(lives_saved) & year < 2022) # all join

# check if countries in the WMR are not in our model
WMR |> left_join(annual_country |> filter(year < 2023) |>
                 mutate(country_name = case_when(country_name == "Côte d’Ivoire" ~ "Côte d'Ivoire",
                        country_name == "Congo - Kinshasa" ~ "Democratic Republic of the Congo",
                        country_name == "Congo - Brazzaville" ~ "Republic of the Congo",
                        TRUE ~ country_name)) , 
                 by = c("country" = "country_name", "year")) |>
  filter(is.na(cases_new_tools)) |>
  filter(year == 2000) |> print(n = 10)

# Algeria, Cabo Verde, Comoros, South Africa, Swaziland, Sao Tome and Principe


# compare model and WMR values 2000 - 2022 to get a scaling factor
scalingf_00_22 <- combined |> filter(year <= 2022) |> # deaths 2000 - 2022
  summarize(model = sum(deaths_new_tools),
            WMR = sum(annual_deaths)) |>
  mutate(ratio = WMR / model) |>
  dplyr::select(ratio) |> as.numeric() # 1.76

scalingf_22 <- combined |> filter(year == 2022) |> # deaths 2022
  summarize(model = sum(deaths_new_tools),
            WMR = sum(annual_deaths)) |>
  mutate(ratio = WMR / model) |>
  dplyr::select(ratio) |> as.numeric() # 1.21

# scaling factor for 2022 mortality rate
scalingf_22_MR <- combined |> filter(year == 2022) |> # deaths 2022
  summarize(model = sum(deaths_new_tools) / sum(pop_new_tools),
            WMR = sum(annual_deaths) / sum(population)) |>
  mutate(ratio = WMR / model) |>
  dplyr::select(ratio) |> as.numeric() # 1.25

# scaling factor for 2022 clinical incidence
scalingf_22_CI <- combined |> filter(year == 2022) |> # cases 2022
  summarize(model = sum(cases_new_tools) / sum(pop_new_tools),
            WMR = sum(annual_cases) / sum(population)) |>
  mutate(ratio = WMR / model) |>
  dplyr::select(ratio) |> as.numeric() # 0.95

scalingf_00_22; scalingf_22; scalingf_22_MR; scalingf_22_CI


# calculate adjusted deaths using both scaling factors
scale_2022 <- combined |>
  group_by(year) |>
  summarize(across(.cols = c(deaths_itn_change:pop_worst_case, deaths_u5_itn_change:annual_deaths), 
                   \(x) sum(x, na.rm = TRUE))) |>
  mutate(adj_deaths_new_tools = case_when(year <= 2022 ~ annual_deaths,
                                          year > 2022 ~ deaths_new_tools * scalingf_22),
         adj_deaths_itn_60 = case_when(year <= 2022 ~ annual_deaths,
                                          year > 2022 ~ deaths_itn_change * scalingf_22),
         adj_deaths_worst_case = case_when(year <= 2022 ~ annual_deaths,
                                           year > 2022 ~ deaths_worst_case * scalingf_22),
         adj_deaths_u5_new_tools = case_when(year <= 2022 ~ 0,
                                          year > 2022 ~ deaths_u5_new_tools * scalingf_22),
         adj_deaths_u5_itn_60 = case_when(year <= 2022 ~ 0,
                                       year > 2022 ~ deaths_u5_itn_change * scalingf_22),
         adj_deaths_u5_worst_case = case_when(year <= 2022 ~ 0,
                                          year > 2022 ~ deaths_u5_worst_case * scalingf_22),
         combined_pop = case_when(year <= 2022 ~ population,
                                 year > 2022 ~ pop_new_tools),
         combined_pop_u5_new_tools = case_when(year <= 2022 ~ 0,
                                  year > 2022 ~ pop_u5_new_tools),
         combined_pop_u5_itn_60 = case_when(year <= 2022 ~ 0,
                                  year > 2022 ~ pop_u5_itn_change),
         combined_pop_u5_worst_case = case_when(year <= 2022 ~ 0,
                                  year > 2022 ~ pop_u5_worst_case)
         ) |>
  mutate(MR_new_tools = adj_deaths_new_tools / combined_pop * scalingf_22_MR,
         MR_itn_60 = adj_deaths_itn_60 / combined_pop * scalingf_22_MR,
         MR_worst_case = adj_deaths_worst_case / combined_pop * scalingf_22_MR,
         
         MR_u5_new_tools = adj_deaths_u5_new_tools / combined_pop_u5_new_tools * scalingf_22_MR,
         MR_u5_itn_60 = adj_deaths_u5_itn_60 / combined_pop_u5_itn_60 * scalingf_22_MR,
         MR_u5_worst_case = adj_deaths_u5_worst_case / combined_pop_u5_worst_case * scalingf_22_MR) 


# plot
ggplot(data = scale_2022) + 
  geom_line(aes(x = year, y = MR_new_tools * 1000), lty = 1) + 
  geom_line(aes(x = year, y = MR_itn_60 * 1000), lty = 2) +
  geom_line(aes(x = year, y = MR_worst_case * 1000), lty = 3) +
  geom_vline(xintercept = 2022, lty = 2) + 
  labs(x = "year", y = "mortality rate per 1,000 people")

ggplot(data = scale_2022) + 
  geom_line(aes(x = year, y = MR_u5_new_tools * 1000), lty = 1) + 
  geom_line(aes(x = year, y = MR_u5_itn_60 * 1000), lty = 2) +
  geom_line(aes(x = year, y = MR_u5_worst_case * 1000), lty = 3) +
  geom_vline(xintercept = 2022, lty = 2) + 
  labs(x = "year", y = "mortality rate per 1,000 children u5")

  
# calculate lives saved between scenarios
lives_saved_total <- combined |> 
  # scale death counts
  mutate(final_deaths_new_tools = case_when(year <= 2022 ~ annual_deaths,
                                            year > 2022 ~  deaths_new_tools * scalingf_22),
         final_deaths_new_tools_u5 = case_when(year <= 2022 ~ 0,
                                            year > 2022 ~  deaths_u5_new_tools * scalingf_22),
         final_deaths_itn_60 = case_when(year <= 2022 ~ annual_deaths,
                                            year > 2022 ~  deaths_itn_change * scalingf_22),
         final_deaths_itn_60_u5 = case_when(year <= 2022 ~ 0,
                                         year > 2022 ~ deaths_u5_itn_change * scalingf_22),
         final_deaths_worst_case = case_when(year <= 2022 ~ annual_deaths,
                                             year > 2022 ~ deaths_worst_case * scalingf_22),
         final_deaths_worst_case_u5 = case_when(year <= 2022 ~ 0,
                                             year > 2022 ~ deaths_u5_worst_case * scalingf_22)) |>
  group_by(country) |>
  # WMR mortality rate 2000
  mutate(MR_2000 = first(annual_deaths) / first(pop_new_tools)) |> 
  rowwise() |>
  # population adjusted deaths 2000
  mutate(deaths_2000 = MR_2000 * pop_new_tools) |> 
  # difference between 2000 and 2023 deaths and current year
  mutate(deaths_diff_WMR2000 = deaths_2000 - final_deaths_new_tools,
         deaths_diff_worst_case_nt = case_when(year <= 2022 ~ deaths_2000 - final_deaths_new_tools,
                                            year > 2022 ~ final_deaths_worst_case - final_deaths_new_tools), 
         deaths_diff_worst_case_nt_2023 = case_when(year <= 2022 ~ 0,
                                                 year > 2022 ~ final_deaths_worst_case - final_deaths_new_tools),
         deaths_diff_worst_case_itn60_2023 = case_when(year <= 2022 ~ 0,
                                                 year > 2022 ~ final_deaths_worst_case - final_deaths_itn_60),
         deaths_diff_worst_case_nt_2023_u5 = case_when(year <= 2022 ~ 0,
                                                    year > 2022 ~ final_deaths_worst_case_u5 - final_deaths_new_tools_u5),
         deaths_diff_worst_case_itn60_2023_u5 = case_when(year <= 2022 ~ 0,
                                                       year > 2022 ~ final_deaths_worst_case_u5 - final_deaths_itn_60_u5)
         ) |> 
  ungroup() |>  group_by(country) |>
  # calculating cumulative deaths averted
  mutate(deaths_averted_cumulative_WMR2000 = cumsum(deaths_diff_WMR2000), 
         deaths_averted_cumulative_worst_case_nt = cumsum(deaths_diff_worst_case_nt),
         deaths_averted_cumulative_worst_case_nt_2023 = cumsum(deaths_diff_worst_case_nt_2023),
         deaths_averted_cumulative_worst_case_itn60_2023 = cumsum(deaths_diff_worst_case_itn60_2023),
         deaths_averted_cumulative_worst_case_nt_2023_u5 = cumsum(deaths_diff_worst_case_nt_2023_u5),
         deaths_averted_cumulative_worst_case_itn60_2023_u5 = cumsum(deaths_diff_worst_case_itn60_2023_u5)) |> 
  # renaming variables as lives saved
rename(lives_saved_WMR2000 = deaths_averted_cumulative_WMR2000,
       lives_saved_worst_case_nt = deaths_averted_cumulative_worst_case_nt,
       lives_saved_worst_case_nt_2023 = deaths_averted_cumulative_worst_case_nt_2023,
       lives_saved_worst_case_itn60_2023 = deaths_averted_cumulative_worst_case_itn60_2023,
       lives_saved_worst_case_nt_2023_u5 = deaths_averted_cumulative_worst_case_nt_2023_u5,
       lives_saved_worst_case_itn60_2023_u5 = deaths_averted_cumulative_worst_case_itn60_2023_u5) |>
  mutate(across(c(lives_saved_WMR2000:lives_saved_worst_case_itn60_2023_u5), \(x) round(x)))

# aggregate over countries
lives_saved_total_world <- lives_saved_total |>
  group_by(year) |>
  summarize(across(c(lives_saved_WMR2000:lives_saved_worst_case_itn60_2023_u5), \(x) sum(x)))

# instances where lives saved under the worst cases scenario is greater than relative to the 2000 baseline
test <- lives_saved_total |> filter(lives_saved_worst_case_nt > lives_saved_WMR2000) |>
  group_by(country) |>
  summarize(n = n())

# plot lives saved
ggplot(data = lives_saved_total_world) + 
  geom_line(aes(x = year, y = lives_saved_WMR2000), color = "purple") + 
  geom_line(aes(x = year, y = lives_saved_worst_case_nt), color = "darkmagenta") + 
  geom_line(aes(x = year, y = lives_saved_worst_case_nt_2023), color = "magenta") + 
  geom_line(aes(x = year, y = lives_saved_worst_case_itn60_2023), color = "magenta", lty = 2) +
  geom_vline(xintercept = 2022, lty = 2) + 
  labs(x = "year", y = "lives saved")


# save as .csv
lives_saved_total_country <- lives_saved_total |>
  dplyr::select(country, country_name, year, lives_saved_WMR2000, lives_saved_worst_case_itn60_2023)
  
lives_saved_total_world |>
  filter(year >= 2023) |>
  dplyr::select(year, lives_saved_worst_case_itn60_2023) |>
  rename(lives_saved = lives_saved_worst_case_itn60_2023) |>
write_csv("./files/modelling_lives_saved_2040_SSA.csv")

lives_saved_total_country |>
  filter(year == 2040) |>
  dplyr::select(country, country_name, year, lives_saved_worst_case_itn60_2023) |>
  rename(lives_saved = lives_saved_worst_case_itn60_2023) |>
write_csv( "./files/modelling_lives_saved_2040_country.csv")


# look at mortality rates
MR <- combined |> 
  group_by(year) |>
  summarize(WMRpop = sum(population),
            WMRdeaths = sum(annual_deaths),
            WMRcases = sum(annual_cases),
            model_pop = sum(pop_new_tools), # total pop for all model runs is the same
            
            u5_pop_new_tools = sum(pop_u5_new_tools),
            # u5_pop_best_case = sum(pop_u5_best_case),
            u5_pop_itn_60 = sum(pop_u5_itn_change),
            u5_pop_vaccine_scale = sum(pop_u5_vaccine_scaleup),
            u5_pop_worst_case = sum(pop_u5_worst_case),
            
            new_tools_deaths = sum(deaths_new_tools),
            # best_case_deaths = sum(deaths_best_case),
            itn_60_deaths = sum(deaths_itn_change),
            vaccine_scale_deaths = sum(deaths_vaccine_scaleup),
            worst_case_deaths = sum(deaths_worst_case),
            
            new_tools_cases = sum(cases_new_tools),
            # best_case_cases = sum(cases_best_case),
            itn_60_cases = sum(cases_itn_change),
            vaccine_scale_cases = sum(cases_vaccine_scaleup),
            worst_case_cases = sum(cases_worst_case),
            
            new_tools_deaths_u5 = sum(deaths_u5_new_tools),
            # best_case_deaths_u5 = sum(deaths_u5_best_case),
            itn_60_deaths_u5 = sum(deaths_u5_itn_change),
            vaccine_scale_deaths_u5 = sum(deaths_u5_vaccine_scaleup),
            worst_case_deaths_u5 = sum(deaths_u5_worst_case),
            
            new_tools_cases_u5 = sum(cases_u5_new_tools),
            # best_case_cases_u5 = sum(cases_u5_best_case),
            itn_60_cases_u5 = sum(cases_u5_itn_change),
            vaccine_scale_cases_u5 = sum(cases_u5_vaccine_scaleup),
            worst_case_cases_u5 = sum(cases_u5_worst_case)) |>
  rowwise() |>
  mutate(WMR_MR = WMRdeaths / WMRpop,
         WMR_CI = WMRcases / WMRpop,
         
         new_tools_MR = new_tools_deaths / model_pop,
         # best_case_MR = best_case_deaths / model_pop,
         itn_60_MR = itn_60_deaths / model_pop,
         vaccine_scale_MR = vaccine_scale_deaths / model_pop,
         worst_case_MR = worst_case_deaths / model_pop,
         
         new_tools_CI = new_tools_cases / model_pop,
         # best_case_CI = best_case_cases / model_pop,
         itn_60_CI = itn_60_cases / model_pop,
         vaccine_scale_CI = vaccine_scale_cases/ model_pop,
         worst_case_CI = worst_case_cases / model_pop,
         
         new_tools_MR_u5 = new_tools_deaths_u5 / u5_pop_new_tools,
         # best_case_MR_u5 = best_case_deaths_u5 / u5_pop_best_case,
         itn_60_MR_u5 = itn_60_deaths_u5 / u5_pop_itn_60,
         vaccine_scale_MR_u5 = vaccine_scale_deaths_u5 / u5_pop_vaccine_scale,
         worst_case_MR_u5 = worst_case_deaths_u5 / u5_pop_worst_case,
         
         new_tools_CI_u5 = new_tools_cases_u5 / u5_pop_new_tools,
         # best_case_CI_u5 = best_case_cases_u5 / u5_pop_best_case,
         itn_60_CI_u5 = itn_60_cases_u5 / u5_pop_itn_60,
         vaccine_scale_CI_u5 = vaccine_scale_cases_u5 / u5_pop_vaccine_scale,
         worst_case_CI_u5 = worst_case_cases_u5 / u5_pop_worst_case,
         ) |>
  
  ungroup() |>
  mutate(across(new_tools_MR:worst_case_CI_u5, ~ rollapply(.x, 3, function(x) mean(x), partial = TRUE))) |>

  mutate(WMR_MR_2000 = first(WMR_MR)) |>
  mutate(WMR_vs_WRM_2000 = WMR_MR / WMR_MR_2000,
         MR_new_tools_vs_WMR_2000 = new_tools_MR / WMR_MR_2000,
         # MR_best_case_vs_WMR_2000 = best_case_MR / WMR_MR_2000,
         MR_itn_60_vs_WMR_2000 = itn_60_MR / WMR_MR_2000,
         
         MR_worst_case_vs_new_tools = new_tools_MR / worst_case_MR,
         MR_worst_case_vs_itn_60 = itn_60_MR / worst_case_MR,
         CI_worst_case_vs_new_tools = new_tools_CI / worst_case_CI,
         CI_worst_case_vs_itn_60 = itn_60_CI / worst_case_CI,
         
         MR_worst_case_vs_new_tools_u5 = new_tools_MR_u5 / worst_case_MR_u5,
         MR_worst_case_vs_itn_60_u5 = itn_60_MR_u5 / worst_case_MR_u5)

# scaling factor for 2022 mortality rate
WMR_scale_2022_MR <- MR |>
  filter(year == 2022) |>
  summarize(MR_scale = WMR_MR / vaccine_scale_MR) |> as.numeric() # 1.25

WMR_scale_2022_CI <- MR |>
  filter(year == 2022) |>
  summarize(CI_scale = WMR_CI / vaccine_scale_CI) |> as.numeric() # 0.95
  
# plot mortality rates with scaling factor
ggplot(data = MR) + 
  geom_line(aes(x = year, y = WMR_MR), color = "blue") +
  geom_line(aes(x = year, y = new_tools_MR * WMR_scale_2022_MR), color = "tomato")


# compare reductions from 2015 MR to 2040 MR to estimate 2030 goals
WMR_MR_2015 <- MR |> filter(year == 2015) |> dplyr::select(WMR_MR) |> as.numeric()
model_MR_2040 <- MR |> filter(year == 2040) |> dplyr::select(itn_60_MR) |> mutate(itn_60_MR = itn_60_MR * WMR_scale_2022_MR) |> as.numeric()

1 - model_MR_2040 / WMR_MR_2015 # MR reduced by 56% by 2040

MR |> filter(year == 2040) |> dplyr::select(MR_worst_case_vs_new_tools,
                                            MR_worst_case_vs_itn_60,
                                            CI_worst_case_vs_new_tools,
                                            CI_worst_case_vs_itn_60,
                                            MR_worst_case_vs_new_tools_u5,
                                            MR_worst_case_vs_itn_60_u5)


1 - (MR |> filter(year == 2022) |> dplyr::select(WMR_vs_WRM_2000)) # MR reduced by 61% by 2022

1 - (MR |> filter(year == 2040) |> dplyr::select(MR_worst_case_vs_new_tools)) # MR reduced by 69%
1 - (MR |> filter(year == 2040) |> dplyr::select(MR_worst_case_vs_itn_60)) # MR reduced by 75%

1 - (MR |> filter(year == 2040) |> dplyr::select(CI_worst_case_vs_new_tools)) # CI reduced by 56%
1 - (MR |> filter(year == 2040) |> dplyr::select(CI_worst_case_vs_itn_60)) # CI reduced by 67%

1 - (MR |> filter(year == 2040) |> dplyr::select(MR_worst_case_vs_new_tools_u5)) # u5 MR reduced by 79%
1 - (MR |> filter(year == 2040) |> dplyr::select(MR_worst_case_vs_itn_60_u5)) # u5 MR reduced by 82%



# plots MNMUK
# plot mortality rate
ggplot(data = MR) + 
  geom_line(data = MR |> filter(year >= 2022), aes(x = year, y = new_tools_MR * 1000 * WMR_scale_2022_MR), color = "cornflowerblue", lty = 2) + 
  geom_line(data = MR |> filter(year >= 2022), aes(x = year, y = itn_60_MR * 1000 * WMR_scale_2022_MR), color = "lightblue", lty = 2) +
  # geom_line(data = MR |> filter(year >= 2022), aes(x = year, y = best_case_MR * 1000 * WMR_scale_2022_MR), color = "blue", lty = 2) + 
  geom_line(data = MR |> filter(year >= 2022), aes(x = year, y = worst_case_MR * 1000 * WMR_scale_2022_MR), color = "tomato", lty = 2) +
  geom_line(aes(x = year, y = WMR_MR * 1000), color = "black", lty = 1) + 
  geom_vline(xintercept = 2022, lty = 2) + 
  scale_y_continuous(limits = c(0, 1.5), breaks = seq(0, 2, 0.5)) + 
  labs(x = "year", y = "mortality rate per 1,000 people")

ggplot(data = MR) + 
  geom_line(data = MR |> filter(year >= 2022), aes(x = year, y = new_tools_CI * 1000 * WMR_scale_2022_CI), color = "cornflowerblue", lty = 2) + 
  geom_line(data = MR |> filter(year >= 2022), aes(x = year, y = itn_60_CI * 1000 * WMR_scale_2022_CI), color = "lightblue", lty = 2) +
  # geom_line(data = MR |> filter(year >= 2022), aes(x = year, y = best_case_CI * 1000 * WMR_scale_2022_CI), color = "blue", lty = 2) + 
  geom_line(data = MR |> filter(year >= 2022), aes(x = year, y = worst_case_CI * 1000 * WMR_scale_2022_CI), color = "tomato", lty = 2) +
  geom_line(aes(x = year, y = WMR_CI * 1000), color = "black", lty = 1) +
  geom_vline(xintercept = 2022, lty = 2) +
  scale_y_continuous(limits = c(0, 500), breaks = seq(0, 500, 100)) +
  labs(x = "year", y = "clinical incidence per 1,000 people")

ggplot(data = MR) + 
  geom_line(data = MR |> filter(year >= 2022), aes(x = year, y = new_tools_MR_u5 * 1000 * WMR_scale_2022_MR), color = "cornflowerblue", lty = 2) + 
  geom_line(data = MR |> filter(year >= 2022), aes(x = year, y = itn_60_MR_u5 * 1000 * WMR_scale_2022_MR), color = "lightblue", lty = 2) +
  # geom_line(data = MR |> filter(year >= 2022), aes(x = year, y = best_case_MR_u5 * 1000 * WMR_scale_2022_MR), color = "blue", lty = 2) + 
  geom_line(data = MR |> filter(year >= 2022), aes(x = year, y = worst_case_MR_u5 * 1000 * WMR_scale_2022_MR), color = "tomato", lty = 2) +
  # geom_line(aes(x = year, y = WMR_MR * 1000), color = "black", lty = 1) + 
  geom_vline(xintercept = 2022, lty = 2) + 
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
  scale_x_continuous(limits = c(2000, 2040), breaks = seq(2000, 2040, 10)) + 
  labs(x = "year", y = "mortality rate per 1,000 children u5")


# plot lives saved
WMR_lives_saved_2022 <- lives_saved_total_world |> filter(year == 2022) |> dplyr::select(lives_saved_WMR2000) |> as.numeric()

ggplot(data = lives_saved_total_world) + 
  geom_line(data = lives_saved_total_world |> filter(year <= 2022), aes(x = year, y = lives_saved_WMR2000 / 1000000), color = "purple") + 
  geom_line(data = lives_saved_total_world |> filter(year >= 2022), aes(x = year, y = lives_saved_worst_case_nt_2023 / 1000000 + WMR_lives_saved_2022 / 1000000), color = "magenta") + 
  geom_line(data = lives_saved_total_world |> filter(year >= 2022), aes(x = year, y = lives_saved_worst_case_itn60_2023 / 1000000 + WMR_lives_saved_2022 / 1000000), color = "magenta", lty = 2) + 
  geom_vline(xintercept = 2022, lty = 2) + 
  labs(x = "year", y = "lives saved (millions)")

lives_saved_total_world |> filter(year == 2022) |> dplyr::select(lives_saved_WMR2000) |> as.numeric()
lives_saved_total_world |> filter(year == 2040) |> dplyr::select(lives_saved_worst_case_itn60_2023) |> as.numeric()
lives_saved_total_world |> filter(year == 2040) |> dplyr::select(lives_saved_worst_case_itn60_2023_u5) |> as.numeric()


# plot annual deaths
WMR_lives_saved_2022 <- lives_saved_total_world |> filter(year == 2022) |> dplyr::select(lives_saved_WMR2000) |> as.numeric()

# scaling factor for 2022 annual deaths
WMR_scale_2022_MR <- MR |>
  filter(year == 2022) |>
  summarize(MR_scale = WMRdeaths / new_tools_deaths) |> as.numeric()

ggplot(data = MR) + 
  geom_line(data = MR |> filter(year >= 2022), aes(x = year, y = new_tools_deaths / 1000 * WMR_scale_2022_MR), color = "cornflowerblue", lty = 2) + 
  geom_line(data = MR |> filter(year >= 2022), aes(x = year, y = itn_60_deaths / 1000 * WMR_scale_2022_MR), color = "lightblue", lty = 2) +
  # geom_line(data = MR |> filter(year >= 2022), aes(x = year, y = best_case_deaths / 1000 * WMR_scale_2022_MR), color = "blue", lty = 2) + 
  geom_line(data = MR |> filter(year >= 2022), aes(x = year, y = worst_case_deaths / 1000 * WMR_scale_2022_MR), color = "tomato", lty = 2) +
  geom_line(aes(x = year, y = WMRdeaths / 1000), color = "black", lty = 1) + 
  geom_vline(xintercept = 2022, lty = 2) + 
  scale_y_continuous(limits = c(0, 1600), breaks = seq(0, 1500, 500)) +
  labs(x = "year", y = "annual deaths per 1,000 people")

1 - (MR |> filter(year == 2040) |> dplyr::select(new_tools_deaths) |> as.numeric()) / (MR |> filter(year == 2022) |> dplyr::select(new_tools_deaths) |> as.numeric())


# scaling factor for 2022 pop 
WMR_scale_2022_MR <- MR |>
  filter(year == 2022) |>
  summarize(MR_scale = WMRpop / model_pop) |> as.numeric()

ggplot(data = MR) + 
  geom_line(data = MR |> filter(year >= 2022), aes(x = year, y = model_pop / 1000000 * WMR_scale_2022_MR), color = "cornflowerblue", lty = 1) + 
  geom_line(data = MR |> filter(year >= 2022), aes(x = year, y = model_pop / 1000000 * WMR_scale_2022_MR), color = "tomato", lty = 2) +
  geom_line(aes(x = year, y = WMRpop / 1000000), color = "black", lty = 1) + 
  geom_vline(xintercept = 2022, lty = 2) + 
  labs(x = "year", y = "population size (millions)")

(MR |> filter(year == 2040) |> dplyr::select(model_pop) |> as.numeric()) / (MR |> filter(year == 2022) |> dplyr::select(model_pop) |> as.numeric())

