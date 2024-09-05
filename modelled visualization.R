# WHO country elimination maps
source("./data_and_libraries.R")

# read in orderly output
output <- readRDS("M:/Lydia/malaria_no_more/outputs/full_output.rds")

head(output)

# concatenate over age
output_month <- output |> group_by(month, country, country_name, site_name, urban_rural, scenario) |>
  summarize(clinical = mean(clinical),
            mortality = mean(mortality)) |>
  mutate(year = ceiling(month / 12))

output_year <- output_month |> group_by(year, country, country_name, site_name, urban_rural, scenario) |>
  summarize(clinical = mean(clinical),
            mortality = mean(mortality)) 

head(output_year)

KEN_month <- output_month |> filter(country_name == "Kenya")
KEN_year <- output_year |> filter(country_name == "Kenya")

ggplot(data = KEN_month) + 
  geom_line(aes(x = month, y = clinical, color = scenario, group = scenario)) + 
  facet_wrap(~ site_name) + 
  theme_classic()

ggplot(data = KEN_year) + 
  geom_line(aes(x = year, y = clinical, color = scenario, group = scenario)) + 
  facet_wrap(~ site_name) + 
  theme_classic()

# we will need to sum cases and deaths prior to calculating annual rates. 
# we will need to sum over age groups prior to calculating rates. 

# link to shapefiles
GADM_SSA_1 <- readRDS("./files/GADM_SSA_1.rds")  |> dplyr::select(ID_0, ID_1, COUNTRY, NAME_1)


output_year <- output_year |>
  mutate(site_name = case_when(country == "AGO" & site_name == "Bie" ~ "Bié",
                               country == "AGO" & site_name == "Huila" ~ "Huíla",
                               country == "AGO" & site_name == "Uige" ~ "Uíge",
                               
                               country == "BEN" & site_name == "Oueme" ~ "Ouémé",
                               
                               country == "CAF" & site_name == "Kemo" ~ "Kémo",
                               country == "CAF" & site_name == "Mambere-Kadei" ~ "Mambéré-Kadéï",
                               country == "CAF" & site_name == "Nana-Grébizi" ~ "Nana-Grébizi",
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
                               
                               TRUE ~ site_name
                               
  ))

linked <- output_year |> left_join(GADM_SSA_1, by = c("country" = "ID_0", "site_name" = "NAME_1") )

notlinked <- linked |> ungroup() |>
  filter(is.na(ID_1)) |> dplyr::select(country, country_name, site_name) |>
  distinct()



# see how many did not run on the HPC for both scenarios
failed <- output_year |> filter(year == 1) |>
  group_by(country, country_name, site_name) |>
  summarize(n = n()) |>
  filter(n == 1)

unique(failed$country_name)  


table(output$scenario)

output_new_tools <- linked |> 
  filter(scenario == "new_tools") |>
  st_as_sf()
  
output_vaccine_scaleup <- linked |> 
  filter(scenario == "vaccine_scaleup") |>
  st_as_sf()

ggplot(data = output_new_tools |> filter(year <= 10)) + 
  geom_sf(aes(fill = clinical)) + 
  facet_wrap(~ year) + 
  theme_classic() 

  

