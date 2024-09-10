# Read in site files
source("./data_and_libraries.R")


# read in model countries from VIMC list
coverage <- read.csv("M:/Lydia/malaria_no_more/src/model_country/bluesky_r21.csv")
vimc_iso3cs <- unique(coverage$country_code)
extra_iso3cs <- c('BWA', 'GNQ', 'ERI', 'GAB', 'GMB', 'NAM', 'RWA', 'SEN', 'ZWE')
iso3cs <- c(vimc_iso3cs, extra_iso3cs)

find_itn_eir <- function(iso3c){
  
  if(iso3c %in% extra_iso3cs){
    site_data <- readRDS(paste0('M:/Lydia/malaria_no_more/src/model_country/original_site_files/', iso3c, '.RDS'))
  } 
  
  if(iso3c == 'UGA'){
    site_data <- readRDS(paste0('M:/Lydia/malaria_no_more/src/model_country/site_files/', iso3c, '.RDS'))
  }
  
  if(iso3c %in% vimc_iso3cs & iso3c != 'UGA'){
    site_data <- readRDS(paste0('M:/Lydia/malaria_no_more/src/model_country/site_files/', iso3c, '_new_EIR.rds'))
  } 
  
  itn_use <- site_data$interventions |> filter(year %in% c(2021:2022)) |> 
    group_by(iso3c, name_1, urban_rural) |>
    summarize(itn_use = mean(itn_use, na.rm = TRUE)) |> ungroup()
  
  eir <- site_data$eir |> filter(spp == "pf") |> dplyr::select(iso3c, name_1, urban_rural, eir)
  
  pfpr <- site_data$prevalence |> filter(year == 2019) |> dplyr::select(iso3c, name_1, urban_rural, pfpr)
  
  pop <- site_data$population |> filter(year == 2022) |> dplyr::select(iso3c, name_1, urban_rural, pop)
  
  itn_eir <- full_join(itn_use, eir) |> full_join(pop) |> full_join(pfpr)
  
  return(itn_eir)

}

combined <- map_dfr(iso3cs, find_itn_eir)

names(combined)

summary(combined$pop)

combined2 <- combined |> mutate(pop_q = case_when(pop <= 256101 ~ 1,
                                     pop > 256101 & pop <= 750773 ~ 2,
                                     pop > 750773 & pop <= 1641284 ~ 3,
                                     pop > 1641284 ~ 4)) |>
  mutate(pop_q = as.factor(pop_q))

table(combined2$pop_q)

ggplot(data = combined2 |> filter(eir > 0)) + 
  geom_point(aes(x = eir, y = itn_use, color = pop_q), alpha = 0.5) + 
  geom_hline(yintercept = 0.60, lty = 2) +
  geom_vline(xintercept = 50, lty = 2) +
  scale_colour_brewer() +
  theme_classic() + 
  scale_x_continuous(trans = 'log10') + 
  labs(x = "EIR", y = "mean ITN use 2021-2022", color = "population \nquartile")

ggplot(data = combined2) + 
  geom_point(aes(x = pfpr, y = itn_use, color = pop_q), alpha = 0.5) + 
  geom_hline(yintercept = 0.60, lty = 2) +
  geom_vline(xintercept = 0.35, lty = 2) +
  scale_colour_brewer() +
  theme_classic() + 
  labs(x = "PfPR", y = "mean ITN use 2021-2022", color = "population \nquartile")

