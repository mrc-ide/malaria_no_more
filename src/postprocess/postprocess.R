# orderly metadata  ----
orderly2::orderly_parameters(iso3c = NULL,
                             description = NULL)

orderly2::orderly_description('Process and plot country scenarios for Malaria No More Artwork')

# packages and functions ----
library(site)
library(data.table)
library(tidyr)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(ggforce)

# list of scenarios
intvns<- c('vaccines', 'txdx', 'genedrive', 'nets')
scenarios <- c(
  intvns, # choose 1
  "vaccines_txdx", "vaccines_genedrive", "vaccines_nets", "txdx_genedrive", "txdx_nets", "genedrive_nets", # choose 2
  "vaccines_txdx_genedrive", "vaccines_txdx_nets", "vaccines_genedrive_nets", "txdx_genedrive_nets", # choose 3
  "vaccines_txdx_genedrive_nets", # choose 4
  "no_intvns"
) # choose none
  
annual<- data.table()
monthly<- data.table()
  
  for (scenario in scenarios) {
    
    message(scenario)
    metadata<-orderly2::orderly_dependency("model_country", quote(latest(parameter:iso3c == this:iso3c &&
                                                                   parameter:description == this:description &&
                                                                   parameter:scenrario == environmet:scenario)),
                                           c(file.rds = "outputs.rds"))
    
    ann<- readRDS(metadata$files$here)$annual
    month<- readRDS(metadata$files$here)$monthly

    annual<- bind_rows(annual, ann, fill = TRUE)
    monthly<- bind_rows(monthly, month, fill = TRUE)
    
}

# pull and modify annual rates under 5 after 2025 down 20% (assuming rectal artenusate administration)
annual<- annual |>
  mutate(mortality = ifelse(year > 2024 & age <= 5 & scenario == 'txdx', mortality*.8, mortality))

# aggregate up deaths under 5 to visualize in separate dataset
annual_children<- annual |>
  group_by(country, country_name, site_name, urban_rural, scenario, year, description) |>
  filter(age < 5.01) |>
    mutate(
      cases = clinical * prop_n * pop,
      deaths = mortality * prop_n * pop,
      pop= prop_n * pop
    ) |>
    summarise(
      cases = sum(cases),
      deaths = sum(deaths),
      pop = sum(pop), # population is the same regardless of age group so will retain true value
      .groups = "keep"
    ) |>
    mutate(
      clinical = cases / pop,
      mortality = deaths / pop
    ) |>
    filter(site_name != TRUE) |>
    mutate(cases = ifelse(is.na(cases), 0, cases),
           deaths = ifelse(is.na(deaths), 0, deaths)) |>
    mutate(clinical = ifelse(is.na(clinical), 0, clinical),
          mortality = ifelse(is.na(mortality), 0, mortality)) 
  
annual_agg <- annual |>
  group_by(country, country_name, site_name, urban_rural, scenario, year, description) |>
  mutate(
    cases = clinical * prop_n * pop,
    deaths = mortality * prop_n * pop
  ) |>
  summarise(
    cases = sum(cases),
    deaths = sum(deaths),
    pop = mean(pop), # population is the same regardless of age group so will retain true value
    .groups = "keep"
  ) |>
  mutate(
    clinical = cases / pop,
    mortality = deaths / pop
  )  |>
  mutate(cases = clinical * pop) |> # backcalculate smoothed cases
  filter(site_name != TRUE) |>
  mutate(cases = ifelse(is.na(cases), 0, cases),
         deaths = ifelse(is.na(deaths), 0, deaths)) |>
  mutate(clinical = ifelse(is.na(clinical), 0, clinical),
        mortality = ifelse(is.na(mortality), 0, mortality)) 



# calculate lives saved
worst_case<- annual_agg |>
  filter(scenario == 'worst_case') |>
  rename(deaths_baseline = deaths) |>
  ungroup() |>
  select(site_name, urban_rural, year, deaths_baseline) 

annual_agg<- merge(worst_case, annual_agg, by = c('site_name', 'urban_rural', 'year')) 

annual_agg<- annual_agg |>
  mutate(lives_saved = deaths_baseline - deaths)

monthly_agg <- monthly |>
  group_by(country, country_name, site_name, urban_rural, scenario, month, description) |>
  mutate(
    cases = clinical * prop_n * pop,
    deaths = mortality * prop_n * pop
  ) |>
  summarise(
    cases = sum(cases),
    deaths = sum(deaths),
    pop = mean(pop), # population is the same regardless of age group so will retain true value
    .groups = "keep"
  ) |>
  mutate(
    clinical = cases / pop,
    mortality = deaths / pop
  ) |>
  filter(site_name != TRUE) |>
    mutate(cases = ifelse(is.na(cases), 0, cases),
           deaths = ifelse(is.na(deaths), 0, deaths)) |>
    mutate(clinical = ifelse(is.na(clinical), 0, clinical),
          mortality = ifelse(is.na(mortality), 0, mortality)) 

worst_case_monthly<- monthly_agg |>
  filter(scenario == 'worst_case') |>
  rename(deaths_baseline = deaths) |>
  ungroup() |>
  select(site_name, urban_rural, month, deaths_baseline)

monthly_agg<- merge(worst_case_monthly, monthly_agg, by = c( 'site_name', 'urban_rural', 'month')) 
monthly_agg<- monthly_agg |>
  mutate(lives_saved = deaths_baseline - deaths)


saveRDS(annual_agg, 'annual_output.rds')
saveRDS(annual_children, 'annual_children.rds')
saveRDS(monthly_agg, 'monthly_output.rds')

# quick plot of outputs 
# pdf('ben_plots.pdf')
#   message(site)
# annual_agg<- data.table(annual_agg)


    p<- ggplot(data= annual_agg, mapping = aes(x= year, y= clinical, color= scenario, fill= scenario)) +
    geom_line(lwd= 0.5) +
    facet_wrap(~site_name) +
    theme_classic() +
    labs(x= 'Year',
         y= 'Clinical incidence, all-age',
         title= 'All-age clinical incidence over time by scenario',
         subtitle = iso3c)
  
  print(p)
# dev.off()
