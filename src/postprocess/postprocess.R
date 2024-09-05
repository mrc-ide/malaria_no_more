# orderly metadata  ----
orderly2::orderly_parameters(iso3c = 'NGA',
                             description = 'gene_drive_fix')

orderly2::orderly_description('Process and plot country scenarios for Malaria No More Artwork')

# packages and functions ----
library(site)
library(data.table)
library(tidyr)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(ggforce)

new_tools<- 'new_tools'
scaleup<- 'vaccine_scaleup'
worst_case<- 'worst_case'
best_case<- 'best_case'
itn<- 'itn_change'

orderly2::orderly_dependency("model_country", "latest(parameter:iso3c == this:iso3c &&
                             parameter:scenario == environment:new_tools &&
                             parameter:description == this:description)", c(new_tools.rds = "outputs.rds"))
orderly2::orderly_dependency("model_country", "latest(parameter:iso3c == this:iso3c &&
                             parameter:scenario == environment:scaleup &&
                             parameter:description == this:description)", c(scaleup.rds = "outputs.rds"))
orderly2::orderly_dependency("model_country", "latest(parameter:iso3c == this:iso3c &&
                             parameter:scenario == environment:worst_case &&
                             parameter:description == this:description)", c(worst_case.rds = "outputs.rds"))                         
orderly2::orderly_dependency("model_country", "latest(parameter:iso3c == this:iso3c &&
                              parameter:scenario == environment:best_case &&
                              parameter:description == this:description)", c(best_case.rds = "outputs.rds"))
 orderly2::orderly_dependency("model_country", "latest(parameter:iso3c == this:iso3c &&
                              parameter:scenario == environment:itn &&
                              parameter:description == this:description)", c(itn_60.rds = "outputs.rds"))


new_tools<- readRDS('new_tools.rds')
scaleup<- readRDS('scaleup.rds')
worst_case<- readRDS('worst_case.rds')
best_case<- readRDS('best_case.rds')
itn_60<- readRDS('itn_60.rds')
# moving_average <- function(series, klags) {
#   return(
#       lag(
#           zoo::rollmean(series, klags, fill = NA), floor(klags / 2)
#       )
#   )
# }
annual<- bind_rows(new_tools$annual, scaleup$annual, worst_case$annual, best_case$annual, itn_60$annual)
monthly<- bind_rows(new_tools$monthly, scaleup$monthly, worst_case$monthly, best_case$monthly, itn_60$monthly)

# pull and modify annual rates under 5 after 2025 down 20% (assuming rectal artenusate administration)
annual<- annual |>
  mutate(mortality = ifelse(year > 2024 & age <= 5 & scenario == 'best_case', mortality*.8, mortality))

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
#   mutate(clinical_avg= moving_average(clinical, klags= 20)) |>
#   mutate(clinical_avg = ifelse(is.na(clinical_avg), clinical, clinical_avg)) |>
#   mutate(clinical = clinical_avg) |>
#  select(-clinical_avg) |>
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


  #   p<- ggplot(data= annual_agg, mapping = aes(x= year, y= clinical, color= scenario, fill= scenario)) +
  #   geom_line(lwd= 0.5) +
  #   facet_wrap(~site_name) +
  #   theme_classic() +
  #   labs(x= 'Year',
  #        y= 'Clinical incidence, all-age',
  #        title= 'All-age clinical incidence over time by scenario',
  #        subtitle = iso3c)
  
  # print(p)
# dev.off()
