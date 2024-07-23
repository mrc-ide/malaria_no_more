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
new_tools<- 'new_tools'
scaleup<- 'vaccine_scaleup'
baseline<- 'no-vaccination'

orderly2::orderly_dependency("model_country", "latest(parameter:iso3c == this:iso3c &&
                             parameter:scenario == environment:new_tools &&
                             parameter:description == environment:description)", c(new_tools.rds = "outputs.rds"))
orderly2::orderly_dependency("model_country", "latest(parameter:iso3c == this:iso3c &&
                             parameter:scenario == environment:scaleup &&
                             parameter:description == environment:description)", c(scaleup.rds = "outputs.rds"))
# orderly2::orderly_dependency("model_country", "latest(parameter:iso3c == this:iso3c &&
#                              parameter:scenario == environment:baseline)", c(baseline.rds = "outputs.rds"))


new_tools<- readRDS('new_tools.rds')
scaleup<- readRDS('scaleup.rds')
#baseline<- readRDS('baseline.rds')


annual<- bind_rows(new_tools$annual, scaleup$annual)
monthly<- bind_rows(new_tools$monthly, scaleup$monthly)

annual<- annual |>
  group_by(site_name, urban_rural, scenario, year) |>
  mutate(cases = clinical * prop_n * pop,
         deaths = mortality * prop_n * pop) |>
  summarise(cases = sum(cases),
            deaths = sum(deaths),
            pop = sum(pop),
            .groups = 'keep') |>
  mutate(clinical = cases/pop,
         mortality = deaths/ pop)

monthly<- monthly |>
  group_by(site_name, urban_rural, scenario, year) |>
  mutate(cases = clinical * prop_n * pop,
         deaths = mortality * prop_n * pop) |>
  summarise(cases = sum(cases),
            deaths = sum(deaths),
            pop = sum(pop),
            .groups = 'keep') |>
  mutate(clinical = cases/pop,
         mortality = deaths/pop)

saveRDS(annual, 'annual_output.rds')
saveRDS(monthly, 'monthly_output.rds')

# quick plot of outputs for 1-year olds
# pdf('ben_plots.pdf')
# 
# for (site in unique(annual$site_name)){
#   
#   message(site)
#   plot<- annual |> filter(year >= 2000 & site_name == site)
#   p<- ggplot(data= plot, mapping = aes(x= year, y= clinical, color= scenario, fill= scenario)) +
#     geom_line(lwd= 0.5) +
#     #facet_wrap_paginate(~site_name, scales = 'free', ncol = 2, nrow = 2, page = 1) +
#     theme_classic() +
#     labs(x= 'Year',
#          y= 'Clinical incidence rate in 1-year olds',
#          title= 'Clinical incidence over time by scenario, Burkina Faso',
#          subtitle = site)
#   
#   
#   
#   print(p)
#   
# }
# dev.off()
