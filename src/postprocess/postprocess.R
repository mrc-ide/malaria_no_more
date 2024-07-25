# orderly metadata  ----
orderly2::orderly_parameters(iso3c = 'AGO',
                             description = 'full_run')

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

orderly2::orderly_dependency("model_country", "latest(parameter:iso3c == this:iso3c &&
                             parameter:scenario == environment:new_tools &&
                             parameter:description == this:description)", c(new_tools.rds = "outputs.rds"))
orderly2::orderly_dependency("model_country", "latest(parameter:iso3c == this:iso3c &&
                             parameter:scenario == environment:scaleup &&
                             parameter:description == this:description)", c(scaleup.rds = "outputs.rds"))
                            

new_tools<- readRDS('new_tools.rds')
scaleup<- readRDS('scaleup.rds')


annual<- bind_rows(new_tools$annual, scaleup$annual)
monthly<- bind_rows(new_tools$monthly, scaleup$monthly)

annual_agg <- annual |>
  group_by(site_name, urban_rural, scenario, year) |>
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
  filter(site_name != TRUE)



monthly_agg <- monthly |>
  group_by(site_name, urban_rural, scenario, month) |>
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
  filter(site_name != TRUE)


saveRDS(annual_agg, 'annual_output.rds')
saveRDS(monthly_agg, 'monthly_output.rds')

# quick plot of outputs for 1-year olds
# pdf('ben_plots.pdf')
  
#   message(site)

#   p<- ggplot(data= annual_agg, mapping = aes(x= year, y= clinical, color= scenario, fill= scenario)) +
#     geom_line(lwd= 0.5) +
#     facet_wrap(~site_name) +
#     theme_classic() +
#     labs(x= 'Year',
#          y= 'Clinical incidence, all-age',
#          title= 'All-age clinical incidence over time by scenario',
#          subtitle = iso3c)
  
#     p<- ggplot(data= monthly_agg, mapping = aes(x= month, y= clinical, color= scenario, fill= scenario)) +
#     geom_line(lwd= 0.5) +
#     facet_wrap(~site_name) +
#     theme_classic() +
#     labs(x= 'Year',
#          y= 'Clinical incidence, all-age',
#          title= 'All-age clinical incidence over time by scenario',
#          subtitle = iso3c)
  
  
#   print(p)
  

# dev.off()
