# orderly metadata  ----
orderly2::orderly_parameters(iso3c = 'ETH',
                             description = 'recalibrate_ethiopia')

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
                            
descrip<- 'set_coverage_at_80'


orderly2::orderly_dependency("model_country", "latest(parameter:iso3c == this:iso3c &&
  parameter:scenario == environment:new_tools &&
  parameter:description == environment:descrip)", c(old_new_tools.rds = "outputs.rds"))
orderly2::orderly_dependency("model_country", "latest(parameter:iso3c == this:iso3c &&
  parameter:scenario == environment:scaleup &&
  parameter:description == environment:descrip)", c(old_scaleup.rds = "outputs.rds"))


new_tools<- readRDS('new_tools.rds')
scaleup<- readRDS('scaleup.rds')

old_new_tools<- readRDS('old_new_tools.rds')
olds_scaleup<- readRDS('old_scaleup.rds')


annual<- bind_rows(new_tools$annual, scaleup$annual)
monthly<- bind_rows(new_tools$monthly, scaleup$monthly)

annual_old<- bind_rows(old_new_tools$annual, olds_scaleup$annual)
annual_old<- annual_old |>
  mutate(description = 'older_run')
monthly_old<- bind_rows(olds_scaleup$monthly, old_new_tools$monthly)
monthly_old<- monthly_old |>
  mutate(description = 'older_run')

annual<-bind_rows(annual, annual_old)
monthly<- bind_rows(monthly, monthly_old)


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
  ) |>
  filter(site_name != TRUE)



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
  filter(site_name != TRUE)


saveRDS(annual_agg, 'annual_output.rds')
saveRDS(monthly_agg, 'monthly_output.rds')

# quick plot of outputs for 1-year olds
# pdf('ben_plots.pdf')
  
#   message(site)
annual_agg<- data.table(annual_agg)
  p<- ggplot(data= annual_agg[description == 'malaria_no_more_runs'], mapping = aes(x= year, y= clinical * 1000, color= scenario, fill= scenario)) +
    geom_line(lwd= 0.5) +
    facet_wrap(~site_name + description, scales= 'free') +
    theme_classic() +
    labs(x= 'Year',
         y= 'Clinical incidence per thousand, all-age',
         title= 'All-age clinical incidence over time by scenario',
         subtitle = iso3c)

p2 <- ggplot(data = annual_agg[description == "older_run"], mapping = aes(x = year, y = clinical * 1000, color = scenario, fill = scenario)) +
  geom_line(lwd = 0.5) +
  facet_wrap(~ site_name + description, scales = "free") +
  theme_classic() +
  labs(
    x = "Year",
    y = "Clinical incidence per thousand, all-age",
    title = "All-age clinical incidence over time by scenario",
    subtitle = iso3c
  )

  
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
