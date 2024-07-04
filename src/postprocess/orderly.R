# orderly metadata  ----
orderly2::orderly_parameters(iso3c = 'BFA')

orderly2::orderly_description('Process and plot country scenarios for Malaria No More Artwork')

# packages and functions ----
library(site)
library(data.table)
library(tidyr)
library(ggplot2)

new_tools<- 'new_tools'
scaleup<- 'vaccine_scaleup'
baseline<- 'no-vaccination'

orderly2::orderly_dependency("model_country", "latest(parameter:iso3c == this:iso3c &&
                             parameter:scenario == environment:new_tools)", c(new_tools.rds = "outputs.rds"))
orderly2::orderly_dependency("model_country", "latest(parameter:iso3c == this:iso3c &&
                             parameter:scenario == environment:scaleup)", c(scaleup.rds = "outputs.rds"))
orderly2::orderly_dependency("model_country", "latest(parameter:iso3c == this:iso3c &&
                             parameter:scenario == environment:baseline)", c(baseline.rds = "outputs.rds"))


new_tools<- readRDS('new_tools.rds')
scaleup<- readRDS('scaleup.rds')
baseline<- readRDS('baseline.rds')


outputs<- bind_rows(new_tools, scaleup, baseline)


# quick plot of outputs for 1-year olds
plot<- outputs |> filter(age == 1, site_name== 'Cascades')

p<- ggplot(data= plot, mapping = aes(x= (month/12) +2000, y= clinical, color= scenario, fill= scenario)) +
  geom_line() +
  facet_wrap(~site_name, scales = 'free') +
  theme_classic() +
  labs(x= 'Year',
       y= 'Clinical incidence rate in 1-year olds',
       title= 'Clinical incidence over time by scenario')


