


outputs<- read.csv('WMR_lives_saved_2022.csv')
outputs<- read.csv('outputs/updated_run_annual.csv')

outputs<- read.csv('modelling_lives_saved_2040_country.csv')
library(dplyr)

test<- outputs |>
  filter(year %in% c(2025:2039)) |>
  group_by(country) |> 
  summarise(lives_saved_cumulative = sum(lives_saved_cumulative),
            lives_saved_annual = sum(lives_saved_annual), .groups= 'keep') 

# |>
#   mutate(lives_saved_cumulative = lives_saved_cumulative * 1.285156,
#           lives_saved_annual= lives_saved_annual *1.285156)
