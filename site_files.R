# Read in site files
source("./data_and_libraries.R")

# list all files in the directory
file_names <- list.files(path = "M:/Lydia/site_files/", pattern = "*.RDS", full.names = FALSE)

# remove the .RDS extension from each file name
file_names_no_ext <- gsub(".RDS", "", file_names)

africa <- c("DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CMR", "CPV", "CAF", "TCD",
            "COM", "COG", "COD", "CIV", "DJI", "EGY", "GNQ", "ERI", "ETH", "GAB",
            "GMB", "GHA", "GIN", "GNB", "KEN", "LSO", "LBR", "LBY", "MDG", "MLI",
            "MWI", "MRT", "MUS", "MYT", "MAR", "MOZ", "NAM", "NER", "NGA", "REU",
            "RWA", "STP", "SEN", "SYC", "SLE", "SOM", "ZAF", "SSD", "SDN", "SWZ",
            "TZA", "TGO", "TUN", "UGA", "ESH", "ZMB", "ZWE")

# keep site files in SSA
africa_site_iso <- intersect(file_names_no_ext, africa)


# pull data from site files
site_lookup <- function(ISO){
  
  # pull site
  site <- readRDS(paste0("M:/Lydia/site_files/", ISO, ".rds"))
  
  # extract cases and deaths
  cases_deaths <- site$cases_deaths
  
  # extract population
  pop <- site$population |>
    dplyr::select(iso3c, year, par) |>
    group_by(iso3c, year) |>
    summarize(par = sum(par)) |>
    filter(year <= 2020) 
  
  summary <- cases_deaths |> 
    dplyr::select(iso3c, country, year, wmr_cases, wmr_deaths) |>
    left_join(pop) |>
    rowwise() |>
    mutate(clinical = wmr_cases / par * 10000,
           mortality = wmr_deaths / par * 100000)
  
  return(summary)
  
}

all_sites <- map_dfr(africa_site_iso, site_lookup)

# calculate cases and deaths averted since 2000
diff <- all_sites |>
  group_by(iso3c, country) |>
  mutate(cases_averted = first(wmr_cases) - wmr_cases,
         deaths_averted = first(wmr_deaths) - wmr_deaths) |>
  mutate(cases_averted_cumulative = cumsum(cases_averted),
         deaths_averted_cumulative = cumsum(deaths_averted),
         mortality_diff = mortality - first(mortality))


# make sure things are trending in the right direction
ggplot(data = diff) + 
  geom_hline(yintercept = 0, lty = 2) + 
  geom_line(aes(x = year, y = deaths_averted_cumulative)) + 
  facet_wrap(~ country)
  
ggplot(data = diff) + 
  geom_hline(yintercept = 0, lty = 2) + 
  geom_line(aes(x = year, y = cases_averted_cumulative)) + 
  facet_wrap(~ country)

ggplot(data = diff) + 
  geom_hline(yintercept = 0, lty = 2) + 
  geom_line(aes(x = year, y = clinical)) + 
  facet_wrap(~ country)

ggplot(data = diff) + 
  geom_hline(yintercept = 0, lty = 2) + 
  geom_line(aes(x = year, y = mortality)) + 
  facet_wrap(~ country)

ggplot(data = diff) + 
  geom_hline(yintercept = 0, lty = 2) + 
  geom_line(aes(x = year, y = mortality_diff)) + 
  facet_wrap(~ country)


# plots to send to groups
A <- ggplot(data = diff) + 
  geom_hline(yintercept = 0, lty = 2) + 
  geom_line(aes(x = year, y = deaths_averted_cumulative)) + 
  labs(x = "year", y = "cumulative deaths averted") + 
  theme_classic() + 
  facet_wrap(~ country)

B <- ggplot(data = diff) + 
  geom_hline(yintercept = 0, lty = 2) + 
  geom_line(aes(x = year, y = mortality_diff)) + 
  labs(x = "year", y = "change in annual mortality rate since 2000 \nper 100,000 people") + 
  theme_classic() + 
  facet_wrap(~ country)

A + B


# output
output <- diff |> 
  dplyr::select(iso3c, country, year, deaths_averted_cumulative, mortality_diff) |>
  rename(lives_saved = deaths_averted_cumulative) |>
  mutate(lives_saved = case_when(lives_saved < 0 ~ 0,
                                 TRUE ~ lives_saved)) |>
  mutate(mortality_diff = round(mortality_diff, 2)) |>
  filter(year == 2015)

sum(output$lives_saved)

write_csv(output, "./files/WMR_lives_saved_2015.csv")
