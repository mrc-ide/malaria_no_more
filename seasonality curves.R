# weighting cases and deaths seasonally
source("./data_and_libraries.R")

# create list of all site file countries
files <- list.files(path = "M:/Lydia/site_files/", full.names = FALSE)
files <- str_remove(files, ".RDS")


# function to output seasonality weights
seasonalw <- function(country){
  
  # read in site file
  site <- readRDS(paste0("M:/Lydia/site_files/", country, ".rds"))
  
  # assign fourier parameters
  g0 = site$seasonality$g0
  g1 = site$seasonality$g1
  g2 = site$seasonality$g2
  g3 = site$seasonality$g3
  h1 = site$seasonality$h1
  h2 = site$seasonality$h2
  h3 = site$seasonality$h3
  
  # predict across 365 days
  predict <- umbrella::fourier_predict(coef = c(g0, g1, g2, g3, h1, h2, h3), t = seq_len(365), floor = 0)
  
  # consolidate into months
  predict_short <- predict |> mutate(date = ymd("2000-01-01") + days(as.integer(t) - 1),
                    month = month(date)) |>
    mutate(estimate = profile / sum(profile)) |>
    group_by(month) |>
    summarize(sum = sum(estimate)) 
  
  # check that estimates sum to 1
  if(round(sum(predict_short$sum), 3) != 1){
    stop("Error: predictions do not sum to 1.")
  }
  
  saveRDS(predict_short, paste0("./files/seasonal weights/", country, ".rds"))
  
  print(paste0("Ran ", country))

}

# run through function for all site files
map(files, seasonalw)

