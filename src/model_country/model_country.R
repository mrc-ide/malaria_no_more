# orderly metadata  ----
orderly2::orderly_parameters(
  iso3c = 'NGA',
  scenario  ='best_case',
  description  = 'testing'
)


orderly2::orderly_description('Model country scenarios for Malaria No More Artwork')
orderly2::orderly_artefact('Model output', 'outputs.rds')
# packages and functions ----
library(site)
library(data.table)
library(dplyr)
library(scene)
library(malariasimulation)
library(tidyr)
library(tibble)
library(postie)
library(countrycode)
library(vimcmalaria)

source('MNM_functions.R')
# read in dependencies  ----
coverage_data<- read.csv('coverage_80.csv') |> filter(year <= 2040) # 'bluesky_r21.csv'
extra_iso3cs<- extra_iso3cs<- c('BWA', 'GNQ', 'ERI', 'GAB', 'GMB', 'NAM', 'RWA', 'SEN', 'ZWE')

if (iso3c %in% extra_iso3cs){

  site_data <- readRDS(paste0('original_site_files/', iso3c, '.RDS'))

  #pull coverage data for another country and rename to country of interest
  # need to do this for vimc functions to work
  # note vaccine coverage is uniform across all countries for these runs
  coverage_data<- coverage_data |> 
    filter(country_code == 'NGA') |>
    mutate(country_code = iso3c)

} else{
  site_data <- readRDS(paste0('site_files/', iso3c, '_new_EIR.rds'))
}

if(iso3c == 'UGA'){
  site_data <- readRDS(paste0('site_files/', iso3c, '.RDS'))
  
}

# make a map of input parameters for site function
site_df<- remove_zero_eirs(iso3c, site_data)
map<- make_mnm_analysis_map(site_df, test = FALSE)

# run analysis function for each site + urban/rural combination ----
cluster_cores <- Sys.getenv("CCP_NUMCPUS")
if (cluster_cores == "") {
  message("running in serial (on a laptop?)")
  output<- lapply(map,
                  analyse_mnm,
                  site_data= site_data,
                  coverage_data=coverage_data,
                  scenario = {{scenario}})
 } else {
  message(sprintf("running in parallel on %s (on the cluster?)", cluster_cores))
  cl <- parallel::makeCluster(as.integer(cluster_cores))
  invisible(parallel::clusterCall(cl, ".libPaths", .libPaths()))
  parallel::clusterCall(cl, function() {
    message('running')
    library(site)
    library(data.table)
    library(dplyr)
    library(scene)
    library(malariasimulation)
    library(tidyr)
    library(tibble)
    library(postie)
    library(countrycode)
    library(vimcmalaria)

    source('MNM_functions.R')


    TRUE
  })

  output<- parallel::clusterApply(cl,
                                  map,
                                  analyse_mnm,
                                  site_data= site_data,
                                  coverage_data=coverage_data,
                                  scenario = {{scenario}})
  parallel::stopCluster(cl)
}

# reformat list outputs into large data frame
outputs<- reformat_output(output)

saveRDS(outputs, 'outputs.rds')