# orderly metadata  ----
orderly2::orderly_parameters(iso3c = 'NGA',
                             scenario = 'test')

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
coverage_data<- read.csv('routine_r21.csv') |> filter(year <= 2040)
site_data <- readRDS(paste0('site_files/', iso3c, '_new_EIR.rds'))

# make a map of input parameters for site function
site_df<- remove_zero_eirs(iso3c, site_data)


map<- make_analysis_map(site_df, site_data, test = FALSE)

# test parameterisation
testing<- parameterise_mnm(site_name = 'Lagos',
                 ur= 'both',
                 iso3c= 'NGA',
                 site_data,
                 coverage_data,
                 scenario = 'new_tools')



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
    library(data.table)
    library(dplyr)
    library(scene)
    library(malariasimulation)
    library(tidyr)
    library(tibble)
    library(postie)
    library(countrycode)
    library(site)
    library(vimcmalaria)
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

# reformat outputs into separate data frames
test<- reformat_output(output)
processed_results<- test$processed_full
raw_output<- test$raw_full


# aggregate outputs up to country level
if(scenario == 'no-vaccination'){
  
  dt<- aggregate_outputs(processed_results, pop_single_yr)
  
} else{
  
  dt<- data.table()
}


#save every output to one list
outputs<- list('country_output' = dt,
               'site_output' = output,
               'raw_output' = raw_output)


saveRDS(outputs, 'outputs.rds')