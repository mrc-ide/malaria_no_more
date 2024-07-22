# workflow for malaria no more runs --------------------------------------------

# if needed install packages
remotes::install_github('mrc-ide/orderly2')
remotes::install_github('mrc-ide/postie@dalys')
remotes::install_github('mrc-ide/scene')
remotes::install_github('mrc-ide/site@vimc')
remotes::install_github('mrc-ide/vimcmalaria')
remotes::install_github('mrc-ide/malariasimulation@dev')


install.packages('countrycode')

# load packages
library(orderly2)
library(malariasimulation)
library(vimcmalaria)


# initialise orderly2 repository if you have not already
#orderly2::orderly_init()

# scenarios to run:
# no-vaccination: coverage of other interventions remain constant from 2022 through 2040, no vaccines
# new_tools: addition of a blood-stage vaccine (IE RH5). In the absence of trial efficacy data, operationalized 
#            such that the blood-stage vaccine has 60% efficacy against  residual cases not protected by R21
# vaccine_scaleup: identical to routine scaleup of R21 vaccine for GAVI runs, with coverage varying by country

scenarios<- c('no-vaccination', 'new_tools', 'vaccine_scaleup')


# to run workflow:
# this will error out unless you have saved coverage and site file inputs in your src/model-country directory--
# contact Lydia for filepaths
hipercow::hipercow_init(driver = 'windows')
hipercow::hipercow_provision()
hipercow::hipercow_environment_create(sources = 'src/model_country/MNM_functions.R')
hipercow::hipercow_configuration()

# first set home directory to repository directory
coverage<- read.csv('src/model_country/bluesky_r21.csv')
iso3cs<- unique(coverage$country_code)

submit_country<- function(iso, scen, report_name){
  
  site_data <- readRDS(paste0('src/model_country/site_files/', iso, '_new_EIR.rds'))
  core<- nrow(site_data$sites)
  
  if(core> 32){
    core<- 32
  }
  
  if(report_name == 'model_country'){
    
    hipercow::task_create_expr(orderly2::orderly_run('model_country',
                                                     parameters= list(iso3c= iso,
                                                                      scenario = scen)),
                               resources = hipercow::hipercow_resources(cores = core))
    
  } else if (report_name == 'postprocess'){
    
   # hipercow::task_create_expr(
      orderly2::orderly_run('postprocess',
                                                     parameters = list(iso3c = iso))
    #)
  }
}

# run model country
lapply('AGO', submit_country, report_name = 'model_country', scen = 'new_tools')

# run postprocessing
lapply(iso3cs[2:31], submit_country, report_name = 'postprocess', scen = NULL)



# pull model outputs into large file to save

#' Pull final outputs from workflow
#' @param descrip         description of runs to pull
#' @export
compile_mnm_outputs<- function(){
  
  completed<- vimcmalaria::completed_reports('model_country')
  completed<- completed |>
    dplyr::arrange(desc(date_time)) |>
    dplyr::distinct(iso3c, scenario, .keep_all = TRUE) |>
    dplyr::arrange(iso3c, scenario)
  
  pull_output<- function(index, map){
    
    message(index)
    map<- map[ index,]
    directory_name<- map$directory_name
    iso3c<- map$iso3c
    output<- readRDS(paste0('J:/malaria_no_more/archive/model_country/', directory_name, '/outputs.rds'))
    return(output)
  }
  
  outputs<- rbindlist(lapply(c(1:nrow(completed)), pull_output, map = completed))
}

# pull all of the outputs
outputs<- compile_mnm_outputs()
