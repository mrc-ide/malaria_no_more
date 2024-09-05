# title   ethiopia recalibration
# author  Lydia Haile
# purpose recalibrate sites in Ethiopia to match MAP prevalence after turning IRS off

library(vimcmalaria)
library(cali)
library(malariasimulation)
library(dplyr)
library(data.table)
source('J:/malaria_no_more/src/model_country/MNM_functions.R')
# vimc inputs ----
site_data<- readRDS('J:/malaria_no_more/src/model_country/site_files/ETH_new_eir.rds')
coverage_data<- read.csv('J:/malaria_no_more/src/model_country/coverage_80.csv')

# turn IRS coverage off
site_data$interventions$irs_cov = 0

# write recalibration function for each site of ethiopia
site<- 'Oromia'

recalibrate<- function(site){
  
  

model_input <- parameterise_mnm(
  site_name = site,
  ur = 'both',
  iso3c = 'ETH',
  site_data = site_data,
  coverage_data,
  scenario = 'vaccine_scaleup'
)
  summary_mean_pfpr_2_10 <- function (x) {
    message('calibrating')
    x<- data.table(x)
    # Calculate the PfPR2-10:
    prev_2_10 <- mean(x[timestep %in% c((10*365):(11*365))]$n_detect_pcr_730_3649/x[timestep %in% c((10*365):(11* 365))]$n_730_3649) # average over a year 
    
    # Return the calculated PfPR2-10:
    return(prev_2_10)
  }
  

  # pull target pfpr from 2010 for corresponding site
  target_pfpr <- site_data$prevalence |> filter(year == 2011, name_1 == site) |> pull(pfpr)
  
  print(paste0('target pfpr ', target_pfpr ))
  
  # Add a parameter to the parameter list specifying the number of timesteps 
  simparams<- model_input$param_list
  simparams$timesteps <- 12 * 365
  
  # Establish a tolerance value:
  pfpr_tolerance <- 0.01
  
  # Set upper and lower EIR bounds for the calibrate function to check
  lower_EIR <- 0.01
  upper_EIR <- 60
  
  #output<- run_simulation(timesteps = simparams$timesteps, parameters = simparams)
  # Run the calibrate() function:
  cali_EIR <- calibrate(target = target_pfpr,
                        summary_function = summary_mean_pfpr_2_10,
                        parameters = simparams,
                        tolerance = pfpr_tolerance, 
                        low = lower_EIR, high = upper_EIR)
  
  print(paste0('calibrated EIR for site ', site, ' :', cali_EIR))

  return(cali_EIR)
}

