# workflow for malaria no more runs --------------------------------------------
# if needed install packages
#install.packages('remotes')
#remotes::install_github('mrc-ide/orderly2')
#remotes::install_github('mrc-ide/postie@dalys')
#remotes::install_github('mrc-ide/scene')
#remotes::install_github('mrc-ide/site@vimc')
#remotes::install_github('mrc-ide/malariasimulation@dev')
#remotes::install_github('mrc-ide/vimcmalaria')


# load packages
library(orderly2)
library(malariasimulation)
library(vimcmalaria)
library(hipercow)
library(dplyr)
library(data.table)
library(postie)
library(scene)

# initialise orderly2 repository if you have not already
#orderly2::orderly_init()
# scenarios to run:
# no-vaccination: coverage of other interventions remain constant from 2022 through 2040, no vaccines
# new_tools: addition of a blood-stage vaccine (IE RH5). In the absence of trial efficacy data, operationalized 
#            such that the blood-stage vaccine has 60% efficacy against  residual cases not protected by R21
# vaccine_scaleup: 80% coverage of R21 (and 90% booster coverage) beginning in 2022
# worst_case: remove all interventions after 2022

scenarios<- c('no-vaccination', 'new_tools', 'vaccine_scaleup', 'worst_case')


# to run workflow:
# this will error out unless you have saved coverage and site file inputs in your src/model-country directory--
# contact Lydia for filepaths
hipercow::hipercow_init(driver = 'windows')
#hipercow::hipercow_provision()
hipercow::hipercow_environment_create(sources = 'src/model_country/MNM_functions.R')
hipercow::hipercow_configuration()

# first set home directory to repository directory
coverage<- read.csv('src/model_country/bluesky_r21.csv')
vimc_iso3cs<- unique(coverage$country_code)
extra_iso3cs<- c('BWA', 'GNQ', 'ERI', 'GAB', 'GMB', 'NAM', 'RWA', 'SEN', 'ZWE')

#see what reports from the worst case scenario have compeleted ( a few are still running on the cluster)
reports<- completed_reports('model_country') |> filter(scenario == 'worst_case')
iso3cs_done<- unique(reports$iso3c) # will need to run remainder when all jobs complete



submit_country<- function(iso, scen, descrip, report_name){
  if (iso %in% vimc_iso3cs){

    site_data <- readRDS(paste0('src/model_country/site_files/', iso, '_new_EIR.rds'))

  } else if (iso %in% extra_iso3cs){

    site_data <- readRDS(paste0('src/model_country/original_site_files/', iso, '.RDS'))


  }  
  
  if (iso == 'UGA'){
    site_data <- readRDS(paste0('src/model_country/original_site_files/', iso, '.RDS'))
  }

  core<- nrow(site_data$sites)
  
  if(core> 32){
    
    core<- 32
  }
  
  if(report_name == 'model_country'){
    
    hipercow::task_create_expr(
      orderly2::orderly_run('model_country',
                             parameters= list(iso3c= iso,
                                              scenario = scen,
                                              description = descrip)),
                             resources = hipercow::hipercow_resources(cores = core)
      )
    
  } else if (report_name == 'postprocess'){

      orderly2::orderly_run('postprocess', parameters = list(iso3c = iso,
                                                             description= descrip))
  }
}

# run model country
lapply(
  c('AGO', 'COD', 'COG', 'GAB'), #  c(vimc_iso3cs, extra_iso3cs)
  submit_country,
  report_name = 'model_country',
  scen = 'new_tools', # c('no-vaccination', 'new_tools', 'vaccine_scaleup', 'worst_case')
  descrip = 'gene_drive_fix' # 'scale_tx_cov'
)


# run postprocessing
lapply(
  c(vimc_iso3cs, extra_iso3cs),
  submit_country,
  report_name = 'postprocess',
  scen = "worst_case", # 'new_tools', 'vaccine_scaleup', 'worst_case'
  descrip = 'gene_drive_fix'
)

# FAILING
# new_tools: AGO, COD, COG, GAB

# resolved bugs:
# some sites have more than 3 vector species (COD, GAB)-- reverted carrying code back to version that was flexible wrt/ number of vector species
# relic postprocessing bug from last week's testing (code errored out removing a "year" column that no longer exists when transforming popluation into monthly values)


task_log_show("a9890ced02f2509022c105fcbe03440f") 
task_log_show("9fd0590eefef43cee133f8980d18ed01") 
task_log_show("2c9fd671987724f06af3f0512c31790b") 
task_log_show("fd8330deb482af2bbf530e50ead33d15") 

# remaining problem reports:
# AGO new tools
# COD new tools - species bug
# COG new tools
# GAB new tools-- species bug

# identify any jobs which failed to run
iso3cs <- unique(coverage$country_code)
reports <- vimcmalaria::completed_reports('model_country')
problem <- reports |> filter(description == 'gene_drive_fix') |> group_by(iso3c, scenario) |> summarize(n = n()) |>
  group_by(iso3c) |> summarize(n = n()) |> filter(n < 3) |> select(iso3c) |> as.vector() |> unlist()


# pull report metadata
reports <- vimcmalaria::completed_reports('model_country')


compile_mnm_outputs<- function(){
  
  completed<- vimcmalaria::completed_reports('postprocess') |>
    filter(description == 'scale_tx_cov')
  completed<- completed |>
    dplyr::arrange(desc(date_time)) |>
    dplyr::distinct(iso3c, description, .keep_all = TRUE) |>
    dplyr::arrange(iso3c, description)
  
  
  pull_annual_output<- function(index, map){
    
    message(index)
    map<- map[ index,]
    directory_name<- map$directory_name
    iso3c<- map$iso3c
    output<- readRDS(paste0('J:/malaria_no_more/archive/postprocess/', directory_name, '/annual_output.rds')) 
    # M:/Lydia/malaria_no_more/archive/postprocess/
    # J:/malaria_no_more/archive/postprocess/
    
    return(output)
  }
  pull_month_output<- function(index, map){
    
    message(index)
    map<- map[ index,]
    directory_name<- map$directory_name
    iso3c<- map$iso3c
    output<- readRDS(paste0('J:/malaria_no_more/archive/postprocess/', directory_name, '/monthly_output.rds')) 
    return(output)
  }


  outputs_annual<- bind_rows(lapply(c(1:nrow(completed)), pull_annual_output, map = completed))
  outputs_monthly<- bind_rows(lapply(c(1:nrow(completed)), pull_month_output, map = completed))

  outputs<- list('annual' = outputs_annual, 'monthly' = outputs_monthly)

  return(outputs)
}

# pull all of the outputs
outputs<- compile_mnm_outputs()


write.csv(outputs$annual, 'outputs/scale_tx_cov_annual.csv')
write.csv(outputs$monthly, 'outputs/scale_tx_cov_monthly.csv')

saveRDS(outputs$annual, 'outputs/scale_tx_cov_annual.rds')
saveRDS(outputs$monthly, 'outputs/scale_tx_cov_monthly.rds')



pdf('plots/comparative_incidence_plots_scale_tx_cov.pdf', width = 12, height= 10)
for(iso3c in unique(outputs$annual$country)){

  message(iso3c)
  annual<- outputs$annual |> filter(country == iso3c)

    p<- ggplot(data= annual, mapping = aes(x= year, y= clinical * 1000, color= scenario, fill= scenario)) +
    geom_line(lwd= 0.5) +
    facet_wrap(~site_name , scales= 'free') +
    theme_classic() +
    labs(x= 'Year',
         y= 'Clinical incidence per thousand, all-age',
         title= 'All-age clinical incidence over time by scenario',
         subtitle = iso3c)
  
  print(p)

}

dev.off()
