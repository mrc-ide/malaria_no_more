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
# interventions to model:
# vaccines: 80% of R21 coverage + additional 60% efficacy on residual cases in 2029 because of RH5
# treatment: scaleup to 80% treatment coverage in 2034 + rectal artenusate resulting in 20% reduction in under-5 mortality
# gene_drive: modelled as 95% reduction in carrying capacity of anopheles gambiae from 2032-2040
# bednets: scaleup to 60% insecticide-treated bednet usage by 2040 (in admin1 units where this has not already been achieved)
intvns<- c('vaccines', 'txdx', 'genedrive', 'nets')
scenarios <- c(
  intvns, # choose 1
  "vaccines_txdx", "vaccines_genedrive", "vaccines_nets", "txdx_genedrive", "txdx_nets", "genedrive_nets", # choose 2
  "vaccines_txdx_genedrive", "vaccines_txdx_nets", "vaccines_genedrive_nets", "txdx_genedrive_nets", # choose 3
  "vaccines_txdx_genedrive_nets", # choose 4
  "no_intvns"
) # choose none



# to run workflow:
# this will error out unless you have saved coverage and site file inputs in your src/model-country directory--
# contact Lydia for filepaths
hipercow::hipercow_init(driver = 'windows')
hipercow::hipercow_provision()
hipercow::hipercow_environment_create(sources = 'src/model_country/MNM_functions.R')
hipercow::hipercow_configuration()

# first set home directory to repository directory
coverage<- read.csv('src/model_country/bluesky_r21.csv')
vimc_iso3cs<- unique(coverage$country_code)
extra_iso3cs<- c('BWA', 'GNQ', 'ERI', 'GAB', 'GMB', 'NAM', 'RWA', 'SEN', 'ZWE')
iso3cs<- c(vimc_iso3cs, extra_iso3cs)

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
  iso3cs, #  
  submit_country,
  report_name = 'model_country',
  scen = 'itn_change', # c('new_tools', 'vaccine_scaleup', 'worst_case', 'best_case')
  descrip = 'updated_run' # 'scale_tx_cov'
)

hipercow::task_log_watch('c097bd8edd448ef22e2de370cde7a4a1')

# run postprocessing
lapply(
  iso3cs[29:40], # c(vimc_iso3cs, extra_iso3cs)
  submit_country,
  report_name = 'postprocess',
  scen = 'best_case', # 'new_tools', 'vaccine_scaleup', 'worst_case'
  descrip = 'gene_drive_fix'
)

# identify any jobs which failed to run
iso3cs <- unique(coverage$country_code)
reports <- vimcmalaria::completed_reports('model_country')
problem <- reports |> filter(description == 'gene_drive_fix') |> group_by(iso3c, scenario) |> summarize(n = n()) |>
  group_by(iso3c) |> summarize(n = n()) |> filter(n < 3) |> select(iso3c) |> as.vector() |> unlist()


# pull report metadata
reports <- vimcmalaria::completed_reports('model_country')


compile_mnm_outputs<- function(){
  
  completed<- vimcmalaria::completed_reports('postprocess') |>
    filter(description == 'updated_run')
  completed<- completed |>
    dplyr::arrange(desc(date_time)) |>
    dplyr::distinct(iso3c, description, .keep_all = TRUE) |>
    dplyr::arrange(iso3c, description)
  
  
  pull_annual_output<- function(index, map){
    
    message(index)
    map<- map[ index,]
    directory_name<- map$directory_name
    iso3c<- map$iso3c
    output<- readRDS(paste0('./archive/postprocess/', directory_name, '/annual_output.rds')) 
    # M:/Lydia/malaria_no_more/archive/postprocess/
    # J:/malaria_no_more/archive/postprocess/
    
    return(output)
  }
  pull_month_output<- function(index, map){
    
    message(index)
    map<- map[ index,]
    directory_name<- map$directory_name
    iso3c<- map$iso3c
    output<- readRDS(paste0('./archive/postprocess/', directory_name, '/monthly_output.rds')) 
    return(output)
  }
  pull_u5_output<- function(index, map){
    
    message(index)
    map<- map[ index,]
    directory_name<- map$directory_name
    iso3c<- map$iso3c
    output<- readRDS(paste0('./archive/postprocess/', directory_name, '/annual_children.rds')) 
    return(output)
  }


  outputs_annual<- bind_rows(lapply(c(1:nrow(completed)), pull_annual_output, map = completed))
  outputs_monthly<- bind_rows(lapply(c(1:nrow(completed)), pull_month_output, map = completed))
  outputs_u5<- bind_rows(lapply(c(1:nrow(completed)), pull_u5_output, map = completed))


  outputs<- list('annual' = outputs_annual, 'monthly' = outputs_monthly, 'u5' = outputs_u5)

  return(outputs)
}

# pull all of the outputs
outputs<- compile_mnm_outputs()


write.csv(outputs$annual, 'outputs/updated_run_annual.csv')
write.csv(outputs$u5, 'outputs/updated_run_annual_u5.csv')

write.csv(outputs$monthly, 'outputs/updated_run_monthly.csv')

saveRDS(outputs$annual, 'outputs/updated_run_annual.rds')
saveRDS(outputs$u5, 'outputs/updated_run_annual_u5.rds')

saveRDS(outputs$monthly, 'outputs/updated_run_monthly.rds')



pdf('plots/diagnostic_plots_updated_run.pdf', width = 12, height= 10)
for(iso3c in unique(outputs$annual$country)){

  message(iso3c)
  annual<- outputs$annual |> filter(country == iso3c) 

    p<- ggplot(data= annual, mapping = aes(x= year, y= clinical * 1000, color= scenario, fill= scenario)) +
    geom_line(lwd= 0.5) +
    facet_wrap(~site_name + urban_rural , scales= 'free') +
    theme_classic() +
    labs(x= 'Year',
         y= 'Clinical incidence per thousand, all-age',
         title= 'All-age clinical incidence over time by scenario',
         subtitle = iso3c)
  
    p2<- ggplot(data= annual, mapping = aes(x= year, y= mortality * 1000, color= scenario, fill= scenario)) +
          geom_line(lwd= 0.5) +
          facet_wrap(~site_name + urban_rural , scales= 'free') +
          theme_classic() +
          labs(x= 'Year',
               y= 'Mortality per thousand, all-age',
               title= 'All-age mortality over time by scenario',
               subtitle = iso3c)
  
  print(p)
  print(p2)

}

dev.off()




pull_itn_coverage<- function(iso){
  if (iso %in% vimc_iso3cs){

    site_data <- readRDS(paste0('src/model_country/site_files/', iso, '_new_EIR.rds'))

  } else if (iso %in% extra_iso3cs){

    site_data <- readRDS(paste0('src/model_country/original_site_files/', iso, '.RDS'))
  }  
  if (iso == 'UGA'){
    site_data <- readRDS(paste0('src/model_country/original_site_files/', iso, '.RDS'))
  }

itns<- site_data$interventions |>
  filter(year == 2022) |>
  select(name_1, urban_rural, itn_use, itn_input_dist)
  
  return(itns)

}
itn_covs<- rbindlist(lapply(iso3cs, pull_itn_coverage))
itn_covs<- data.table(itn_covs)


boxplot(itn_covs$itn_use) +
  title('ITN coverage by admin 1 unit, 2022')


itn_nga<- pull_itn_coverage('NGA')



nrow(itn_covs[itn_use > .8])
