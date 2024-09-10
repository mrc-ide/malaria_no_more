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

#source helper functions
source('workflow_functions.R')

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


# run model country
lapply(
  'AGO',  
  submit_country,
  report_name = 'model_country',
  scenarios = {scenarios},
  descrip = 'exhibit_test_runs' 
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


# pull all of the outputs
outputs<- compile_mnm_outputs(descrip = 'updated_run')


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
