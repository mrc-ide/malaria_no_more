submit_country<- function(iso, scenarios, descrip, report_name){
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
    
    for(scen in scenarios){
      message(scen)

      hipercow::task_create_expr(
        orderly2::orderly_run('model_country',
                               parameters= list(iso3c= iso,
                                                scenario = scen,
                                                description = descrip)),
                               resources = hipercow::hipercow_resources(cores = core)
        )
      
  
    }
  } else if (report_name == 'postprocess'){

      orderly2::orderly_run('postprocess', parameters = list(iso3c = iso,
                                                             description= descrip))
  }
}





compile_mnm_outputs<- function(descrip){
  
  completed<- vimcmalaria::completed_reports('postprocess') |>
    filter(description == descrip)
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
