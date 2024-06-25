# malaria_no_more functions
# pulled from vimcmalaria package but slightly modified


parameterise_mnm<- function(site_name,
                             ur,
                             iso3c,
                             site_data,
                             coverage_data,
                             scenario){
  message('parameterizing')
  
  # extract site
  site <- vimcmalaria::extract_site(site_file = site_data,
                       site_name = site_name,
                       ur = ur)
  
  
  run_params<- vimcmalaria::pull_age_groups_time_horizon(quick_run= FALSE)
  
  # specify vaccine coverage based on forecast  ----------------------------------
  site<- vimcmalaria::expand_intervention_coverage(site,
                                      terminal_year = 2040)
  
  site<- vimcmalaria::update_coverage_values(site,
                                iso3c = iso3c,
                                coverage_data,
                                scenario_name = 'malaria-r3-r4-default')
  
  if(scenario == 'no-vaccination'){
    
    site$interventions$r21_booster_coverage<- 0
    site$interventions$r21_coverage<- 0
  }
  
  
  # check the site has a non-zero EIR
  check_eir(site)
  
  # pull parameters for this site ------------------------------------------------
  params <- site::site_parameters(
    interventions = site$interventions,
    demography = site$demography,
    vectors = site$vectors,
    seasonality = site$seasonality,
    eir = site$eir$eir[1],
    burnin = run_params$burnin,
    overrides = list(human_population = run_params$pop_val)
  )
  
  
  # set up a theoretical transmission-blocking vaccine (which averted additional 60% of residual cases after pre-erythrocytic vaccine)
  if(scenario == 'new_tools'){
    
  bs_efficacy<-  (1- params$pev_profiles[[1]]$vmax) *.6
  params$pev_profiles[[1]]$vmax<-  params$pev_profiles[[1]]$vmax + bs_efficacy
  
  bs_efficacy_booster<-  (1- params$pev_profiles[[2]]$vmax) *.6
  params$pev_profiles[[2]]$vmax<-  params$pev_profiles[[2]]$vmax + bs_efficacy
    
  }
  
  
  # set age groups
  params$clinical_incidence_rendering_min_ages = run_params$min_ages
  params$clinical_incidence_rendering_max_ages = run_params$max_ages
  params$severe_incidence_rendering_min_ages = run_params$min_ages
  params$severe_incidence_rendering_max_ages = run_params$max_ages
  params$age_group_rendering_min_ages = run_params$min_ages
  params$age_group_rendering_max_ages = run_params$max_ages
  

  params$pev<- TRUE
  
  inputs <- list(
    'param_list' = params,
    'site_name' = site_name,
    'ur' = ur,
    'iso' = iso3c,
    'scenario' = scenario,
    'pop_val' = run_params$pop_val,
    'burnin' =  run_params$burnin
  )
  
  
  return(inputs)
  
}



analyse_mnm<- function(site,
                       site_data,
                       coverage_data,
                       scenario){
  
  
  model_input<- parameterise_mnm(site_name = site$site_name,
                            ur = site$ur, 
                            iso3c = site$iso3c,
                            site_data = site_data,
                            coverage_data,
                            scenario)
  
  
  model<- vimcmalaria::run_model(model_input)
  
  # calculate rates
  raw_output<- drop_burnin(model, burnin= unique(model$burnin)* 365)
  
  # add identifying columns
  raw_output<- raw_output |>
    mutate( iso3c = iso3c,
            site_name = site_name,
            ur = ur,
            scenario = scenario)
  
  
  output <- postie::get_rates(
    raw_output,
    time_divisor = 365,
    baseline_t = 1999,
    age_divisor = 365,
    scaler = 0.215,
    treatment_scaler = 0.517,
  )
  
  return(output)
}


#' make an analysis map of input parameters for vaccine modelling run
#' @param site_df   analysis map with input parameters
#' @param site_data site data
#' @param test      boolean-- if true, only run analysis for two test sites. Good for quick tests of code functionality
#' @returns analysis map to be used as an input for analyse_site
#' @export
make_analysis_map<- function(site_df,
                             site_data,
                             test){
  
  
  site_data$prevalence<- site_data$prevalence |>
    dplyr::filter(year == 2019) |>
    mutate(run_model = ifelse(pfpr > 0.10, TRUE, FALSE))
  
  # make exceptions for Madagascar, Ethiopia, and Sudan
  # hardcode for time's sake but operationalize later
  if(unique(site_df$country) == 'Madagascar'){
    
    site_data$prevalence <- site_data$prevalence |>
      mutate(run_model = ifelse(name_1 == 'Toliary', TRUE, run_model))
  }
  
  if(unique(site_df$country) == 'Ethiopia'){
    
    site_data$prevalence <- site_data$prevalence |>
      mutate(run_model = ifelse(name_1 %like% 'Gambela', TRUE, run_model))
    
    
  }
  
  if(unique(site_df$country) == 'Sudan'){
    
    
    site_data$prevalence <- site_data$prevalence |>
      mutate(run_model = ifelse(name_1 == 'South Darfur', TRUE, run_model)) |>
      mutate(run_model = ifelse(name_1 == 'West Kurdufan', TRUE, run_model))
    
  }
  prevalence<- site_data$prevalence |>
    select(name_1, urban_rural, iso3c, run_model) |>
    rename(site_name = name_1,
           ur= urban_rural)
  
  site_df<- site_df |>
    rename(site_name = name_1,
           ur= urban_rural)
  
  site_info<- merge(prevalence, site_df, by = c('site_name', 'ur', 'iso3c'))
  
  if(nrow(prevalence) < nrow(site_info)){
    stop('dropped admin units, debug')
  }
  
  if(scenario == 'no-vaccination'){
    
    site_info<- site_info |>
      mutate(run_model = TRUE)
    
  }
  
  site_info<- site_info |>
    dplyr::filter(run_model == TRUE)
  site_info<- site_info |>
    mutate(scenario = {{scenario}})
  
  
  Encoding(site_info$site_name) <- "UTF-8"
  site_info$site_name<- iconv(site_info$site_name, from="UTF-8", to="ASCII//TRANSLIT")
  
  if (test == TRUE) {
    
    site_info<- site_info[1:2,]
    
  }
  sites<- purrr::map(.x = c(1:nrow(site_info)), .f= ~ site_info[.x,])
  
  return(sites)
}



  
  