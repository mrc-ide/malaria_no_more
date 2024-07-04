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
                                scenario_name = 'malaria-r3-r4-bluesky')
  
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
    overrides = list(human_population = 50000)
  )
  
  # if(scenario == 'new_tools'){
  # 
  #   
  # }

  
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
  
  
  model<- run_mnm_model(model_input)
  
  # calculate rates
  raw_output<- drop_burnin(model, burnin= unique(model$burnin)* 365)
  
  # add identifying columns
  raw_output<- raw_output |>
    mutate(iso3c = site$iso3c,
            site_name = site$site_name,
            ur = site$ur,
            scenario = site$scenario)
  
  
  output <- postie::get_rates(
    raw_output,
    time_divisor = 365/12, # calculate monthly output from model
    baseline_t = 0,
    age_divisor = 365,
    scaler = 0.215,
    treatment_scaler = 0.517,
  )
  
  output<- output |>
    rename(month = t)

  output <-
    format_outputs_mnm(
      output,
      iso3c = site$iso3c,
      site_name = site$site_name,
      ur = site$ur,
      scenario = site$scenario,
      description = 'malaria_no_more_runs')
  

  
  return(output)
}


#' make an analysis map of input parameters for vaccine modelling run
#' @param site_df   analysis map with input parameters
#' @param site_data site data
#' @param test      boolean-- if true, only run analysis for two test sites. Good for quick tests of code functionality
#' @returns analysis map to be used as an input for analyse_site
#' @export
make_mnm_analysis_map<- function(site_df,
                                 test){
  
  site_df<- site_df |>
    rename(site_name = name_1,
           ur= urban_rural)
  
  
  site_info<- site_df |>
    mutate(scenario = {{scenario}}) |>
    mutate(run_model= TRUE)
  
  
  Encoding(site_info$site_name) <- "UTF-8"
  site_info$site_name<- iconv(site_info$site_name, from="UTF-8", to="ASCII//TRANSLIT")
  
  if (test == TRUE) {
    
    site_info<- site_info[1:2,]
    
  }
  sites<- purrr::map(.x = c(1:nrow(site_info)), .f= ~ site_info[.x,])
  
  return(sites)
}

#' format outputs for submission
#' @param dt  postprocessed output
#' @param site_name name of site
#' @param ur urbanicity
#' @param iso3c country code
#' @param scenario vaccine scenar.io
#' @param description reason for model run
#' @export
format_outputs_mnm<- function(dt, iso3c, site_name, ur, scenario,  description){
  dt <- dt |>
    mutate(
      disease = 'Malaria',
      country = iso3c,
      country_name = countrycode::countrycode(
        sourcevar = iso3c,
        origin = 'iso3c',
        destination = 'country.name'),
      site_name = site_name,
      urban_rural = ur,
      scenario = scenario,
      description = description,
    ) |>
    rename(age = .data$age_lower) |>
    select(
      .data$disease,
      .data$month,
      .data$age,
      .data$country,
      .data$country_name,
      .data$site_name,
      .data$urban_rural,
      .data$scenario,
      description,
      .data$clinical,
      .data$mortality,
    ) |>
    mutate(
      mortality = if_else(is.na(mortality), 0, mortality),
      clinical = if_else(is.na(clinical), 0, clinical),
    )
  
  return(dt)
}

  

#' Run model for site of interest
#' @param   model_input      list with input parameters and identifying info
#' @returns model output
#' @export
run_mnm_model<- function(model_input){
  message('running the model')
  
  params <- model_input$param_list
  params$progress_bar <- TRUE
  
  set.seed(56)
  
  if(model_input$scenario %in% c('vaccine_scaleup', 'no-vaccination')){
    model <- retry::retry(
      malariasimulation::run_simulation(timesteps = params$timesteps,
                                        parameters = params),
      max_tries = 5,
      when = 'error reading from connection|embedded nul|unknown type',
      interval = 3
    )
    
  }else if(model_input$scenario == 'new_tools'){
    
    first_phase <- retry::retry(
      malariasimulation:::run_resumable_simulation(timesteps = 365*(29 +15), # including burn-in period of 15 years
                                        parameters = params),
      max_tries = 5,
      when = 'error reading from connection|embedded nul|unknown type',
      interval = 3
    )
    
    # set up a theoretical blood-stage vaccine (which averted additional 60% of residual cases after pre-erythrocytic vaccine)
    bs_params<- copy(params)
    
    # set efficacy for first 3 doses
    bs_efficacy<-  (1- params$pev_profiles[[1]]$vmax) *.6
    bs_params$pev_profiles[[1]]$vmax<-  bs_params$pev_profiles[[1]]$vmax + bs_efficacy
    
    # set efficacy for booster dose
    bs_efficacy_booster<-  (1- bs_params$pev_profiles[[2]]$vmax) *.6
    bs_params$pev_profiles[[2]]$vmax<-  bs_params$pev_profiles[[2]]$vmax + bs_efficacy
    
    # 
    # # change carrying capacity for future scenario (assuming some kind of gene drive that reduces mosquito populations massively)
    cc <- get_init_carrying_capacity(bs_params)
    n_vectors<- nrow(data.frame(cc))  # number of species in this site

    
    bs_params<- bs_params |>
      set_carrying_capacity(
        carrying_capacity = cc *  matrix(c(0.5), ncol = n_vectors),
        timesteps = 31 * 365
      )  
    # just for a test set carrying capacity to half of its current value


    # run simulation for remaining period
    second_phase <- malariasimulation:::run_resumable_simulation(
      timesteps = bs_params$timesteps,
      bs_params,
      initial_state=first_phase$state,
      restore_random_state=TRUE)
    
    # bind output from first and second phase together
    model<- rbind(first_phase$data, second_phase$data)
    
    }
  
  
  # add identifying information to output
  model <- model |>
    mutate(site_name = model_input$site_name,
           urban_rural = model_input$ur,
           iso = model_input$iso3c,
           description = model_input$description,
           scenario = model_input$scenario,
           gfa = model_input$gfa,
           parameter_draw = model_input$parameter_draw,
           population = model_input$pop_val,
           burnin = model_input$burnin)
  
  # save model runs somewhere
  message('saving the model')
  return(model)
}

  