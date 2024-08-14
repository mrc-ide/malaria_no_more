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
  
  # specify vaccine coverage based on forecast  --------------------------------
  site<- vimcmalaria::expand_intervention_coverage(site,
                                      terminal_year = 2040)
  
  
  # to follow vimc coverage levels
  site<- vimcmalaria::update_coverage_values(site,
                                iso3c = iso3c,
                                coverage_data,
                                scenario_name = 'malaria-r3-r4-bluesky')
  
  if(scenario == 'no-vaccination' | scenario == 'worst_case'){
    
    site$interventions$r21_booster_coverage<- 0
    site$interventions$r21_coverage<- 0
  }
  
  # scale treatment up to 80%  from 2022-2034
site$interventions <- site$interventions |>
  mutate(tx_cov = ifelse(year %in% c(2022:2034), NA, tx_cov)) |>
  mutate(tx_cov = ifelse(year > 2034, 0.8, tx_cov)) |>
  scene::linear_interpolate(vars = c("tx_cov"), group_var = names(site$sites))

  
  # check the site has a non-zero EIR
  check_eir(site)
  
  if(site$country %in% c('ETH', 'KEN', 'MOZ', 'MRT', 'SDN', 'UGA', 'NGA', 'GNQ')){
    
    # make blank vector sheet
    vectrow <-  tibble(species = c("arabiensis", "funestus", "gambiae"),
                       prop = c(0, 0, 0),
                       blood_meal_rates = c(0.333, 0.333, 0.333),
                       foraging_time = c(0.69, 0.69, 0.69),
                       Q0 = c(0.71, 0.94, 0.92),
                       phi_bednets = c(0.8, 0.78, 0.85),
                       phi_indoors = c(0.86, 0.87, 0.9),
                       mum = c(0.132, 0.112, 0.132))
    
    arabiensis <- vectrow[1,]
    funestus <- vectrow[2,]
    gambiae <- vectrow[3,]
    
    # check that vector table lists all three species
    if(!("arabiensis" %in% c(site$vectors$species))){
      add_row <- slice_tail(site$vectors, n = 1)
      add_row[, 5:12] <- arabiensis
      site$vectors <- bind_rows(site$vectors, add_row)
    }
    
    if(!("funestus" %in% c(site$vectors$species))){
      add_row <- slice_tail(site$vectors, n = 1)
      add_row[, 5:12] <- funestus
      site$vectors <- bind_rows(site$vectors, add_row)
    }
    
    if(!("gambiae" %in% c(site$vectors$species))){
      add_row <- slice_tail(site$vectors, n = 1)
      add_row[, 5:12] <- gambiae
      site$vectors <- bind_rows(site$vectors, add_row)
    }
    
    site$vectors <- site$vectors |> arrange(species)
    
  }
  
  
    #reorder such that gambiae is the last observation in vectors spreadsheet
if(nrow(site$vectors[species == 'gambiae']) ==1){

  gamb<- site$vectors |> filter(species == 'gambiae')
  vex<- site$vectors |> filter(species != 'gambiae')

  vex<- rbind(vex, gamb)
  
  site$vectors<- vex
}

  if(scenario == 'worst_case'){ # test a case where all interventions are set to zero after 2022

test <- site$interventions[year > 2022, `:=`(
  irs_cov = 0,
  itn_input_dist = 0,
  itn_use = 0,
  smc_cov = 0,
  tx_cov = 0,
  rtss_cov = 0,
  pmc_cov = 0
)]

    site$interventions<- test
    
  }
  # pull parameters for this site ----------------------------------------------
  params <- site::site_parameters(
    interventions = site$interventions,
    demography = site$demography,
    vectors = site$vectors,
    seasonality = site$seasonality,
    eir = site$eir$eir[1],
    burnin = run_params$burnin,
    overrides = list(human_population = 50000)
  )
  

  if(scenario == 'new_tools'){

cc <- get_init_carrying_capacity(params)
n_vectors<- nrow(data.frame(cc))  # number of species in this site

if(is.na(cc["gambiae"]) == FALSE){

# nrow: carrying capacity at timestep
# ncol: species
params<- params |>
  set_carrying_capacity(
    carrying_capacity = matrix(c(rep(1, times= 10*(n_vectors -1)), 0.81, 0.62, 0.43, 0.24, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05), ncol = n_vectors),
    timesteps = (c(31, 32, 33, 34, 35, 36, 37, 38, 39, 40)+ 15)  * 365
  )

}

  }

  
  # # set age groups
  params$clinical_incidence_rendering_min_ages = run_params$min_ages
  params$clinical_incidence_rendering_max_ages = run_params$max_ages
  params$severe_incidence_rendering_min_ages = run_params$min_ages
  params$severe_incidence_rendering_max_ages = run_params$max_ages
  params$age_group_rendering_min_ages = run_params$min_ages
  params$age_group_rendering_max_ages = run_params$max_ages


  if(site_name == 'Oromia'){

    params<- set_equilibrium(params, init_EIR = 20)



  }

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
  
  pop<- site_data$population |>
    filter(name_1 == site$site_name,
           urban_rural == site$ur) |>
    select(year, pop)


  #expand pop into monthly
monthly_pop <- data.table()
pop <- data.table(pop)

for (i in unique(pop$year)) {
  year_dt <- data.table()
  for (month in 1:12) {
    year_dt <- rbind(year_dt, pop[year == i, .(year, month, pop)])
  }
  monthly_pop <- rbind(monthly_pop, year_dt, fill = T)
}
monthly_pop[, month := 1:.N]
monthly_pop<- monthly_pop |>
  select(-year)
  
  
  # add identifying columns
  raw_output<- raw_output |>
    mutate(iso3c = site$iso3c,
            site_name = site$site_name,
            ur = site$ur,
            scenario = site$scenario)
  
  monthly_output <- postie::get_rates(
    raw_output,
    time_divisor = 365/12, # calculate monthly output from model
    baseline_t = 0,
    age_divisor = 365,
    scaler = 0.215,
    treatment_scaler = 0.517,
  )
  
  monthly_output <-
    format_outputs_mnm(
      monthly_output,
      iso3c = site$iso3c,
      site_name = site$site_name,
      ur = site$ur,
      scenario = site$scenario,
      description = 'malaria_no_more_runs')
  
  annual_output <- postie::get_rates(
    raw_output,
    time_divisor = 365, # calculate annual output from model
    baseline_t = 1999,
    age_divisor = 365,
    scaler = 0.215,
    treatment_scaler = 0.517,
  )
  annual_output <-
    format_outputs_mnm(
      annual_output,
      iso3c = site$iso3c,
      site_name = site$site_name,
      ur = site$ur,
      scenario = site$scenario,
      description = 'malaria_no_more_runs')
  
  annual_output<- annual_output |>
    rename(year = t)
  monthly_output<- monthly_output |>
    rename(month = t)
  # merge on population
  annual_output<- merge(annual_output, pop, by = 'year') 
  monthly_output<- merge(monthly_output, monthly_pop, by = 'month')

  
  return(list('monthly' = monthly_output, 'annual' = annual_output))
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
      .data$t,
      .data$age,
      .data$prop_n,
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
  
  if(model_input$scenario %in% c('vaccine_scaleup', 'no-vaccination', 'worst_case')){
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

    # run simulation for remaining period
    second_phase <- malariasimulation:::run_resumable_simulation(
      timesteps = bs_params$timesteps,
      bs_params,
      initial_state=first_phase$state,
      restore_random_state=TRUE)
    
    # bind output from first and second phase together
    model<- dplyr::bind_rows(first_phase$data, second_phase$data)
    
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



#' format output from analyse_site
#' @param output  model output
#' @export
reformat_output<- function(output){
  
  annual_results<- data.table()
  monthly_results<- data.table()
  
  for(item in c(1:length(output))){
    
    subset<- output[[item]]
    
    annual<- subset$annual
    monthly<- subset$monthly
    
    annual_results<- dplyr::bind_rows(annual, annual_results) #  fill =T
    monthly_results<- dplyr::bind_rows(monthly, monthly_results)
    
  }
  
  return(list('annual' = annual_results,
              'monthly' = monthly_results))
  
}

  