

library(malariasimulation)


model_input<- readRDS('test_model_input.rds')
params<- model_input$param_list
params$progress_bar<- TRUE


# this model runs-- no carrying capacity changes
control_model <-malariasimulation::run_simulation(timesteps = params$timesteps,
                                          parameters = params)

  cc <- get_init_carrying_capacity(params)
  n_vectors<- nrow(data.frame(cc))  # number of species in this site

  cc_params<- params |>
    set_carrying_capacity(
      carrying_capacity = matrix(c(0.5), ncol = n_vectors),
      timesteps = (31 + 15)  * 365
    )



# this model hangs indefinitely at 83%
  cc_model <-malariasimulation::run_simulation(timesteps = cc_params$timesteps,
                                                    parameters = cc_params)
  