# workflow for malaria no more runs --------------------------------------------

# if needed install packages
remotes::install_github('mrc-ide/orderly2')
remotes::install_github('mrc-ide/vimcmalaria')
remotes::install_github('mrc-ide/scene')
remotes::install_github('mrc-ide/postie')
install.packages('countrycode')

# load packages
library(orderly2)
library(malariasimulation)
library(vimcmalaria)


# initialise orderly2 repository if you have not already
orderly2::orderly_init()



# scenarios to run:
# no-vaccination: coverage of other interventions remain constant from 2022 through 2040, no vaccines
# new_tools: addition of a blood-stage vaccine (IE RH5). In the absence of trial efficacy data, operationalized 
#            such that the blood-stage vaccine has 60% efficacy against  residual cases not protected by R21
# vaccine_scaleup: identical to routine scaleup of R21 vaccine for GAVI runs, with coverage varying by country

scenarios<- c('no-vaccination', 'new_tools', 'vaccine_scaleup')


# to run workflow:
# first set home directory to repository directory

# then run report:
orderly2::orderly_run('model_country',
                      parameters= list(iso3c= 'NGA',
                                       scenario = 'no-vaccination'))

# this will error out unless you have saved coverage and site file inputs in your src/model-country directory--
# contact Lydia for filepaths

