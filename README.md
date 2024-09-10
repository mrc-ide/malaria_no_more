# Malaria No More
Code used to produce data for Malaria No More artwork.


# Quick start

###  Download site file and vaccine coverage inputs
Contact Lydia for these filepaths. These files should be saved in `src/model_country/`.

###  Run workflow.R
This script contains the `orderly2::orderly_run()` function, which is used to run the model_country orderly report. The parameters for this report are as follows:

- *iso3c*: country code for country to model
- *scenario*: scenario to run for Malaria No More runs.

Options:
  * *vaccine_scaleup*: includes vaccine scale-up of 80% coverage for R21 vaccine starting in 2023, where the vaccine is administered to all admin-1 units in the country.
  * *no-vaccination*: no vaccine scale-up, interventions remain constant through 2040.
  * *new_tools*: incorporates a blood-stage vaccine (IE RH5) in 2029, which we presume has 60% efficacy against cases not prevented by R21 vaccination. Operationalised as an increase in vaccine efficacy with no other changes to R21 parameters.
    Also reduces carrying capacity by 50% in 2029, as a crude attempt to model the introduction of gene drive technologies that may reduce mosquito populations in malaria-endemic regions.


Models are run from 2000-2040 for all malaria-endemic countries in Sub-Saharan Africa at the admin-1 level. The modelled population size is 50,000, with a burn-in period of 15 years.

Outputs will be saved to the `archive\model_country` directory, with site-level incidence and mortality rates saved to `outputs.rds` for every country/scenario combination.


## Directory

```
.
├── ethiopia                             # Ethiopia test case
|   ├── ethiopia_recalibration.R         # Ethiopia EIR recalibration
├── plots                                # Plots of model output
├── src                                  # Coding scripts
|   ├── model_country                    # Country runs
|       ├── MNM_functions.R              # Model run functions
|       ├── model_country.R              # Model run script
|   ├── postprocess                      # Post-processing
|       ├── compare_ethiopia_versions.R  # ETH comparison
|       ├── postprocess.R                # Post-processing functions
├── country_shapefiles.R                 # GADM shapefiles
├── data_and_libraries.R                 # R packages and paths to load
├── ethiopia_recalibration.R             # Ethiopia EIR recalibration
├── export_shapefiles.R                  # Exporting GADM and MAP .rds as .shp
├── ITN_vs_PfPR_plot.R                   # Site ITN use vs. PfPR
├── malariaatlasproject.R                # Shapefiles of MAP CI data Pf & Pv
├── model_outputs.R                      # Key script for ICL model results
├── seasonality curves.R                 # Creating seasonality weights for each site
├── site_files.R                         # Calculating lives saved from site file data
├── test_carrying_capacity.R             # Testing the cc function in malariasim
├── WHO_elimination.R                    # WHO decade of elimination maps
├── WMR cases and deaths.R               # Test script for saving shp files
├── WMR lives saved.R                    # Cumulative lives saved: WMR 2023
├── workflow.R                           # Key script for running models
├
├── .gitignore                           # gitignore file
├── malaria_no_more.Rproj                # R.Studio project file
└── README.md                            # Project overview

```
