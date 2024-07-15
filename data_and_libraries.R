# Libraries --------------------------------------------------------------------

# Packages ----
library(tidyverse)
library(data.table)
library(lubridate)
library(geodata)    # GADM
library(sf)
library(raster)
library(terra)
library(malariaAtlas) 
library(umbrella)   # remotes::install_github('mrc-ide/umbrella')
library(beepr)

# HPC
library(hipercow) # for computer cluster: https://mrc-ide.github.io/hipercow/articles/hipercow.html

# Plotting
library(patchwork)
library(MetBrewer) # devtools::install_github('BlakeRMills/MetBrewer')
library(magick)

# paths
HPCpath <- "M:/Hillary/MNMUK/" # HPC
path <- "C:/Users/htopazia/OneDrive - Imperial College London/Github/malaria_no_more/" # local
