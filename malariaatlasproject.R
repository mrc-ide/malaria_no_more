# Download shapefiles and rasters from Malaria Atlas Project
source("./data_and_libraries.R")


# SHAPEFILES----

# list shapefile versions
listShpVersions()

# list shapefiles available for a version
listShp(version = "202403")

# get shapefiles for levels 0 and 1
world_0 <- getShp(ISO = "all", 
                 admin_level = "admin0", 
                 version = "202403")

world_1 <- getShp(ISO = "all", 
                  admin_level = "admin1", 
                  version = "202403")

# look at crs
crs(world_0); crs(world_1)

# plot
ggplot() + 
  geom_sf(data = world_1, color = "darkgrey", fill = NA) + 
  geom_sf(data = world_0, color = "black", fill = NA) + 
  theme_bw()

# save
saveRDS(world_0, "./files/world_0.rds")
saveRDS(world_1, "./files/world_1.rds")
saveRDS(world_0, paste0(HPCpath, "world_0.rds"))
saveRDS(world_1, paste0(HPCpath, "world_1.rds"))

# read-in
world_0 <- readRDS("./files/world_0.rds")
world_1 <- readRDS("./files/world_1.rds")


# INCIDENCE ----
# list rasters
malariaAtlas::listRaster()
# Malaria__202406_Global_Pf_Mortality_Rate 
# Malaria__202406_Global_Pf_Incidence_Rate 
# Malaria__202406_Global_Pv_Incidence_Rate 

# function malariaAtlas::getRaster is not working for these surfaces - download
# worldpop estimates: https://hub.worldpop.org/geodata/listing?id=64

# read in test raster
rast <- raster::raster(paste0(HPCpath, "MAP rasters/2024_GBD2023_Global_Pf_Incidence_Count_2000.tif"))

crs(x) # WGS 1984

# HPC set-up
setwd(HPCpath)
hipercow_init()
hipercow_init(driver = "windows")

# set source function
hipercow_environment_create(sources = "function_extractrast.R")

# install packages from pkgdepends.txt
hipercow_provision()

# set up your job 
value <- c("Pf_Incidence_Count", 
           "Pv_Incidence_Count", 
           "Pf_Mortality_Count", 
           "population")

year <- seq(2000, 2022, 1)

scenarios <- crossing(value, year) |>
  filter(!(value == "population" & year > 2020))

# submit jobs
id <- task_create_expr(extractrast("Pf_Incidence_Count", 2000))
task_status(id)
task_log_show(id)

# in bulk
bundle <- task_create_bulk_expr(extractrast(value, year), scenarios)
hipercow_bundle_status(bundle)
# hipercow_bundle_cancel(bundle)



# function to read in file
read_file <- function(year){
  
  Pf_IC <- readRDS(paste0(HPCpath, "extracts/Pf_Incidence_Count_", year, ".rds"))
  Pv_IC <- readRDS(paste0(HPCpath, "extracts/Pv_Incidence_Count_", year, ".rds"))
  Pf_MC <- readRDS(paste0(HPCpath, "extracts/Pf_Mortality_Count_", year, ".rds"))
  
  if(year <= 2020){
    population <- readRDS(paste0(HPCpath, "extracts/population_", year, ".rds"))
  }
  
  if(year > 2020){ # for years 2021 and 2022, read in 2020
    population <- readRDS(paste0(HPCpath, "extracts/population_2020.rds"))
  }
  
  # create tibble and remove nested structure
  dat <- tibble(Pf_IC = Pf_IC$sum, Pv_IC = Pv_IC$sum, 
         Pf_MC = Pf_MC$sum, population = population$sum) 
  
  # calculate incidence and mortality
  dat <- dat |> rowwise() |> 
    mutate(Pf_IC = ifelse(Pf_IC < 1, 0, Pf_IC), # incidence must be at least 1 person
           Pv_IC = ifelse(Pv_IC < 1, 0, Pv_IC), # incidence must be at least 1 person
           Pf_MC = ifelse(Pf_MC < 1, 0, Pf_MC), # incidence must be at least 1 person
           incidence = (Pf_IC + Pv_IC) / population * 1000,
           mortality = (Pf_MC) / population * 100000) |>
    dplyr::select(incidence, mortality)
  
  colnames(dat) <- c(paste0("incidence_", year), paste0("mortality_", year))
  
  return(dat)
  
}

# read all files into a dataframe
dat_complete <- map_dfc(seq(2000, 2022, 1), read_file)

summary(dat_complete)

dat_complete_clean <- dat_complete |>
  mutate(across(starts_with("incidence"), ~ case_when(. > 700 ~ 700,
                                                      . == 0 ~ NA,
                                                      . < 0.01 ~ 0.01,
                                                      TRUE ~ .)),
         across(starts_with("mortality"), ~ case_when(. > 1000 ~ 1000,
                                                      . == 0 ~ NA,
                                                      . < 0.01 ~ 0.01,
                                                      TRUE ~ .)))
summary(dat_complete_clean)


# bind to sf object
admin1s <- bind_cols(world_1, dat_complete_clean)






# function to create incidence maps
plotinc <- function(var){
  
  p <- ggplot(data = admin1s) +
        geom_sf(aes_string(fill = {{var}}), color = NA) +
        labs(title = str_extract(var, "\\d+"), 
             fill = str_extract(var, "[a-zA-Z]+")) + 
        scale_fill_gradientn(
          colors = scales::viridis_pal()(10),
          values = scales::rescale(c(0.1, 175, 350, 525, 700), to = c(0, 1)),
          breaks = c(0.1, 175, 350, 525, 700),
          labels = c("<0.1", "175", "350", "525", ">700"),
          limits = c(0.1, 700)) +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              axis.ticks = element_blank(), 
              axis.text.x = element_blank(), 
              axis.text.y = element_blank(),
              panel.border = element_blank(),
              plot.background = element_blank(),
              panel.background = element_blank())
  
  # save plot as PNG 
  ggsave(filename = paste0(HPCpath, "plots/plot_", var, ".png"), plot = p, width = 6, height = 4)
  
  print(paste0("Ran ", var))

}


# run through function
map(names(admin1s)[grep("incidence", names(admin1s))], plotinc)

# list saved image filenames
image_files <- list.files(path = paste0(HPCpath, "plots"), pattern = "plot_incidence_\\d+.png", full.names = TRUE)

# read images
images <- magick::image_read(image_files)

# combine images into GIF
output_gif <- magick::image_animate(images, fps = 1)

# save the animated GIF
image_write(output_gif, paste0(HPCpath, "incidence_animation.gif"))


# function to create mortality plots
plotmort <- function(var){
  
  p <- ggplot(data = admin1s) +
    geom_sf(aes_string(fill = {{var}}), color = NA) +
    labs(title = str_extract(var, "\\d+"), 
         fill = str_extract(var, "[a-zA-Z]+")) + 
    scale_fill_gradientn(
      colors = scales::viridis_pal()(10),
      # values = scales::rescale(c(0.1, 1, 10, 100, 1000), to = c(0, 1)),
      breaks = c(0.1, 1, 10, 100, 1000),
      labels = c("<0.1", "1", "10", "100", ">1,000"),
      trans = "log10",
      limits = c(0.1, 1000)) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          axis.ticks = element_blank(), 
          axis.text.x = element_blank(), 
          axis.text.y = element_blank(),
          panel.border = element_blank(),
          plot.background = element_blank(),
          panel.background = element_blank())
  
  # save plot as PNG 
  ggsave(filename = paste0(HPCpath, "plots/plot_", var, ".png"), plot = p, width = 6, height = 4)
  
  print(paste0("Ran ", var))
  
}

# run through function
map(names(admin1s)[grep("mortality", names(admin1s))], plotmort)

# list saved image filenames
image_files <- list.files(path = paste0(HPCpath, "plots"), pattern = "plot_mortality_\\d+.png", full.names = TRUE)

# read images
images <- magick::image_read(image_files)

# combine images into GIF
output_gif <- magick::image_animate(images, fps = 1)

# save the animated GIF
image_write(output_gif, paste0(HPCpath, "mortality_animation.gif"))

