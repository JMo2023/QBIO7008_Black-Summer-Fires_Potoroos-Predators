#Author: Justin Moore

#Project: Fire, Potoroos and Predators

#1. Data Processing script: Create input matrices for unmarkedFrameOccuMulti object for model fitting ####

## Libraries ####

require(ggplot2)
require(dplyr)
require(tidyr)
require(patchwork)
require(readxl)
require(sf)
require(ggspatial)
require(ozmaps)
require(tmap)
require(raster)
require(RNetCDF)
require(terra)

## Directory setup ####

directory <- "Input_Data"

#### directory where data is saved once complete ###
data_out <- "transformed_data"

NDVI_path <- "Input_Data/NDVI_data"

#Setup directory


deploy <- read_excel(paste(directory, "deployments_29022004_gipps.xlsx", sep = "/"))

deploy$duration <- (deploy$end_date - deploy$start_date)/7


#load camera trap (CT) dataset
cam_data <- read_excel(paste(directory, "images_29022004_gipps.xlsx", sep = "/"))

#extract date from timestamp
cam_data$date <- as.Date(cam_data$timestamp)

#extract time from timestamp
cam_data$time <-  format(cam_data$timestamp, "%H:%M:%S")

#Calculate minute since midnight
time_components <- strsplit(cam_data$time, ":")
hours <- as.integer(sapply(time_components, "[[", 1))
minutes <- as.integer(sapply(time_components, "[[", 2))
seconds <- as.integer(sapply(time_components, "[[", 3))

cam_data$min_since_midnight <- hours * 60 + minutes + seconds / 60

#filter deployment ID for site names
cam_data <- cam_data %>%
  separate(deployment_id, into = "site", sep = "_", remove = FALSE)

## 1.1 Detection level covariates ####

#Capture_times for averaging
capture_time_matrix <- function(data, column, genus_search_term) {
  
  # Set cat column based on genus search term
  data$sp <- ifelse(data[, column] == genus_search_term, data$min_since_midnight, NA)
  
  # Extract unique dates and sites
  dates <- as.character(sort(unique(data$date)))
  sites <- unique(data$site)
  
  # Create an empty matrix with dimensions corresponding to the length of dates and sites
  mat <- matrix(NA, nrow = length(sites), ncol = length(dates))
  
  # Assign dates to column names and sites to row names
  rownames(mat) <- sites
  colnames(mat) <- dates
  
  # Fill the matrix with data from the dataframe$is_..., matching date and site

  for (i in 1:nrow(data)) {
    date_index <- which(colnames(mat) == as.character(data$date[i]))
    site_index <- which(rownames(mat) == data$site[i])
    
    if (!is.na(data$sp[i])) {
      if (!is.na(mat[site_index, date_index])) {
        mat[site_index, date_index] <- paste(mat[site_index, date_index], data$sp[i], sep = ",")
      } else {
        mat[site_index, date_index] <- as.character(data$sp[i])
      }
    }
  }

   return(mat)
  
}
# Write time mattices
cat_time_matrix <- capture_time_matrix(cam_data, "genus", "Felis")
write.csv(cat_time_matrix, paste(data_out, "daily_cat_time_matrix.csv", sep = "/"))

dingo_time_matrix <- capture_time_matrix(cam_data, "genus", "Canis")
write.csv(dingo_time_matrix, paste(data_out, "daily_dingo_time_matrix.csv", sep = "/"))

fox_time_matrix <- capture_time_matrix(cam_data, "genus", "Vulpes")
write.csv(fox_time_matrix, paste(data_out, "daily_fox_time_matrix.csv", sep = "/"))

potorous_time_matrix <- capture_time_matrix(cam_data,"genus", "Potorous")
write.csv(potorous_time_matrix, paste(data_out, "daily_potorous_time_matrix.csv", sep = "/"))

p.tridactylus_time_matrix <- capture_time_matrix(cam_data,"species", "longipes")
write.csv(p.tridactylus_time_matrix, paste(data_out, "daily_p_tridactylus_time_matrix.csv", sep = "/"))

LN_Bandicoot_time_matrix <- capture_time_matrix(cam_data,"species", "nasuta")
write.csv(LN_Bandicoot_time_matrix, paste(data_out, "daily_LN_Bandicoot_time_matrix.csv", sep = "/"))

## Data outputs ####

#### directory where data is saved once complete ###
data_out <- "transformed_data"


### Daily Bait type matrix ####

dates <- as.character(sort(unique(cam_data$date)))
sites <- unique(cam_data$site)

bait_mat <- matrix(NA, nrow = length(sites), ncol = length(dates))

# Assign dates to column names and sites to row names
rownames(bait_mat) <- sites
colnames(bait_mat) <- dates

for (i in sites) {
  
  site_bait <- deploy[deploy$placename == i,]
  
  for(j in 1:nrow(site_bait)){
  
  rows <- seq(site_bait$start_date[j], 
      site_bait$end_date[j], by = "day")
  
  rows <- gsub(" UTC", "",rows)
  
  bait_mat[i,rows] <- site_bait$bait_description[j]
  }
}
### Plot heatmap of daily bait type ####
bait_mat <- as.matrix(gsub("Tuna oil", "Tuna Oil", bait_mat))
write.csv(bait_mat, paste(data_out, "daily_bait-type-chr_matrix.csv", sep = "/"))

bait_num <- as.factor(bait_mat)
write.csv(bait_num, paste(data_out, "daily_bait-type-fctr_matrix.csv", sep = "/"))

### Plot heatmap of daily bait type ####

### Define custom color palette and labels ###
my_colors <- c("blue", "green", "yellow")
my_labels <- c("Chicken", "Synthetic Fermented Egg", "Tuna Oil", "NA")

# Create a data frame for the heatmap
df <- expand.grid(
  placename = 1:100,
  date = colnames(bait_mat)
)
df$value <- as.vector(bait_mat)


# Figure 1: Plot heatmap using ggplot2 for methods
ggplot(df, aes(x = date, y = placename, fill = factor(value))) +
  geom_tile(color = "NA") +
  scale_fill_manual(values = my_colors, labels = my_labels) +
  labs(fill = "Bait Type", y = "Site number", x="Deployment dates") +  # Adding label for y-axis
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
  scale_x_discrete(breaks = unique(df$date)[c(TRUE, rep(FALSE, 28))]) +
  scale_y_continuous(breaks = seq(1, 100, by = 9))  # Setting y-axis breaks from 1 to 100

ggsave(paste(data_out, "bait_heatmap_daily.png", sep="/"), width = 10, height = 8, units = "in")

## 1.2 Species detection / non-detection data ####


#bait for each date

cam_data$bait <- NA

# Iterate over each row in cam_data
for (i in 1:nrow(cam_data)) {
  # Find all matching rows in deploy where cam_data$site matches deploy$placename
  matching_rows <- deploy[deploy$placename == cam_data$site[i], ]
  
  # Check if there are any matching rows
  if (nrow(matching_rows) > 0) {
    # Iterate over each matching row
    for (j in 1:nrow(matching_rows)) {
      # Check if cam_data$date is between deploy$start_date and deploy$end_date
      if (as.Date(cam_data$date[i]) >= as.Date(matching_rows$start_date[j]) &&
          as.Date(cam_data$date[i]) <= as.Date(matching_rows$end_date[j])) {
        # If the condition is met, assign deploy$bait_description to cam_data$bait
        cam_data$bait[i] <- matching_rows$bait_description[j]
        # Break the loop since we found a matching row
        break
      }
    }
  }
}

cam_data$bait <- gsub("Tuna Oil", "Tuna oil", cam_data$bait)

table(is.na(cam_data$bait))


dates <- as.character(sort(unique(cam_data$date)))
sites <- unique(cam_data$site)

#Function to create presence absence values for site and timestep based on selected species

species_matrix <- function(data, column, genus_search_term) {
  
  # Set cat column based on genus search term
  data$sp <- ifelse(data[, column] == genus_search_term, 1, 0)
  
  # Set cat to 0 if is_cat is NA
  data$sp[is.na(data$sp)] <- 0
  
  # Extract unique dates and sites
  dates <- as.character(sort(unique(data$date)))
  sites <- unique(data$site)
  
  # Create an empty matrix with dimensions corresponding to the length of dates and sites
  mat <- matrix(0, nrow = length(sites), ncol = length(dates))
  
  # Assign dates to column names and sites to row names
  rownames(mat) <- sites
  colnames(mat) <- dates
  
  # Fill the matrix with data from the dataframe$is_..., matching date and site
  for (i in 1:nrow(data)) {
    date_index <- which(colnames(mat) == as.character(data$date[i]))
    site_index <- which(rownames(mat) == data$site[i])
    
    mat[site_index, date_index] <- data$sp[i]
    
  }
  
  
  mat[is.na(bait_mat)] <- NA
  # dfna <- df[is.na(df$value),]
  # 
  # for(i in 1:nrow(dfna)){
  # 
  #   row <- dfna[i,]$placename
  #   col <- dfna[i,]$date
  #   
  #   mat[row,col] <- dfna[i,]$value
  # 
  # }
  return(mat)
  
  
  
}

## Data outputs ####

#### directory where data is saved once complete ###
data_out <- "transformed_data"


## create Daily presence absence Matrices, this takes about 10-15mins
cat_matrix <- species_matrix(cam_data, "genus", "Felis")
write.csv(cat_matrix, paste(data_out, "daily_cat_presence-absence_matrix.csv", sep = "/"))

image(t(as.matrix(cat_matrix)))

dingo_matrix <- species_matrix(cam_data, "genus", "Canis")
write.csv(dingo_matrix, paste(data_out, "daily_dingo_presence-absence_matrix.csv", sep = "/"))

image(t(dingo_matrix))

fox_matrix <- species_matrix(cam_data, "genus", "Vulpes")
write.csv(fox_matrix, paste(data_out, "daily_fox_presence-absence_matrix.csv", sep = "/"))

potorous_matrix <- species_matrix(cam_data,"genus", "Potorous")
write.csv(potorous_matrix, paste(data_out, "daily_potorous_presence-absence_matrix.csv", sep = "/"))

p.tridactylus_matrix <- species_matrix(cam_data,"species", "longipes")
write.csv(p.tridactylus_matrix, paste(data_out, "daily_p_tridactylus_presence-absence_matrix.csv", sep = "/"))

# Bandicoot is not included in this study but is present to show than any species or genus 
# in the dataset can be formatted for unmakred from iamges counts
LN_Bandicoot_matrix <- species_matrix(cam_data,"species", "nasuta")
write.csv(LN_Bandicoot_matrix, paste(data_out, "daily_LN_Bandicoot_presence-absence_matrix.csv", sep = "/"))


## 1.3 Site level covariates ####

site_covar_df <- data.frame(unique(deploy$placename),unique(deploy$latitude),unique(deploy$longitude))

colnames(site_covar_df) <- c("site", "latitude", "longitude")

#East Gippsland region
#bounding box to reduce size and process time for rasters
E_Gips_box <- data.frame(
  xmin = 148,
  xmax = 150,
  ymin = -38,
  ymax = -37
)

### 1.3.1 Black Summer Fire severity category ####

#extract fire severity for each deployment ID


#get raster from predator transect notes

pred_trans_data <- read_excel(paste(directory, "Southern Ark - Predator Transect Forms.xlsx", sep = "/"), sheet = "Sheet2")

unique(pred_trans_data$`Fire severity`)

site_covar_df$fire_sev_infield <- NA

for(i in 1:length(site_covar_df$site)){
  
  site_covar_df$fire_sev_infield[i] <- pred_trans_data[i,]$`Fire severity`
  
}

### 1.3.2 Elevation ####
elevation_raster <- raster(paste(directory,"dem-9s/dem-9s.tif", sep = "/"))  

elevation_trim<- crop(elevation_raster , extent(E_Gips_box$xmin, E_Gips_box$xmax, E_Gips_box$ymin, E_Gips_box$ymax))

# image(elevation_trim)

site_covar_df$elevation <- extract(elevation_trim, cbind(site_covar_df$longitude, site_covar_df$latitude))

### 1.3.3 Forest Landscape Integrity Index (FLII) ####

flii_raster <- raster(paste(directory,"intactness/flii_Oceania.tif", sep = "/")) 
flii_trim<- crop(flii_raster, extent(E_Gips_box$xmin, E_Gips_box$xmax, E_Gips_box$ymin, E_Gips_box$ymax))

image(flii_trim)

site_covar_df$flii <- extract(flii_trim, cbind(site_covar_df$longitude, site_covar_df$latitude))/1000

### 1.3.4 Mean Monthly Total Rainfall 2021-2022 ####

extract_rain_data <- function(directory, nc_files, coordinates) {
  # Read the raster data from the first file to determine the number of rows
  first_rain_raster <- brick(file.path(directory, "Rain_BOM", nc_files[1]))
  n_rows <- nrow(extract(first_rain_raster, coordinates))
  
  # Create an empty data frame to store the results
  rain_data <- data.frame(matrix(NA, nrow = n_rows, ncol = length(nc_files)))
  
  # Loop through each file
  for (i in seq_along(nc_files)) {
    # Read the raster data from the file
    rain_raster <- brick(file.path(directory, "Rain_BOM", nc_files[i]))
    
    # Extract rainfall data for the given coordinates
    rain_month <- extract(rain_raster, coordinates)
    
    # Add the extracted rainfall data to the data frame
    rain_data[, i] <- rain_month
  }
  
  # Set column names based on file names
  colnames(rain_data) <- nc_files
  
  return(rain_data)
}

nc_rain_files <- list.files(paste(directory, "Rain_BOM", sep = "/"))
coordinates <- cbind(site_covar_df$longitude, site_covar_df$latitude)

rain_data <- extract_rain_data(directory, nc_rain_files, coordinates)

site_covar_df$Avg_mnth_tot_rain <- rowMeans(rain_data)


# Load the required libraries
library(tidyr)
library(lubridate)

# Reshape the data
tidy_rain_df <- gather(rain_data, month_file, rainfall)

# Extract site_id and month from the month_file column
tidy_rain_df$site_id <- gsub("^precip_total_r005_(.*?)_.*$", "\\1", tidy_rain_df$month_file)
tidy_rain_df$month <- gsub("^precip_total_r005_.*?_(\\d{6}).*$", "\\1", tidy_rain_df$month_file)

# Parse the month string and format it as "Month Year"
tidy_rain_df$month <- format(ymd(paste(substr(tidy_rain_df$month, 1, 4), substr(tidy_rain_df$month, 5, 6), "01")), "%B %Y")

# Drop the month_file column
tidy_rain_df <- tidy_rain_df[, c("month", "rainfall", "site_id")]

# Print the first few rows of the tidy dataset
head(tidy_rain_df)

### 1.3.5 Mean Monthly NDVI 2021-2022 ####

NDVI_path <- "C:/Users/jmoore/Downloads/NDVI_data"

NDVI_file_list <- list.files(NDVI_path)

# Function to extract NDVI data from raster files
extract_ndvi_data <- function(directory, ndvi_files, coordinates) {
  # Read the raster data from the first file to determine the number of rows
  first_ndvi_raster <- raster(file.path(directory, ndvi_files[1]))
  n_rows <- nrow(extract(first_ndvi_raster, coordinates))
  
  # Create an empty data frame to store the results
  ndvi_data <- data.frame(matrix(NA, nrow = n_rows, ncol = length(ndvi_files)))
  
  colnames(ndvi_data) <- ndvi_files
  
 library(tictoc)
  # Loop through each file
  for (i in ndvi_files) {
    tic()
    # Read the raster data from the file
    ndvi_raster1 <- raster(paste(directory, i, sep = "/"))
    
    ndvi_raster <- projectRaster(ndvi_raster1, crs = crs(elevation_raster))
    
    # Extract NDVI data for the given coordinates
    ndvi_month <- extract(ndvi_raster, coordinates)
    
    # Add the extracted NDVI data to the data frame
    ndvi_data[, i] <- ndvi_month
    toc()
  }
  
  # Set column names based on file names
  colnames(ndvi_data) <- ndvi_files
  
  return(ndvi_data)
}

NDVI_file_list <- list.files(NDVI_path)
# List NDVI raster files
ndvi_files <- list.files(file.path(NDVI_path))

# Extract coordinates
coordinates <- cbind(site_covar_df$longitude, site_covar_df$latitude)

# Extract NDVI data: this takes 
ndvi_data <- extract_ndvi_data(NDVI_path, ndvi_files, coordinates) #this takes about 30 sec per file approx 13 minutes

# Calculate average monthly NDVI for each site
site_covar_df$Avg_mnth_ndvi <- rowMeans(ndvi_data)

### Write site covariates csv ####
write.csv(site_covar_df,  paste(data_out, "site_level_covariates.csv", sep = "/"))

write.csv(ndvi_data, paste(data_out, "NDVI_16day_site-values.csv", sep = "/"))

write.csv(tidy_rain_df, paste(data_out, "rainfall_tot-avg-monthly_site-values.csv", sep = "/"))
