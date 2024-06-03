#Author: Justin Moore

#Project: Fire, Potoroos and Predators

# Plot generation script: Create Outputs for results ####

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
require(unmarked)
require(cowplot)


## Directory setup ####
directory <- "transformed_data"

#load the most supporte models for 

#P.loniges and predators
Model1 <- readRDS(paste(directory, "/Model_1.Rdata", sep = ""))

#Potorous sp. and predators
Model2 <- readRDS(paste(directory, "/Model_2_co_species_1.Rdata", sep = ""))

# 
#   
# 
# 
# bait_cov_plot <- bait_cov %>% pivot_longer(cols = everything(), names_to = "date", values_to = "bait_type") %>%
# 
#   mutate(date = as.Date(sub("^X", "", date), format = "%Y.%m.%d"))
# 


#2. Plotting ####

library(ggplot2)
library(cowplot)

## 2.1 Detection covariate plot generation ####


nd_cond1 <- data.frame(
  bait = rep(c("Synthetic Fermented Egg" , "Tuna Oil", "Chicken pieces in cage" ),  each = 1440),
  pot_trid_det = seq(1, 1440, 1),
  pot_sp_det = seq(1, 1440, 1),
  dingo_det = seq(1, 1440, 1),
  fox_det = seq(1, 1440, 1),
  cat_det = seq(1, 1440, 1),
  length.out = 1440
)

nd_cond <- data.frame(
  bait = rep("Chicken pieces in cage",  length = 1440),
  pot_trid_det = seq(1, 1440, 1),
  pot_sp_det = seq(1, 1440, 1),
  dingo_det = seq(1, 1440, 1),
  fox_det = seq(1, 1440, 1),
  cat_det = seq(1, 1440, 1),
  length.out = 1440
)

#function to enerate predictions and add additional columns for plotting
generate_predictions <- function(model, species, newdata, model_name, conditions) {
  pred <- predict(model, type = "det", species = species, newdata = nd_cond, condition = conditions)
  pred$minute <- seq(1, 1440)  # Assuming the number of predictions equals the number of minutes
  pred$species <- species
  pred$model <- model_name
  pred$bait = nd_cond$bait
  return(pred)
}

# bait_plot <- bait_det_wekly %>%
#   mutate(RowNumber = row_number()) %>%
#   pivot_longer(cols = starts_with("X"), names_to = "Column", values_to = "StringValue")
# 
# bait_plot$Column <- as.integer(gsub("X", "", bait_plot$Column))
# 
# cat_bait_plot <- as.data.frame(week_function(cat)) %>%
#   mutate(RowNumber = row_number()) %>%
#   pivot_longer(cols = starts_with("X"), names_to = "Column", values_to = "StringValue")
# 
# 
# cat_bait_plot$Column <- as.integer(gsub("X", "", cat_bait_plot$Column))
# 
# ggplot(bait_plot, aes(x = Column, y = RowNumber, fill = StringValue)) +
#   geom_tile() +
#   geom_point(data=cat_bait_plot, aes(x=Column,  y = RowNumber))
# 
# as.data.frame(week_function(cat))

# pred <- predict(Model1, type = "det", species = "cat", condition= "-bait",newdata = nd_cond1)


###2.1.1 Detection Time Predictions ####
# Predictions for P.sp from model 2

P.trid1_det_pred_det <- generate_predictions(Model1, "P.tridactylus", nd_cond, "Model 1", "-bait")


P.sp2_det_pred_det <- generate_predictions(Model2, "P.sp", nd_cond, "Model 2", "-bait")


# Predictions for dingo from Model 1
dingo1_det_pred_det <- generate_predictions(Model1, "dingo", nd_cond, "Model 1", "-bait")


# Predictions for dingo from Model 2
dingo2_det_pred_det <- generate_predictions(Model2, "dingo", nd_cond, "Model 2", "-bait")

# Predictions for cat from Model 1
cat1_det_pred_det <- generate_predictions(Model1, "cat", nd_cond, "Model 1", "-bait")

# Predictions for cat from Model 2
cat2_det_pred_det <- generate_predictions(Model2, "cat", nd_cond, "Model 2", "-bait")

# Cat_det_pred_mod2$minute <- rep(c("Chicken", "Tuna Oil", "Synthetic. Egg") , 1440)

# Predictions for fox from Model 1
fox1_det_pred_det <- generate_predictions(Model1, "fox", nd_cond, "Model 1", "-bait")

# Predictions for fox from Model 2
fox2_det_pred_det <- generate_predictions(Model2, "fox", nd_cond, "Model 2", "-bait")


# Combine all data frames using rbind
combined_det_time_predictions <- rbind(
  P.trid1_det_pred_det,
  P.sp2_det_pred_det,
  dingo1_det_pred_det,
  dingo2_det_pred_det,
  cat1_det_pred_det,
  cat2_det_pred_det,
  fox1_det_pred_det,
  fox2_det_pred_det
)

combined_det_time_predictions$minute <- as.POSIXct(combined_det_time_predictions$minute * 60, origin = "2024-05-01", tz = "UTC")

combined_det_time_predictions$minute<- format(combined_det_time_predictions$minute , format = "%H:%M")

combined_det_time_predictions[combined_det_time_predictions$species == "P.tridactylus"|combined_det_time_predictions$species == "P.sp",]$species <-"P. longipes & Potorous sp."

# predict(Model1, type="det", species="cat", newdata=as.data.frame(bait = unique(unlist(bait_det_wekly))))
# unique(unlist(bait_det_wekly))

unique_baits <- c("Synthetic Fermented Egg" , "Tuna Oil", "Chicken pieces in cage" )

#open observation covariate data

site_cov <-  read.csv(paste(directory, "site_level_covariates.csv", sep="/"))

obs_cov <- readRDS(paste(directory, "obsservation_covariates.rds", sep="/"))

# Create a data frame with a single column named "bait"
new_data <- data.frame(bait = unique_baits)

new_data$cat_det <- mean(obs_cov$cat_det[obs_cov$cat_det != 0], na.rm = TRUE)
new_data$dingo_det <- mean(obs_cov$dingo_det[obs_cov$dingo_det != 0], na.rm = TRUE)
new_data$fox_det <- mean(obs_cov$fox_det[obs_cov$fox_det != 0], na.rm = TRUE)
new_data$pot_trid_det <- mean(obs_cov$pot_trid_det[obs_cov$pot_trid_det != 0], na.rm = TRUE)
new_data$pot_sp_det <- mean(obs_cov$pot_sp_det[obs_cov$pot_sp_det != 0], na.rm = TRUE)

obs_cov$cat_det[obs_cov$cat_det != 0]

mean(obs_cov$cat_det[obs_cov$cat_det != 0], na.rm = TRUE)
# Repeat the initial data frame for 100 iterations
# repeated_data <- initial_data[rep(seq_len(nrow(initial_data)), each = 100),]
# 
# # Update row names
# rownames(repeated_data) <- NULL
# 
# # Show the first few rows of the repeated data frame
# head(repeated_data)

###2.1.1 Bait Effect Predictions ####

bait_predict_and_format <- function(model, species, new_data) {
  
  prediction <- predict(model, type = "det", species = species, newdata = new_data)
  
  prediction$bait <- new_data$bait
  
  prediction <- prediction %>%
    pivot_longer(cols = c(upper, lower, Predicted), names_to = "Type", values_to = "Values")

  prediction$species <- species
  prediction$model <- deparse(substitute(model))  # Add the model name as a column
  
  return(prediction)
}

P.trid1_det_pred_bait <- bait_predict_and_format(Model1, "P.tridactylus", new_data)
P.sp2_det_pred_bait <- bait_predict_and_format(Model2, "P.sp", new_data)
cat1_det_pred_bait <- bait_predict_and_format(Model1, "cat", new_data)
cat2_det_pred_bait <- bait_predict_and_format(Model2, "cat", new_data)
dingo1_det_pred_bait <- bait_predict_and_format(Model1, "dingo", new_data)
dingo2_det_pred_bait <- bait_predict_and_format(Model2, "dingo", new_data)
fox1_det_pred_bait <- bait_predict_and_format(Model1, "fox", new_data)
fox2_det_pred_bait <- bait_predict_and_format(Model2, "fox", new_data)

# Combine all data frames using rbind
combined_det_bait_predictions <- rbind(
  P.trid1_det_pred_bait,
  P.sp2_det_pred_bait,
  dingo1_det_pred_bait,
  dingo2_det_pred_bait,
  cat1_det_pred_bait,
  cat2_det_pred_bait,
  fox1_det_pred_bait,
  fox2_det_pred_bait
)

combined_det_bait_predictions[combined_det_bait_predictions$species == "P.tridactylus"|combined_det_bait_predictions$species == "P.sp",]$species <-"P. longipes & Potorous sp."


  combined_det_bait_predictions$bait <- factor(combined_det_bait_predictions$bait, 
                                               levels = c("Synthetic Fermented Egg", "Tuna Oil", "Chicken pieces in cage"), 
                                               labels = c("Synthetic Egg", "Tuna Oil", "Chicken"))


library(patchwork)

bait_plots <- 
  ggplot(combined_det_bait_predictions, aes(x = bait, y = Values, colour = model, fill = model)) +
  geom_boxplot(alpha = 0.21) +
  labs(x = "Bait", y = "Values") +
  scale_color_manual(values = c("red", "black"), labels = c("P.longipes", "Potorous sp.")) +
  scale_fill_manual(values = c("red", "lightblue"), labels = c("P.longipes", "Potorous sp.")) +
  scale_alpha_manual(values = c(0.7, 0.7)) +  # Adjust alpha values if needed
  facet_wrap(vars(species), ncol = 1) +
  theme_bw() +
  labs(x = "Bait Type", y = "Detection Probability") +
  ylim(0, 1.00)  # Set the y-axis limit

det_plots <- 
  ggplot(combined_det_time_predictions %>% filter(minute != "00:00")) +
  aes(x = as.POSIXct(minute, format = "%H:%M"), y = Predicted, colour = model) +
  geom_line(size = .5) +
  scale_color_manual(values = c("red", "black")) +  # Reversed colors
  scale_alpha_manual(values = c(1, 0.5)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = model), alpha = 0.21, size = 0.75) +
  facet_wrap(vars(species), ncol = 1) +
  scale_x_datetime(date_labels = "%H:%M", breaks = "6 hour") +
  labs(x = "Time of Detection") +
  theme_bw()+theme(legend.position = "none", axis.title.y = element_blank())

# Figure 14. Combine plots into a single row with shared y-axis
combined_plots <- (bait_plots / det_plots) + plot_layout(ncol = 2, guides = 'collect')

##2.2. Plot variable effects ####


#data frame setup for marginal occupancy. all fire sev set to UB (Unburnt)
nd_marg_elev <- data.frame(elevation = seq(min(site_cov$elevation), max(site_cov$elevation), length.out = 1000),
                           flii = rep(mean(site_cov$flii), 1000),
                           fire_sev_infield = rep("H", 1000),
                           Avg_mnth_tot_rain = rep(mean(site_cov$Avg_mnth_tot_rain), 1000),
                           Avg_mnth_ndvi = rep(mean(site_cov$Avg_mnth_ndvi), 1000))

nd_marg_rain <- data.frame(elevation = rep(mean(site_cov$elevation), 1000),
                           flii = rep(mean(site_cov$flii), 1000),
                           fire_sev_infield = rep("H", 1000),
                           Avg_mnth_tot_rain = seq(min(site_cov$Avg_mnth_tot_rain), max(site_cov$Avg_mnth_tot_rain), length.out = 1000),
                           Avg_mnth_ndvi = rep(mean(site_cov$Avg_mnth_ndvi), 1000))

nd_marg_flii <- data.frame(elevation = rep(mean(site_cov$elevation), 1000),
                           flii = seq(min(site_cov$flii), max(site_cov$flii), length.out = 1000),
                           fire_sev_infield = rep("H", 1000),
                           Avg_mnth_tot_rain = rep(mean(site_cov$Avg_mnth_tot_rain), 1000),
                           Avg_mnth_ndvi = rep(mean(site_cov$Avg_mnth_ndvi), 1000))
nd_marg_ndvi <- data.frame(elevation = rep(mean(site_cov$elevation), 1000),
                           flii = rep(mean(site_cov$flii), 1000),
                           fire_sev_infield = rep("H", 1000),
                           Avg_mnth_tot_rain = rep(mean(site_cov$Avg_mnth_tot_rain), 1000),
                           Avg_mnth_ndvi = seq(min(site_cov$Avg_mnth_ndvi), max(site_cov$Avg_mnth_ndvi), length.out = 1000))

nd_marg_fire <- data.frame(elevation = rep(mean(site_cov$elevation), 1000),
                           flii = rep(mean(site_cov$flii), 1000),
                           fire_sev_infield = c(rep("UB", 250),rep("L/M", 250),rep("H", 250),rep("VH", 250)),
                           Avg_mnth_tot_rain = rep(mean(site_cov$Avg_mnth_tot_rain), 1000),
                           Avg_mnth_ndvi = rep(mean(site_cov$Avg_mnth_ndvi), 1000))

###2.2.1 Fire ####
# Model 1, Species: Cat
cat_marg_fire1 <- predict(Model1, type = 'state', species = 'cat', newdata = nd_marg_fire)
cat_marg_fire1$species <- "cat"
cat_marg_fire1$fire <-c(rep("1. UB", 250),rep("2. L/M", 250),rep("3. H", 250),rep("4. VH", 250))
cat_marg_fire1$model <- "Model 1"

# Model 2, Species: Cat
cat_marg_fire2 <- predict(Model2, type = 'state', species = 'cat', newdata = nd_marg_fire)
cat_marg_fire2$species <- "cat"
cat_marg_fire2$fire <- c(rep("1. UB", 250),rep("2. L/M", 250),rep("3. H", 250),rep("4. VH", 250))
cat_marg_fire2$model <- "Model 2"

# Model 1, Species: Dingo
dingo_marg_fire1 <- predict(Model1, type = 'state', species = 'dingo', newdata = nd_marg_fire)
dingo_marg_fire1$species <- "dingo"
dingo_marg_fire1$fire <- c(rep("1. UB", 250),rep("2. L/M", 250),rep("3. H", 250),rep("4. VH", 250))
dingo_marg_fire1$model <- "Model 1"

# Model 2, Species: Dingo
dingo_marg_fire2 <- predict(Model2, type = 'state', species = 'dingo', newdata = nd_marg_fire)
dingo_marg_fire2$species <- "dingo"
dingo_marg_fire2$fire <- c(rep("1. UB", 250),rep("2. L/M", 250),rep("3. H", 250),rep("4. VH", 250))
dingo_marg_fire2$model <- "Model 2"

# Model 1, Species: Fox
fox_marg_fire1 <- predict(Model1, type = 'state', species = 'fox', newdata = nd_marg_fire)
fox_marg_fire1$species <- "fox"
fox_marg_fire1$fire <- c(rep("1. UB", 250),rep("2. L/M", 250),rep("3. H", 250),rep("4. VH", 250))
fox_marg_fire1$model <- "Model 1"

# Model 2, Species: Fox
fox_marg_fire2 <- predict(Model2, type = 'state', species = 'fox', newdata = nd_marg_fire)
fox_marg_fire2$species <- "fox"
fox_marg_fire2$fire <- c(rep("1. UB", 250),rep("2. L/M", 250),rep("3. H", 250),rep("4. VH", 250))
fox_marg_fire2$model <- "Model 2"

# Model 1, Species: P.tridactylus
P.trid_marg_fire1 <- predict(Model1, type = 'state', species = 'P.tridactylus', newdata = nd_marg_fire)
P.trid_marg_fire1$species <- "P.longipes & Potorous sp."
P.trid_marg_fire1$fire <- c(rep("1. UB", 250),rep("2. L/M", 250),rep("3. H", 250),rep("4. VH", 250))
P.trid_marg_fire1$model <- "Model 1"

# Model 2, Species: P.sp
P.sp_marg_fire2 <- predict(Model2, type = 'state', species = 'P.sp', newdata = nd_marg_fire)
P.sp_marg_fire2$species <- "P.longipes & Potorous sp."
P.sp_marg_fire2$fire <- c(rep("1. UB", 250),rep("2. L/M", 250),rep("3. H", 250),rep("4. VH", 250))
P.sp_marg_fire2$model <- "Model 2"

combine_fire_marg <- rbind(cat_marg_fire1, 
                           cat_marg_fire2, 
                           dingo_marg_fire1, 
                           dingo_marg_fire2, 
                           fox_marg_fire1, 
                           fox_marg_fire2, 
                           P.trid_marg_fire1, 
                           P.sp_marg_fire2)

combine_fire_marg$upper_SE <- combine_fire_marg$Predicted + combine_fire_marg$SE

combine_fire_marg$lower_SE <- combine_fire_marg$Predicted - combine_fire_marg$SE

combine_fire_marg <- combine_fire_marg %>%
  pivot_longer(cols = c(upper_SE, lower_SE, Predicted), names_to = "Type", values_to = "Predicted")

## Figure 4. Fire and predators ####
  ggplot(combine_fire_marg, aes(x = fire, y = Predicted, colour=model, fill=model)) +
  geom_boxplot(alpha = 0.3) +
  labs(x = "Fire Severity", y = "Occupancy probability") +
  scale_color_manual(name = "Model", values = c("red", "black"), labels = c("P. longipes model", "Potorous sp. model")) +
  scale_fill_manual(name = "Model", values = c("red", "lightblue"), labels = c("P. longipes model", "Potorous sp. model")) +
  scale_alpha_manual(values = c(0.8, 0.7)) +
  facet_wrap(vars(species), nrow = 1) +
  theme_bw() +
  ylim(0, 1.00)  # Set the y-axis limit
  


  ###2.2.2 FLII ####
  # Model 1, Species: Cat
  cat_marg_flii1 <- predict(Model1, type = 'state', species = 'cat', newdata = nd_marg_flii)
  cat_marg_flii1$species <- "cat"
  cat_marg_flii1$model <- "Model 1"
  cat_marg_flii1$flii <- nd_marg_flii$flii
  
  # Model 2, Species: Cat
  cat_marg_flii2 <- predict(Model2, type = 'state', species = 'cat', newdata = nd_marg_flii)
  cat_marg_flii2$species <- "cat"
  cat_marg_flii2$model <- "Model 2"
  cat_marg_flii2$flii <- nd_marg_flii$flii
  

  
  # Model 2, Species: P.sp
  P.sp_marg_flii2 <- predict(Model2, type = 'state', species = 'P.sp', newdata = nd_marg_flii)
  P.sp_marg_flii2$species <- "Potorous sp."
  P.sp_marg_flii2$model <- "Model 2"
  P.sp_marg_flii2$flii <- nd_marg_flii$flii
  
  combine_flii_marg <- rbind(cat_marg_flii1, 
                             cat_marg_flii2, 
                                            P.sp_marg_flii2)
  # 
  # combine_fire_marg <- combine_fire_marg %>%
  #   pivot_longer(cols = c(upper, lower, Predicted), names_to = "Type", values_to = "Values")
  
  flii_marg_plots <- 
    ggplot(combine_flii_marg, aes(x = flii, y = Predicted, colour=model, fill=model)) +
    geom_line(alpha = 0.21) +
    labs(x = "Bait", y = "Values") +
      geom_line(size = 1.5) +
    scale_color_manual(values = c("red", "black")) +  # Reversed colors
    scale_alpha_manual(values = c(1, 0.5)) +
    scale_linetype_manual(values = c("solid", "dashed")) +
    geom_ribbon(aes(ymin = Predicted-SE, ymax = Predicted+SE, fill = model), alpha = 0.21, size = 0.5) +
    facet_wrap(vars(species), nrow = 1)+
    theme_bw() +
    labs(x = "FLII", y = "Marginal Probability")

  ###2.2.3 Elevation ####
  # Model 1, Species: Dingo
  dingo_marg_elev1 <- predict(Model1, type = 'state', species = 'dingo', newdata = nd_marg_elev)
  dingo_marg_elev1$species <- "dingo"
  dingo_marg_elev1$model <- "Model 1"
  dingo_marg_elev1$elevation <- nd_marg_elev$elevation
  
  # Model 2, Species: Dingo
  dingo_marg_elev2 <- predict(Model2, type = 'state', species = 'dingo', newdata = nd_marg_elev)
  dingo_marg_elev2$species <- "dingo"
  dingo_marg_elev2$model <- "Model 2"
  dingo_marg_elev2$elevation <- nd_marg_elev$elevation
  
  # Model 1, Species: Dingo
  fox_marg_elev1 <- predict(Model1, type = 'state', species = 'fox', newdata = nd_marg_elev)
  fox_marg_elev1$species <- "fox"
  fox_marg_elev1$model <- "Model 1"
  fox_marg_elev1$elevation <- nd_marg_elev$elevation
  
  # Model 2, Species: Dingo
  fox_marg_elev2 <- predict(Model2, type = 'state', species = 'fox', newdata = nd_marg_elev)
  fox_marg_elev2$species <- "fox"
  fox_marg_elev2$model <- "Model 2"
  fox_marg_elev2$elevation <- nd_marg_elev$elevation
  
  # Model 1, Species: P.long
  P.trid_marg_elev1 <- predict(Model1, type = 'state', species = 'P.tridactylus', newdata = nd_marg_elev)
  P.trid_marg_elev1$species <- "P.longipes & Potorous sp."
  P.trid_marg_elev1$model <- "Model 1"
  P.trid_marg_elev1$elevation <- nd_marg_elev$elevation
  
  # Model 2, Species: P.sp
  P.sp_marg_elev2 <- predict(Model2, type = 'state', species = 'P.sp', newdata = nd_marg_elev)
  P.sp_marg_elev2$species <- "P.longipes & Potorous sp."
  P.sp_marg_elev2$model <- "Model 2"
  P.sp_marg_elev2$elevation <- nd_marg_elev$elevation
  
  combine_elev_marg <- rbind(dingo_marg_elev1, 
                             dingo_marg_elev2, 
                             fox_marg_elev1, 
                             fox_marg_elev2,
                             P.trid_marg_elev1,
                             P.sp_marg_elev2)
  # 
  # combine_fire_marg <- combine_fire_marg %>%
  #   pivot_longer(cols = c(upper, lower, Predicted), names_to = "Type", values_to = "Values")
  
  elevation_marg_plots <- 
    ggplot(combine_elev_marg, aes(x = elevation, y = Predicted, colour=model, fill=model)) +
    geom_line(alpha = 0.21) +
    labs(x = "Bait", y = "Values") +
    geom_line(size = 1.5) +
    scale_color_manual(values = c("red", "black")) +  # Reversed colors
    scale_alpha_manual(values = c(1, 0.5)) +
    scale_linetype_manual(values = c("solid", "dashed")) +
    geom_ribbon(aes(ymin = Predicted-SE, ymax = Predicted+SE, fill = model), alpha = 0.21, size = 0.5) +
    facet_wrap(vars(species), nrow = 1)+
    theme_bw() +
    labs(x = "elevation (m)", y = "Marginal Probability")
  
  ###2.2.4 Mean monthly total rainfall 2021-2022 ####
  # Model 1, Species: Dingo
  dingo_marg_rain1 <- predict(Model1, type = 'state', species = 'dingo', newdata = nd_marg_rain)
  dingo_marg_rain1$species <- "dingo"
  dingo_marg_rain1$model <- "Model 1"
  dingo_marg_rain1$rain <- nd_marg_rain$Avg_mnth_tot_rain
  
  # Model 2, Species: Dingo
  dingo_marg_rain2 <- predict(Model2, type = 'state', species = 'dingo', newdata = nd_marg_rain)
  dingo_marg_rain2$species <- "dingo"
  dingo_marg_rain2$model <- "Model 2"
  dingo_marg_rain2$rain <- nd_marg_rain$Avg_mnth_tot_rain
  
  # Model 1, Species: Dingo
  fox_marg_rain1 <- predict(Model1, type = 'state', species = 'fox', newdata = nd_marg_rain)
  fox_marg_rain1$species <- "fox"
  fox_marg_rain1$model <- "Model 1"
  fox_marg_rain1$rain <- nd_marg_rain$Avg_mnth_tot_rain
  
  # Model 2, Species: Dingo
  fox_marg_rain2 <- predict(Model2, type = 'state', species = 'fox', newdata = nd_marg_rain)
  fox_marg_rain2$species <- "fox"
  fox_marg_rain2$model <- "Model 2"
  fox_marg_rain2$rain <- nd_marg_rain$Avg_mnth_tot_rain
  
  # Model 2, Species: P.sp
  P.trid_marg_rain1 <- predict(Model1, type = 'state', species = 'P.tridactylus', newdata = nd_marg_rain)
  P.trid_marg_rain1$species <- "P.longipes & Potorous sp."
  P.trid_marg_rain1$model <- "Model 1"
  P.trid_marg_rain1$rain <- nd_marg_rain$Avg_mnth_tot_rain
  
  
  # Model 2, Species: P.sp
  P.sp_marg_rain2 <- predict(Model2, type = 'state', species = 'P.sp', newdata = nd_marg_rain)
  P.sp_marg_rain2$species <- "P.longipes & Potorous sp."
  P.sp_marg_rain2$model <- "Model 2"
  P.sp_marg_rain2$rain <- nd_marg_rain$Avg_mnth_tot_rain
  
  combine_rain_marg <- rbind(dingo_marg_rain1, 
                             dingo_marg_rain2, 
                             fox_marg_rain1, 
                             fox_marg_rain2, 
                             P.trid_marg_rain1,
                             P.sp_marg_rain2)

  # 
  # combine_fire_marg <- combine_fire_marg %>%
  #   pivot_longer(cols = c(upper, lower, Predicted), names_to = "Type", values_to = "Values")
  
 rain_marg_plots <- 
    ggplot(combine_rain_marg, aes(x =rain, y = Predicted, colour=model, fill=model)) +
    geom_line(alpha = 0.21) +
    labs(x = "Bait", y = "Values") +
    geom_line(size = 1.5) +
    scale_color_manual(values = c("red", "black")) +  # Reversed colors
    scale_alpha_manual(values = c(1, 0.5)) +
    scale_linetype_manual(values = c("solid", "dashed")) +
    geom_ribbon(aes(ymin = Predicted-SE, ymax = Predicted+SE, fill = model), alpha = 0.21, size = 0.5) +
    facet_wrap(vars(species), nrow = 1)+
    theme_bw() +
    labs(x = "Mean monthly NDVI", y = "Marginal Probability")  
  
 
 ###2.2.5 Mean monthly NDVI 2021-2022 ####

 # Model 2, Species: P.sp
 P.sp_marg_ndvi2 <- predict(Model2, type = 'state', species = 'P.sp', newdata = nd_marg_ndvi)
 P.sp_marg_ndvi2$species <- "Potorous sp."
 P.sp_marg_ndvi2$model <- "Model 2"
 P.sp_marg_ndvi2$ndvi <- nd_marg_ndvi$Avg_mnth_ndvi
 
 # 
 # combine_fire_marg <- combine_fire_marg %>%
 #   pivot_longer(cols = c(upper, lower, Predicted), names_to = "Type", values_to = "Values")
 
 ndvi_marg_plots <- 
   ggplot(P.sp_marg_ndvi2, aes(x =ndvi, y = Predicted, colour=model, fill=model)) +
   geom_line(size = 1.5) +
   scale_fill_manual(values = c( "lightblue")) +  # Reversed colors
   scale_color_manual(values = c( "black")) +  # Reversed colors
   scale_alpha_manual(values = c(1, 0.5)) +
   scale_linetype_manual(values = c("solid", "dashed")) +
   geom_ribbon(aes(ymin = Predicted-SE, ymax = Predicted+SE, fill = model), alpha = 0.21, size = 0.5) +
   facet_wrap(vars(species), nrow = 1)+
   theme_bw() +
   labs(x = "Mean monthly NDVI", y = "Marginal Probability")  
 
 
# (fire_cat_plot / fire_dingo_plot/ fire_fox_plot/fire_P_trid_plot ) + plot_layout(ncol = 1, guides = 'collect')
 
 # combined_plots <- cowplot::plot_grid(
 #   fire_marge_plots, flii_marg_plots, elevation_marg_plots, 
 #   rain_marg_plots, ndvi_marg_plots, ncol = 2
 # )
 
 
  #fire
  # Model 1, Species: Cat
  cat_marg_fire1 <- predict(Model1, type = 'state', species = 'cat', newdata = nd_marg_fire)
  cat_marg_fire1$species <- "cat"
  cat_marg_fire1$fire <-c(rep("1. UB", 250),rep("2. L/M", 250),rep("3. H", 250),rep("4. VH", 250))
  cat_marg_fire1$model <- "Model 1"
  
  # Model 2, Species: Cat
  cat_marg_fire2 <- predict(Model2, type = 'state', species = 'cat', newdata = nd_marg_fire)
  cat_marg_fire2$species <- "cat"
  cat_marg_fire2$fire <- c(rep("1. UB", 250),rep("2. L/M", 250),rep("3. H", 250),rep("4. VH", 250))
  cat_marg_fire2$model <- "Model 2"
  
  # Model 1, Species: Dingo
  dingo_marg_fire1 <- predict(Model1, type = 'state', species = 'dingo', newdata = nd_marg_fire)
  dingo_marg_fire1$species <- "dingo"
  dingo_marg_fire1$fire <- c(rep("1. UB", 250),rep("2. L/M", 250),rep("3. H", 250),rep("4. VH", 250))
  dingo_marg_fire1$model <- "Model 1"
  
  # Model 2, Species: Dingo
  dingo_marg_fire2 <- predict(Model2, type = 'state', species = 'dingo', newdata = nd_marg_fire)
  dingo_marg_fire2$species <- "dingo"
  dingo_marg_fire2$fire <- c(rep("1. UB", 250),rep("2. L/M", 250),rep("3. H", 250),rep("4. VH", 250))
  dingo_marg_fire2$model <- "Model 2"
  
  # Model 1, Species: Fox
  fox_marg_fire1 <- predict(Model1, type = 'state', species = 'fox', newdata = nd_marg_fire)
  fox_marg_fire1$species <- "fox"
  fox_marg_fire1$fire <- c(rep("1. UB", 250),rep("2. L/M", 250),rep("3. H", 250),rep("4. VH", 250))
  fox_marg_fire1$model <- "Model 1"
  
  # Model 2, Species: Fox
  fox_marg_fire2 <- predict(Model2, type = 'state', species = 'fox', newdata = nd_marg_fire)
  fox_marg_fire2$species <- "fox"
  fox_marg_fire2$fire <- c(rep("1. UB", 250),rep("2. L/M", 250),rep("3. H", 250),rep("4. VH", 250))
  fox_marg_fire2$model <- "Model 2"
  
  # Model 1, Species: P.tridactylus
  P.trid_marg_fire1 <- predict(Model1, type = 'state', species = 'P.tridactylus', newdata = nd_marg_fire)
  P.trid_marg_fire1$species <- "P.longipes & Potorous sp."
  P.trid_marg_fire1$fire <- c(rep("1. UB", 250),rep("2. L/M", 250),rep("3. H", 250),rep("4. VH", 250))
  P.trid_marg_fire1$model <- "Model 1"
  
  # Model 2, Species: P.sp
  P.sp_marg_fire2 <- predict(Model2, type = 'state', species = 'P.sp', newdata = nd_marg_fire)
  P.sp_marg_fire2$species <- "P.longipes & Potorous sp."
  P.sp_marg_fire2$fire <- c(rep("1. UB", 250),rep("2. L/M", 250),rep("3. H", 250),rep("4. VH", 250))
  P.sp_marg_fire2$model <- "Model 2"
  
  combine_fire_marg <- rbind(cat_marg_fire1, 
                             cat_marg_fire2, 
                             dingo_marg_fire1, 
                             dingo_marg_fire2, 
                             fox_marg_fire1, 
                             fox_marg_fire2, 
                             P.trid_marg_fire1, 
                             P.sp_marg_fire2)
  
  combine_fire_marg <- combine_fire_marg %>%
    pivot_longer(cols = c(upper, lower, Predicted), names_to = "Type", values_to = "Values")
  
  # fire_marge_plots <- 
  #   ggplot(combine_fire_marg, aes(x = fire, y = Values, colour=model, fill=model)) +
  #   geom_boxplot(alpha = 0.21) +
  #   labs(x = "Bait", y = "Values") +
  #   scale_color_manual(values = c("red", "black")) +
  #   scale_alpha_manual(values = c(0.7, 0.5)) +# Reversed colors
  #   facet_wrap(vars(species), nrow = 1)+
  #   theme_bw() +
  #   labs(x = "Bait Type", y = "Detection Probability") 
  # ylim(0, 1.00)  # Set the y-axis limit  
  # 


###2.2.6 Plots ####
# p.sp

flii_p.sp_plots <- 
  ggplot(combine_flii_marg%>% filter(species == "Potorous sp."), aes(x = flii, y = Predicted, colour=model, fill=model)) +
  geom_line(alpha = 0.21) +
  geom_line(size = 1.5) +
  scale_fill_manual(values = "lightblue") +
  scale_color_manual(values = "black") +  # Reversed colors
  scale_alpha_manual(values = c(1, 0.5)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = model), alpha = 0.4, size = 0.5) +
  facet_wrap(vars(species), nrow = 1)+
  theme_bw() + theme(legend.position = "none") +
  labs(x = "FLII", y = "Marginal occupancy")+
  ylim(0, 1)  

elevation_p.sp_plots <- 
  ggplot(combine_elev_marg %>% filter(species == "P.longipes & Potorous sp."), aes(x = elevation, y = Predicted, colour=model, fill=model)) +
  geom_line(alpha = 0.21) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c("red", "black")) +  # Reversed colors
  scale_alpha_manual(values = c(1, 0.5)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = model), alpha = 0.21, size = 0.5) +
  facet_wrap(vars(species), nrow = 1)+
  theme_bw() + theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(x = "elevation (m)")+
  ylim(0,1)  

rain_p.sp_plots <- 
  ggplot(combine_rain_marg %>% filter(species == "P.longipes & Potorous sp."), aes(x =rain, y = Predicted, colour=model, fill=model)) +
  geom_line(alpha = 0.21) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c("red", "black")) +  # Reversed colors
  scale_alpha_manual(values = c(1, 0.5)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = model), alpha = 0.21, size = 0.5) +
  facet_wrap(vars(species), nrow = 1)+
  theme_bw() + theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(x = "Mean monthly total rainfall (mm)") +
  ylim(0,1)  

ndvi_p.sp_plots <- 
  ggplot(P.sp_marg_ndvi2, aes(x =ndvi/1000, y = Predicted, colour=model, fill=model)) +
  geom_line(alpha = 0.2) +
  geom_line(size = 1.5) +
  scale_fill_manual(values = "lightblue") +
  scale_color_manual(values = "black") +  # Reversed colors
  scale_alpha_manual(values = c(1, 0.5)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = model), alpha = 0.4, size = 0.5) +
  facet_wrap(vars(species), nrow = 1)+
  theme_bw() + theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(x = "Mean monthly NDVI") +
  ylim(0,1) 



pots <- plot_grid(
  flii_p.sp_plots,elevation_p.sp_plots, rain_p.sp_plots, ndvi_p.sp_plots,
  # labels = c("Fire Fox", "", "Elevation Fox", "", "Rain Fox"),
  nrow = 1, ncol = 4
)


# fox


elevation_fox_plots <- 
  ggplot(combine_elev_marg %>% filter(species == "fox"), aes(x = elevation, y = Predicted, colour=model, fill=model)) +
  geom_line(alpha = 0.21) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c("red", "black")) +  # Reversed colors
  scale_alpha_manual(values = c(1, 0.5)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = model), alpha = 0.21, size = 0.5) +
  facet_wrap(vars(species), nrow = 1)+
  theme_bw() + theme(legend.position = "none") +
  labs(x = "elevation (m)", y = "Marginal occupancy")+
  ylim(0,1)  

rain_fox_plots <- 
  ggplot(combine_rain_marg %>% filter(species == "fox"), aes(x =rain, y = Predicted, colour=model, fill=model)) +
  geom_line(alpha = 0.21) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c("red", "black")) +  # Reversed colors
  scale_alpha_manual(values = c(1, 0.5)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = model), alpha = 0.21, size = 0.5) +
  facet_wrap(vars(species), nrow = 1)+
  theme_bw() + theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(x = "Mean monthly total rainfall (mm)") +
  ylim(0,1)    


#fox <-  (fire_fox_plot /NULL /  elevation_fox_plots/ NUll/ rain_fox_plots) + plot_layout(nrow = 1, ncol = 5, guides = 'collect')

fox <- plot_grid(
  elevation_fox_plots, rain_fox_plots,  NULL,
  # labels = c("Fire Fox", "", "Elevation Fox", "", "Rain Fox"),
  nrow = 1, ncol = 4
)

# dingo


elevation_dingo_plots <- 
  ggplot(combine_elev_marg %>% filter(species == "dingo"), aes(x = elevation, y = Predicted, colour=model, fill=model)) +
  geom_line(alpha = 0.21) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c("red", "black")) +  # Reversed colors
  scale_alpha_manual(values = c(1, 0.5)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  geom_ribbon(aes(ymin =lower, ymax = upper, fill = model), alpha = 0.21, size = 0.5) +
  facet_wrap(vars(species), nrow = 1)+
  theme_bw() + theme(legend.position = "none") +
  labs(x = "elevation (m)", "Marginal occupancy")+
  ylim(0,1)  

rain_dingo_plots <- 
  ggplot(combine_rain_marg %>% filter(species == "dingo"), aes(x =rain, y = Predicted, colour=model, fill=model)) +
  geom_line(alpha = 0.21) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c("red", "black")) +  # Reversed colors
  scale_alpha_manual(values = c(1, 0.5)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = model), alpha = 0.21, size = 0.5) +
  facet_wrap(vars(species), nrow = 1)+
  theme_bw() + theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(x = "Mean monthly total rainfall (mm)") +
  ylim(0,1)    


#dingo <-  (fire_dingo_plot /NULL /  elevation_dingo_plots/ NUll/ rain_dingo_plots) + plot_layout(nrow = 1, ncol = 5, guides = 'collect')

dingo <- plot_grid(
  elevation_dingo_plots, rain_dingo_plots, NULL, NULL,
  # labels = c("Fire dingo", "", "Elevation dingo", "", "Rain dingo"),
  nrow = 1, ncol = 4
)

# cat
fire_cat_plot <- 
  ggplot(combine_fire_marg %>% filter(species == "cat"), aes(x = fire, y = Predicted, colour=model, fill=model)) +
  geom_boxplot(alpha = 0.21) +
  
  scale_color_manual(values = c("red", "black")) +
  scale_alpha_manual(values = c(0.7, 0.5)) +# Reversed colors
  facet_wrap(vars(species), nrow = 1)+
  theme_bw() +  theme(legend.position = "none") +
  labs(x = "Fire severity", y = "Marginal occupancy") +
  ylim(0,1)       # Set the y-axis limit  

flii_cat_plots <- 
  ggplot(combine_flii_marg%>% filter(species == "cat"), aes(x = flii, y = Predicted, colour=model, fill=model)) +
  geom_line(alpha = 0.21) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c("red", "black")) +  # Reversed colors
  scale_alpha_manual(values = c(1, 0.5)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = model), alpha = 0.21, size = 0.5) +
  facet_wrap(vars(species), nrow = 1)+
  theme_bw() + theme(legend.position = "none") +
  labs(x = "FLII", y = "Marginal occupancy")+
  ylim(0, 1)  

#cat <-  (fire_cat_plot /NULL /  elevation_cat_plots/ NUll/ rain_cat_plots) + plot_layout(nrow = 1, ncol = 5, guides = 'collect')

cat <- plot_grid(
  flii_cat_plots, NULL, NULL, NULL,
  # labels = c("Fire cat", "", "Elevation cat", "", "Rain cat"),
  nrow = 1, ncol = 4,  align = "v"
)

## Figure 7: Plots for marginal occpuancy ####
(pots / fox /dingo/cat) + plot_layout(nrow = 4, guides = 'collect')

##2.3. Plots for co-occurunce ####

short_fire <- unique(nd_marg_fire)


# Load necessary library
library(dplyr)


### 2.3.1.1. Plots for fire Model 1 co-occurrence ####
# Create the data frame manually
data_model1 <- data.frame(
  Interaction = c("P.longipes:cat", "P.longipes:cat", "P.longipes:cat", "P.longipes:cat", "P.longipes:cat", "P.longipes:cat", "P.longipes:cat",
                  "P.longipes:dingo", "P.longipes:dingo", "P.longipes:dingo", "P.longipes:dingo", "P.longipes:dingo", "P.longipes:dingo",
                  "P.longipes:fox", "P.longipes:fox", "P.longipes:fox", "P.longipes:fox", "P.longipes:fox", "P.longipes:fox",
                  "cat:dingo", "cat:dingo", "cat:dingo", "cat:dingo", "cat:dingo", "cat:dingo", "cat:dingo",
                  "cat:fox", "cat:fox", "cat:fox", "cat:fox", "cat:fox", "cat:fox", "cat:fox",
                  "dingo:fox", "dingo:fox", "dingo:fox", "dingo:fox", "dingo:fox", "dingo:fox"),
  Predictor = c("3.  H", "2.  L/M", "1.  UB", "4.  VH", "scale(elevation)", "scale(Avg_mnth_tot_rain)", "scale(flii)",
                "3.  H", "2.  L/M", "1.  UB", "4.  VH", "scale(elevation)", "scale(Avg_mnth_tot_rain)",
                "3.  H", "2.  L/M", "1.  UB", "4.  VH", "scale(elevation)", "scale(Avg_mnth_tot_rain)",
                "3.  H", "2.  L/M", "1.  UB", "4.  VH", "scale(flii)", "scale(elevation)", "scale(Avg_mnth_tot_rain)",
                "3.  H", "2.  L/M", "1.  UB", "4.  VH", "scale(flii)", "scale(elevation)", "scale(Avg_mnth_tot_rain)",
                "3.  H", "2.  L/M", "1.  UB", "4.  VH", "scale(elevation)", "scale(Avg_mnth_tot_rain)"),
  Estimate = c(0.65519, 0.28422, -1.56513, 0.44545, 0.45974, -1.09976, -0.41622,
               0.32529, 0.24574, -1.77629, 0.26928, 0.21006, -0.59533,
               0.40862, -0.50131, -0.89450, -0.00461, -0.39171, -0.39330,
               0.99396, -0.19383, -0.13086, 0.42715, -0.39683, 0.90712, -0.13356,
               0.53618, 0.93174, -1.03514, -0.27934, 0.16002, 0.39747, -0.50222,
               1.33466, 0.08652, -0.60395, -0.25712, 0.48851, -0.25507),
  StdError = c(0.506, 0.484, 0.405, 0.360, 0.321, 0.518, 0.515,
               0.554, 0.601, 0.450, 0.388, 0.492, 0.678,
               0.517, 0.472, 0.295, 0.452, 0.425, 0.714,
               0.529, 0.474, 0.593, 0.471, 0.607, 0.603, 0.541,
               0.482, 0.451, 0.414, 0.304, 0.606, 0.503, 0.689,
               0.510, 0.419, 0.482, 0.387, 0.540, 0.532),
  ZValue = c(1.2955, 0.5868, -3.8690, 1.2366, 1.4323, -2.1238, -0.8082,
             0.5877, 0.4088, -3.9480, 0.6939, 0.4273, -0.8775,
             0.7898, -1.0631, -3.0291, -0.0102, -0.9208, -0.5507,
             1.8783, -0.4094, -0.2205, 0.9068, -0.6538, 1.5039, -0.2467,
             1.1121, 2.0673, -2.5017, -0.9201, 0.2642, 0.7898, -0.7294,
             2.6159, 0.2065, -1.2529, -0.6636, 0.9039, -0.4795),
  PValue = c(1.95e-01, 5.57e-01, 1.09e-04, 2.16e-01, 1.52e-01, 3.37e-02, 4.19e-01,
             5.57e-01, 6.83e-01, 7.88e-05, 4.88e-01, 6.69e-01, 3.80e-01,
             4.30e-01, 2.88e-01, 2.45e-03, 9.92e-01, 3.57e-01, 5.82e-01,
             6.03e-02, 6.82e-01, 8.25e-01, 3.65e-01, 5.13e-01, 1.33e-01, 8.05e-01,
             2.66e-01, 3.87e-02, 1.24e-02, 3.58e-01, 7.92e-01, 4.30e-01, 4.66e-01,
             8.90e-03, 8.36e-01, 2.10e-01, 5.07e-01, 3.66e-01, 6.32e-01)
)

data_model1$significant <- ifelse(data_model1$PValue <= 0.05, "Significant","Not significant")
library(stringr)
## Figure 5: P. longipes/predator fire estimates ####
data_model1 %>%
  filter(!(Predictor %in% c("scale(elevation)", "scale(Avg_mnth_tot_rain)", "scale(flii)"))) %>%
  filter(str_detect(Interaction, "P.longipes")) %>%
  ggplot(aes(x = Estimate, y = Predictor, colour = significant)) +
  geom_point(shape = "circle", size = 1.5) +
  geom_errorbar(aes(xmin = Estimate - StdError, xmax = Estimate + StdError, y = Predictor)) +
  scale_color_manual(values = c("black", "red")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  theme_bw() +
  facet_wrap(vars(Interaction), ncol = 3) +
  labs(x = "Estimate",
       y = "Fire severity",
       colour = "Significant")

# 2.3.1.1 Predator Plot for fire and model1 co-occurence 
## Figure 9: predator fire estimates ####
data_model1 %>%
  filter(!(Predictor %in% c("scale(elevation)", "scale(Avg_mnth_tot_rain)", "scale(flii)"))) %>%
  filter(!str_detect(Interaction, "P.longipes")) %>%
  ggplot(aes(x = Estimate, y = Predictor, colour = significant)) +
  geom_point(shape = "circle", size = 1.5) +
  geom_errorbar(aes(xmin = Estimate - StdError, xmax = Estimate + StdError, y = Predictor)) +
  scale_color_manual(values = c("black", "red")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  theme_bw() +
  facet_wrap(vars(Interaction), ncol = 3) +
  labs(x = "Estimate",
       y = "Fire severity",
       colour = "Significant")

# Plot for fire and co-occupancy resposne on 
prediction_fire <-unique(nd_marg_fire)

pot_all_pred <- predict(Model1, type="state" ,species="P.tridactylus", newdata = prediction_fire)
pot_no_pred <- predict(Model1, type="state" ,species="P.tridactylus", newdata = prediction_fire, cond=c("-cat","-dingo","-fox"))
pot_with_fox <- predict(Model1, type="state" ,species="P.tridactylus", newdata = prediction_fire, cond=c("-cat","-dingo"))
pot_with_dingo <- predict(Model1, type="state" ,species="P.tridactylus", newdata = prediction_fire, cond=c("-cat","-fox"))
pot_with_cat <- predict(Model1, type="state" ,species="P.tridactylus", newdata = prediction_fire, cond=c("-dingo","-fox"))

combined_pred_pot <- rbind(pot_all_pred,pot_no_pred,pot_with_fox,pot_with_dingo,pot_with_cat)

combined_pred_pot$fire <-  rep(c("1. UB","2. L/M", "3. H","4. VH"), 5)

combined_pred_pot$terms <- rep(c("All", "None", "Fox", "Dingo", "Cat"), each=4)

## Figure 6: Predicted Occupancy P.longipes and predators ####
combined_pred_pot %>%
  filter(fire %in% "1. UB") %>%
  ggplot() +
  aes(x = terms, y = Predicted) +
  geom_boxplot(shape = "circle", size = 1.5, colour = "#112446") +
  geom_errorbar(aes(x = terms, ymin = Predicted - SE, ymax = Predicted + SE), width = 0.2, colour = "#112446") +
  theme_minimal() +
  labs(x = "Predator status", y = "Predicted P.longipes occupancy") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

 
# 2.3.1.1 Plot for Rain, elevation and FLII and model1 co-occurence 

### 2.3.1.2. Predictions for Model 1 elevation ####
plong_combine1 <- rbind(predict(Model1, type = "state", species = "P.tridactylus", newdata=nd_marg_elev),
                      predict(Model1, type = "state", species = "P.tridactylus", cond = "-cat", newdata=nd_marg_elev),
                      predict(Model1, type = "state", species = "P.tridactylus", cond = "-fox", newdata=nd_marg_elev),
                      predict(Model1, type = "state", species = "P.tridactylus", cond = "-dingo", newdata=nd_marg_elev))
plong_combine1$species <- "P.longipes"
plong_combine1$condition <- rep(c("original", "-cat", "-fox", "-dingo"), each = 1000)
plong_combine1$elevation <- rep(nd_marg_elev$elevation, 4)

# Predictions for Cat
cat_combine1 <- rbind(predict(Model1, type = "state", species = "cat", newdata=nd_marg_elev),
                      predict(Model1, type = "state", species = "cat", cond = "-P.tridactylus", newdata=nd_marg_elev),
                      predict(Model1, type = "state", species = "cat", cond = "-fox", newdata=nd_marg_elev),
                      predict(Model1, type = "state", species = "cat", cond = "-dingo", newdata=nd_marg_elev))
cat_combine1$species <- "Cat"
cat_combine1$condition <- rep(c("original", "-P.longipes", "-fox", "-dingo"), each = 1000)
cat_combine1$elevation <- rep(nd_marg_elev$elevation, 4)

# Predictions for Dingo
dingo_combine1 <- rbind(predict(Model1, type = "state", species = "dingo", newdata=nd_marg_elev),
                        predict(Model1, type = "state", species = "dingo", cond = "-P.tridactylus", newdata=nd_marg_elev),
                        predict(Model1, type = "state", species = "dingo", cond = "-cat", newdata=nd_marg_elev),
                        predict(Model1, type = "state", species = "dingo", cond = "-fox", newdata=nd_marg_elev))
dingo_combine1$species <- "Dingo"
dingo_combine1$condition <- rep(c("original", "-P.longipes", "-cat", "-fox"), each = 1000)
dingo_combine1$elevation <- rep(nd_marg_elev$elevation, 4)

# Predictions for Fox
fox_combine1 <- rbind(predict(Model1, type = "state", species = "fox", newdata=nd_marg_elev),
                      predict(Model1, type = "state", species = "fox", cond = "-P.tridactylus", newdata=nd_marg_elev),
                      predict(Model1, type = "state", species = "fox", cond = "-cat", newdata=nd_marg_elev),
                      predict(Model1, type = "state", species = "fox", cond = "-dingo", newdata=nd_marg_elev))
fox_combine1$species <- "Fox"

fox_combine1$condition <- rep(c("original", "-P.longipes", "-cat", "-dingo"), each = 1000)
fox_combine1$elevation <- rep(nd_marg_elev$elevation, 4)

m1_elev_df <- rbind(plong_combine1, 
                     cat_combine1, dingo_combine1, fox_combine1)

# # plong_combine1$species <- "Potorous longipes"
# plong_cat_combine1$condition <- rep(c("P.longipes & Cat", "P.longipes", "Cat"), each = 1000)
# plong_cat_combine1$elevation <- rep(nd_marg_elev$elevation, 3)


# Define the function to create plots for each condition
create_plot <- function(species_name, condition_filter, title_suffix, color_values, labels) {
  ggplot(m1_elev_df %>%
           filter(species == species_name, condition %in% condition_filter)) +
    geom_point(size = 0.75, aes(x = elevation, y = Predicted, color = condition)) +
    geom_ribbon(aes(x = elevation, ymin = Predicted - SE, ymax = pmin(Predicted + SE, 1), fill = condition), alpha = 0.2) +
    # scale_alpha_manual(values = c(0.2)) + 
    # theme_bw() + 
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
    # scale_x_discrete(labels = c("Absent", "Present")) +
    labs(y= "Predcited occupancy", title = paste(species_name, title_suffix)) +
    scale_color_manual(values = color_values, 
                       labels = labels) +
    scale_fill_manual(values = color_values, 
                      labels = labels)
}

# Define the color values and labels for each plot
color_values_cat_elv <- c("original" = "black", "-cat" = "red")
labels_cat <- c("Present", "Absent")

color_values_dingo_elv  <- c("original" = "black", "-dingo" = "red")
labels_dingo <- c("Present", "Absent")

color_values_fox_elv  <- c("original" = "black", "-fox" = "red")
labels_fox <- c("Present", "Absent")

color_values_pot_elv  <- c("original" = "black", "-P.longipes" = "red")
labels_pot <- c("Present", "Absent")

pot_x_cat <- create_plot("P.longipes", c("original", "-cat"), "- Cat", color_values_cat_elv , labels_cat)
pot_x_dingo <- create_plot("P.longipes", c("original", "-dingo"), "- Dingo", color_values_dingo_elv , labels_dingo)
pot_x_fox <- create_plot("P.longipes", c("original", "-fox"), "- Fox", color_values_fox_elv , labels_fox)
cat_x_pot <- create_plot("Cat", c("original", "-P.longipes"), "- P.longipes", color_values_pot_elv , labels_pot)
cat_x_dingo <- create_plot("Cat", c("original", "-dingo"), "- Dingo", color_values_dingo_elv , labels_dingo)
cat_x_fox <- create_plot("Cat", c("original", "-fox"), "- Fox", color_values_fox_elv, labels_fox)
dingo_x_pot <- create_plot("Dingo", c("original", "-P.longipes"), "- P.longipes", color_values_pot_elv , labels_pot)
dingo_x_cat <- create_plot("Dingo", c("original", "-cat"), "- Cat", color_values_cat_elv , labels_cat)
dingo_x_fox <- create_plot("Dingo", c("original", "-fox"), "- Fox", color_values_fox_elv , labels_fox)
fox_x_pot <- create_plot("Fox", c("original", "-P.longipes"), "- P.longipes", color_values_pot_elv , labels_pot)
fox_x_cat <- create_plot("Fox", c("original", "-cat"), "- Cat", color_values_cat_elv , labels_cat)
fox_x_dingo <- create_plot("Fox", c("original", "-dingo"), "- Dingo", color_values_dingo_elv , labels_dingo)

#prey
pot_x_cat <- pot_x_cat +
  theme()

pot_x_dingo <- pot_x_dingo +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank())

pot_x_fox <- pot_x_fox +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank())


combined_plot_elv_prey <- plot_grid(
  pot_x_cat, pot_x_dingo, pot_x_fox,
 
  nrow = 1, ncol = 3, axis = "tblr"
)
## Figure 10: mesopredator elveation
plot_grid(
  
   NULL, cat_x_dingo, cat_x_fox,
  dingo_x_cat, NULL, dingo_x_fox,
  fox_x_cat, fox_x_dingo, NULL,
  nrow = 3, ncol = 3, axis = "tblr"
)


### 2.3.1.3. Predictions for Model 1 Rainfall ####
plong_combine1 <- rbind(predict(Model1, type = "state", species = "P.tridactylus", newdata=nd_marg_rain),
                        predict(Model1, type = "state", species = "P.tridactylus", cond = "-cat", newdata=nd_marg_rain),
                        predict(Model1, type = "state", species = "P.tridactylus", cond = "-fox", newdata=nd_marg_rain),
                        predict(Model1, type = "state", species = "P.tridactylus", cond = "-dingo", newdata=nd_marg_rain))
plong_combine1$species <- "P.longipes"
plong_combine1$condition <- rep(c("original", "-cat", "-fox", "-dingo"), each = 1000)
plong_combine1$rainfall<- rep(nd_marg_rain$Avg_mnth_tot_rain, 4)

# Predictions for Cat
cat_combine1 <- rbind(predict(Model1, type = "state", species = "cat", newdata=nd_marg_rain),
                      predict(Model1, type = "state", species = "cat", cond = "-P.tridactylus", newdata=nd_marg_rain),
                      predict(Model1, type = "state", species = "cat", cond = "-fox", newdata=nd_marg_rain),
                      predict(Model1, type = "state", species = "cat", cond = "-dingo", newdata=nd_marg_rain))
cat_combine1$species <- "Cat"
cat_combine1$condition <- rep(c("original", "-P.longipes", "-fox", "-dingo"), each = 1000)
cat_combine1$rainfall <- rep(nd_marg_rain$Avg_mnth_tot_rain, 4)

# Predictions for Dingo
dingo_combine1 <- rbind(predict(Model1, type = "state", species = "dingo", newdata=nd_marg_rain),
                        predict(Model1, type = "state", species = "dingo", cond = "-P.tridactylus", newdata=nd_marg_rain),
                        predict(Model1, type = "state", species = "dingo", cond = "-cat", newdata=nd_marg_rain),
                        predict(Model1, type = "state", species = "dingo", cond = "-fox", newdata=nd_marg_rain))
dingo_combine1$species <- "Dingo"
dingo_combine1$condition <- rep(c("original", "-P.longipes", "-cat", "-fox"), each = 1000)
dingo_combine1$rainfall <- rep(nd_marg_rain$Avg_mnth_tot_rain, 4)

# Predictions for Fox
fox_combine1 <- rbind(predict(Model1, type = "state", species = "fox", newdata=nd_marg_rain),
                      predict(Model1, type = "state", species = "fox", cond = "-P.tridactylus", newdata=nd_marg_rain),
                      predict(Model1, type = "state", species = "fox", cond = "-cat", newdata=nd_marg_rain),
                      predict(Model1, type = "state", species = "fox", cond = "-dingo", newdata=nd_marg_rain))
fox_combine1$species <- "Fox"

fox_combine1$condition <- rep(c("original", "-P.longipes", "-cat", "-dingo"), each = 1000)
fox_combine1$rainfall <- rep(nd_marg_rain$Avg_mnth_tot_rain, 4)

m1_rain_df <- rbind(plong_combine1, 
                    cat_combine1, dingo_combine1, fox_combine1)


# Define the function to create plots for each condition
create_plot_rain <- function(species_name, condition_filter, title_suffix, color_values, labels) {
  ggplot(m1_rain_df %>%
           filter(species == species_name, condition %in% condition_filter)) +
    geom_point(size = 0.75, aes(x = rainfall, y = Predicted, color = condition)) +
    geom_ribbon(aes(x = rainfall, ymin = Predicted - SE, ymax = pmin(Predicted + SE, 1), fill = condition), alpha = 0.2) +
    # scale_alpha_manual(values = c(0.2)) + 
    # theme_bw() + 
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
    # scale_x_discrete(labels = c("Absent", "Present")) +
    labs(y= "Predcited occupancy",title = paste(species_name, title_suffix)) +
    scale_color_manual(values = color_values, 
                       labels = labels) +
    scale_fill_manual(values = color_values, 
                      labels = labels)
}




# Define the color values and labels for each plot
color_values_cat_rain <- c("original" = "black", "-cat" = "blue")
labels_cat <- c("Present", "Absent")

color_values_dingo_rain <- c("original" = "black", "-dingo" = "blue")
labels_dingo <- c("Present", "Absent")

color_values_fox_rain <- c("original" = "black", "-fox" = "blue")
labels_fox <- c("Present", "Absent")

color_values_pot_rain <- c("original" = "black", "-P.longipes" = "blue")
labels_pot <- c("Present", "Absent")

pot_x_cat_rain <- create_plot_rain("P.longipes", c("original", "-cat"), "- Cat", color_values_cat_rain, labels_cat)
pot_x_dingo_rain <- create_plot_rain("P.longipes", c("original", "-dingo"), "- Dingo", color_values_dingo_rain, labels_dingo)
pot_x_fox_rain <- create_plot_rain("P.longipes", c("original", "-fox"), "- Fox", color_values_fox_rain, labels_fox)
cat_x_pot <- create_plot_rain("Cat", c("original", "-P.longipes"), "- P.longipes", color_values_pot_rain, labels_pot)
cat_x_dingo <- create_plot_rain("Cat", c("original", "-dingo"), "- Dingo", color_values_dingo_rain, labels_dingo)
cat_x_fox <- create_plot_rain("Cat", c("original", "-fox"), "- Fox", color_values_fox_rain, labels_fox)
dingo_x_pot <- create_plot_rain("Dingo", c("original", "-P.longipes"), "- P.longipes", color_values_pot_rain, labels_pot)
dingo_x_cat <- create_plot_rain("Dingo", c("original", "-cat"), "- Cat", color_values_cat_rain, labels_cat)
dingo_x_fox <- create_plot_rain("Dingo", c("original", "-fox"), "- Fox", color_values_fox_rain, labels_fox)
fox_x_pot <- create_plot_rain("Fox", c("original", "-P.longipes"), "- P.longipes", color_values_pot_rain, labels_pot)
fox_x_cat <- create_plot_rain("Fox", c("original", "-cat"), "- Cat", color_values_cat_rain, labels_cat)
fox_x_dingo <- create_plot_rain("Fox", c("original", "-dingo"), "- Dingo", color_values_dingo_rain, labels_dingo)

#prey
pot_x_cat_rain <- pot_x_cat_rain +
  theme()

pot_x_dingo_rain <- pot_x_dingo_rain +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank())

pot_x_fox_rain <- pot_x_fox_rain +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank())



## Figure 11: mesporator plot with rainfall ####
combined_plot_rain_pred <- plot_grid(
  
  NULL, cat_x_dingo, cat_x_fox,
  dingo_x_cat, NULL, dingo_x_fox,
  fox_x_cat, fox_x_dingo, NULL,
  nrow = 3, ncol = 3, axis = "tblr"
)





## Figure 7: Potoroos wit the effect of predators for elevation and rain ####

#plot for figure 8 This figure presents conditional occupancy plots demonstrating the effect of 
#elevation and rainfall on conditional occupancy of P.longipes when predators (cat, dingo, fox) are 
#present and absent. The error bars and bands represent 95% confidence intervals. 
#Based on camera-trapping surveys in East Gippsland and predicted by the P. longipes model.
combined_plot_rain_prey <- plot_grid(
  pot_x_cat, pot_x_dingo, pot_x_fox,
  pot_x_cat_rain, pot_x_dingo_rain, pot_x_fox_rain,
  nrow = 2, ncol = 3, align = 'v', axis = 'tblr',
  rel_widths = c(0.1, 0.1, 0.1), rel_heights = c(1, 1)
)


### 2.3.1.4. Predictions for Model 1 flii ####
plong_combine1 <- rbind(predict(Model1, type = "state", species = "P.tridactylus", newdata=nd_marg_flii),
                        predict(Model1, type = "state", species = "P.tridactylus", cond = "-cat", newdata=nd_marg_flii))

plong_combine1$species <- "P.longipes"
plong_combine1$condition <- rep(c("original", "-cat"), each = 1000)
plong_combine1$flii<- rep(nd_marg_flii$flii, 2)

# Predictions for Cat
cat_combine1 <- rbind(predict(Model1, type = "state", species = "cat", newdata=nd_marg_flii),
                      predict(Model1, type = "state", species = "cat", cond = "-P.tridactylus", newdata=nd_marg_flii),
                      predict(Model1, type = "state", species = "cat", cond = "-fox", newdata=nd_marg_flii),
                      predict(Model1, type = "state", species = "cat", cond = "-dingo", newdata=nd_marg_flii))
cat_combine1$species <- "Cat"
cat_combine1$condition <- rep(c("original", "-P.longipes", "-fox", "-dingo"), each = 1000)
cat_combine1$flii <- rep(nd_marg_flii$flii, 4)

# Predictions for Dingo
dingo_combine1 <- rbind(predict(Model1, type = "state", species = "dingo", newdata=nd_marg_flii),
                       predict(Model1, type = "state", species = "dingo", cond = "-cat", newdata=nd_marg_flii))

dingo_combine1$species <- "Dingo"
dingo_combine1$condition <- rep(c("original",  "-cat"), each = 1000)
dingo_combine1$flii <- rep(nd_marg_flii$flii, 2)

# Predictions for Fox
fox_combine1 <- rbind(predict(Model1, type = "state", species = "fox", newdata=nd_marg_flii),
                      predict(Model1, type = "state", species = "fox", cond = "-cat", newdata=nd_marg_flii))
                 
fox_combine1$species <- "Fox"

fox_combine1$condition <- rep(c("original", "-cat"), each = 1000)
fox_combine1$flii <- rep(nd_marg_flii$flii, 2)

m1_flii_df <- rbind(plong_combine1, 
                    cat_combine1, dingo_combine1, fox_combine1)


# Define the function to create plots for each condition
create_plot_flii <- function(species_name, condition_filter, title_suffix, color_values, labels) {
  ggplot(m1_flii_df %>%
           filter(species == species_name, condition %in% condition_filter)) +
    geom_point(size = 0.75, aes(x = flii, y = Predicted, color = condition)) +
    geom_ribbon(aes(x = flii, ymin = Predicted - SE, ymax = pmin(Predicted + SE, 1), fill = condition), alpha = 0.2) +
    # scale_alpha_manual(values = c(0.2)) + 
    # theme_bw() + 
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
    # scale_x_discrete(labels = c("Absent", "Present")) +
    labs(x= "FLII", y = "Predicted occupancy", title = paste(species_name, title_suffix)) +
    scale_color_manual(values = color_values, 
                       labels = labels) +
    scale_fill_manual(values = color_values, 
                      labels = labels)
}

# Define the color values and labels for each plot
color_values_cat <- c("original" = "black", "-cat" = "brown")
labels_cat <- c("Present", "Absent")

color_values_dingo <- c("original" = "black", "-dingo" = "brown")
labels_dingo <- c("Present", "Absent")

color_values_fox <- c("original" = "black", "-fox" = "brown")
labels_fox <- c("Present", "Absent")

color_values_pot <- c("original" = "black", "-P.longipes" = "brown")
labels_pot <- c("Present", "Absent")



cat_x_pot <- create_plot_flii("Cat", c("original", "-P.longipes"), "- P.longipes", color_values_pot, labels_pot)
cat_x_dingo <- create_plot_flii("Cat", c("original", "-dingo"), "- Dingo", color_values_dingo, labels_dingo)
cat_x_fox <- create_plot_flii("Cat", c("original", "-fox"), "- Fox", color_values_fox, labels_fox)


## Figure 12: FLII cat v dingo, cat v fox ####
combined_plot_flii <- plot_grid(
  
  cat_x_dingo, cat_x_fox,

  nrow = 1, ncol = 2, axis = "tblr"
)



### 2.3.2. Predictions for Model 2 ####


co_oc_ndat <- site_cov[50,]
psp_combine2 <- rbind(predict(Model2, type = "state", species = "P.sp",newdata=co_oc_ndat),
                        predict(Model2, type = "state", species = "P.sp", cond = "-cat", newdata=co_oc_ndat),
                        predict(Model2, type = "state", species = "P.sp", cond = "-fox", newdata=co_oc_ndat),
                        predict(Model2, type = "state", species = "P.sp", cond = "-dingo", newdata=co_oc_ndat))
psp_combine2$species <- "Potorous sp."
psp_combine2$condition <- rep(c("original", "-cat", "-fox", "-dingo"), each = 1)
psp_combine2$model <- "Model 2"

# Predictions for Cat
cat_combine2 <- rbind(predict(Model2, type = "state", species = "cat", newdata=co_oc_ndat),
                      predict(Model2, type = "state", species = "cat", cond = "-P.sp", newdata=co_oc_ndat),
                      predict(Model2, type = "state", species = "cat", cond = "-fox", newdata=co_oc_ndat),
                      predict(Model2, type = "state", species = "cat", cond = "-dingo", newdata=co_oc_ndat))
cat_combine2$species <- "Cat"
cat_combine2$condition <- rep(c("original", "-P.sp", "-fox", "-dingo"), each = 1)
cat_combine2$model <- "Model 2"

# Predictions for Dingo
dingo_combine2 <- rbind(predict(Model2, type = "state", species = "dingo", newdata=co_oc_ndat),
                        predict(Model2, type = "state", species = "dingo", cond = "-P.sp", newdata=co_oc_ndat),
                        predict(Model2, type = "state", species = "dingo", cond = "-cat", newdata=co_oc_ndat),
                        predict(Model2, type = "state", species = "dingo", cond = "-fox", newdata=co_oc_ndat))
dingo_combine2$species <- "Dingo"
dingo_combine2$condition <- rep(c("original", "-P.sp", "-cat", "-fox"), each = 1)
dingo_combine2$model <- "Model 2"

# Predictions for Fox
fox_combine2 <- rbind(predict(Model2, type = "state", species = "fox", newdata=co_oc_ndat),
                     predict(Model2, type = "state", species = "fox", cond = "-P.sp", newdata=co_oc_ndat),
                     predict(Model2, type = "state", species = "fox", cond = "-cat", newdata=co_oc_ndat),
                     predict(Model2, type = "state", species = "fox", cond = "-dingo", newdata=co_oc_ndat))
fox_combine2$species <- "Fox"

fox_combine2$condition <- rep(c("original", "-P.sp", "-cat", "-dingo"), each = 1)
fox_combine2$model <- "Model 2"


# Combine all data frames


combined_df <- rbind(psp_combine2, 
                      cat_combine2, dingo_combine2, fox_combine2)



pot_x_cat <- ggplot(combined_df %>%
  filter(species == "Potorous sp.",model == "Model 2", condition %in% c("original", "-cat"))) +
  geom_point(size = 2, aes(x = condition, y = Predicted, color = model, fill = model))+
  geom_errorbar(aes(x= condition, ymin=lower, ymax=upper))+
  # geom_errorbar(aes(x= condition, ymin=lower, upper))+
  # scale_fill_manual(values = c("lightblue")) +
  # scale_color_manual(values = c("red","black")) +
  scale_alpha_manual(values = c(0.2)) +# Reversed colors
  # facet_wrap(vars(species), nrow = 1)+
  theme_bw() +  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = c("Absent", "Present"))+
  labs(x = element_blank(), y = "Conditional occupancy", title="Potorous sp-Cat")+
  ylim(0,0.75) 

pot_x_dingo <- ggplot(combined_df %>%
  filter(species == "Potorous sp.", condition %in% c("original", "-dingo"))) +
  geom_point(size = 2, aes(x = condition, y = Predicted, color = model, fill = model))+
  geom_errorbar(aes(x= condition, ymin=lower, ymax=upper))+
  # scale_fill_manual(values = c("lightblue")) +
  # scale_color_manual(values = c("red","black")) +
  scale_alpha_manual(values = c(0.2)) +# Reversed colors
  # facet_wrap(vars(species), nrow = 1)+
  theme_bw() +  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = c("Absent", "Present"))+
  labs(x = element_blank(), y = element_blank(), title="Potorous sp-Dingo")+
  ylim(0,0.75) 

pot_x_fox <- ggplot(combined_df %>%
  filter(species == "Potorous sp." & condition %in% c("original", "-fox"))) +
  geom_point(size = 2, aes(x = condition, y = Predicted, color = model, fill = model))+
  geom_errorbar(aes(x= condition, ymin=lower, ymax=upper))+
  # scale_fill_manual(values = c("lightblue")) +
  # scale_color_manual(values = c("red","black")) +
  scale_alpha_manual(values = c(0.2)) +# Reversed colors
  # facet_wrap(vars(species), nrow = 1)+
  theme_bw() +  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = c("Absent", "Present"))+
  labs(x = element_blank(), y = element_blank(), title="Potorous sp-Fox")+
  ylim(0,0.75) 

cat_x_pot <- ggplot(combined_df %>%
                      filter(species == "Cat" & condition %in% c("original",  "-P.sp"))) +
  geom_point(size = 2, aes(x = condition, y = Predicted, color = model, fill = model))+
  geom_errorbar(aes(x= condition, ymin=lower, ymax=upper))+
  # scale_fill_manual(values = c("lightblue")) +
  # scale_color_manual(values = c("red","black")) +
  scale_alpha_manual(values = c(0.2)) +# Reversed colors
  # facet_wrap(vars(species), nrow = 1)+
  theme_bw() +  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = c("Absent", "Present"))+
  labs(x = element_blank(), y = "Conditional occupancy", title="Cat-Potorous sp")+
  ylim(0.4,1) 

cat_x_dingo <- ggplot(combined_df %>%
                      filter(species == "Cat" & condition %in% c("original", "-dingo"))) +
  geom_point(size = 2, aes(x = condition, y = Predicted, color = model, fill = model))+
  geom_errorbar(aes(x= condition, ymin=lower, ymax=upper))+
  # scale_fill_manual(values = c("lightblue")) +
  # scale_color_manual(values = c("red","black")) +
  scale_alpha_manual(values = c(0.2)) +# Reversed colors
  # facet_wrap(vars(species), nrow = 1)+
  theme_bw() +  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = c("Absent", "Present"))+
  labs(x = element_blank(), y = element_blank(),title="Cat-Dingo")+
  ylim(0.4,1)

cat_x_fox <- ggplot(combined_df %>%
                        filter(species == "Cat" & condition %in% c("original", "-fox"))) +
  geom_point(size = 2, aes(x = condition, y = Predicted, color = model, fill = model))+
  geom_errorbar(aes(x= condition, ymin=lower, ymax=upper))+
  # scale_fill_manual(values = c("lightblue")) +
  # scale_color_manual(values = c("red","black")) +
  scale_alpha_manual(values = c(0.2)) +# Reversed colors
  # facet_wrap(vars(species), nrow = 1)+
  theme_bw() +  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = c("Absent", "Present"))+
  labs(x = element_blank(), y = element_blank(), title="Cat-Fox")+
  ylim(0.4,1)

dingo_x_pot <- ggplot(combined_df %>%
                      filter(species == "Dingo" & condition %in% c("original", "-P.sp"))) +
  geom_point(size = 2, aes(x = condition, y = Predicted, color = model, fill = model))+
  geom_errorbar(aes(x= condition, ymin=lower, ymax=upper))+
  # scale_fill_manual(values = c("lightblue")) +
  # scale_color_manual(values = c("red","black")) +
  scale_alpha_manual(values = c(0.2)) +# Reversed colors
  # facet_wrap(vars(species), nrow = 1)+
  theme_bw() +  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = c("Absent", "Present"))+
  labs(x = element_blank(), y = "Conditional occupancy", title="Dingo-Potorous sp")+
  ylim(0,1) 

dingo_x_cat <- ggplot(combined_df %>%
                        filter(species == "Dingo" & condition %in% c("original", "-cat"))) +
  geom_point(size = 2, aes(x = condition, y = Predicted, color = model, fill = model))+
  geom_errorbar(aes(x= condition, ymin=lower, ymax=upper))+
  # scale_fill_manual(values = c("lightblue")) +
  # scale_color_manual(values = c("red","black")) +
  scale_alpha_manual(values = c(0.2)) +# Reversed colors
  # facet_wrap(vars(species), nrow = 1)+
  theme_bw() +  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = c("Absent", "Present"))+
  labs(x = element_blank(), y = "Conditional occupancy", title="Dingo-Cat")+
  ylim(0,1)

dingo_x_fox <- ggplot(combined_df %>%
                      filter(species == "Dingo" & condition %in% c("original", "-fox"))) +
  geom_point(size = 2, aes(x = condition, y = Predicted, color = model, fill = model))+
  geom_errorbar(aes(x= condition, ymin=lower, ymax=upper))+
  # scale_fill_manual(values = c("lightblue")) +
  # scale_color_manual(values = c("red","black")) +
  scale_alpha_manual(values = c(0.2)) +# Reversed colors
  # facet_wrap(vars(species), nrow = 1)+
  theme_bw() +  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = c("Absent", "Present"))+
  labs(x = element_blank(), y = element_blank(), title="Dingo-Fox")+
  ylim(0,1) 

fox_x_pot <- ggplot(combined_df %>%
                        filter(species == "Fox" & condition %in% c("original", "-P.sp"))) +
  geom_point(size = 2, aes(x = condition, y = Predicted, color = model, fill = model))+
  geom_errorbar(aes(x= condition, ymin=lower, ymax=upper))+
  # scale_fill_manual(values = c("lightblue")) +
  # scale_color_manual(values = c("red","black")) +
  scale_alpha_manual(values = c(0.2)) +# Reversed colors
  # facet_wrap(vars(species), nrow = 1)+
  theme_bw() +  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = c("Absent", "Present"))+
  labs(x = element_blank(), y = "Conditional occupancy", title="Fox-Potorous sp")+
  ylim(0,1) 

fox_x_cat <- ggplot(combined_df %>%
                        filter(species == "Fox" & condition %in% c("original", "-cat"))) +
  geom_point(size = 2, aes(x = condition, y = Predicted, color = model, fill = model))+
  geom_errorbar(aes(x= condition, ymin=lower, ymax=upper))+
  # scale_fill_manual(values = c("lightblue")) +
  # scale_color_manual(values = c("red","black")) +
  scale_alpha_manual(values = c(0.2)) +# Reversed colors
  # facet_wrap(vars(species), nrow = 1)+
  theme_bw() +  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = c("Absent", "Present"))+
  labs(x = element_blank(), y = element_blank(), title="Fox-Cat")+
  ylim(0,1)

fox_x_dingo <- ggplot(combined_df %>%
                        filter(species == "Fox" & condition %in% c("original", "-dingo"))) +
  geom_point(size = 2, aes(x = condition, y = Predicted, color = model, fill = model))+
  geom_errorbar(aes(x= condition, ymin=lower, ymax=upper))+
  # scale_fill_manual(values = c("lightblue")) +
  # scale_color_manual(values = c("red","black")) +
  scale_alpha_manual(values = c(0.2)) +# Reversed colors
  # facet_wrap(vars(species), nrow = 1)+
  theme_bw() +  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = c("Absent", "Present"))+
  labs(x = element_blank(), y = element_blank(), title="Fox-Dingo")+
  ylim(0,1)



## Figure 8: Plot mesopredatorco-occurence model 2 ####
plot_grid(
  
  NULL, cat_x_dingo, cat_x_fox,
  dingo_x_cat, NULL, dingo_x_fox,
  fox_x_cat, fox_x_dingo, NULL,
  # labels = c("Fire cat", "", "Elevation cat", "", "Rain cat"),
  nrow = 3, ncol = 3,   axis = "tblr"
)


# 
# # Your original data
# estimates <- Model2@estimates@estimates[["state"]]@estimates
# 
# # Filter out descriptions without ":"
# filtered_estimates <- estimates[grep(":", names(estimates))]
# 
# plot(filtered_estimates)

##2.4. Plots for maps ####

#East Gippsland region
#bounding box to reduce size and process time for rasters
E_Gips_box <- data.frame(
  xmin = 148,
  xmax = 150,
  ymin = -38,
  ymax = -37
)

plot(E_Gips_box)

#get raster from predator transect notes

directory <- "Input_Data"

#### directory where data is saved once complete ###
data_out <- "transformed_data"

NDVI_path <- "Input_Data/NDVI_data"

### 1.3.2 Elevation ####
elevation_raster <- raster(paste(directory,"dem-9s/dem-9s.tif", sep = "/"))  

elevation_trim<- crop(elevation_raster , extent(E_Gips_box$xmin, E_Gips_box$xmax, E_Gips_box$ymin, E_Gips_box$ymax))

plot(elevation_trim)

New_df <- as.data.frame(elevation_trim, xy = TRUE)

colnames(New_df) <- c("long", "lat", "elevation")

coordinates <- cbind(New_df$long,New_df$lat)

### 1.3.3 Forest Landscape Integrity Index (FLII) ####

flii_raster <- raster(paste(directory,"intactness/flii_Oceania.tif", sep = "/")) 
flii_trim<- crop(flii_raster, extent(E_Gips_box$xmin, E_Gips_box$xmax, E_Gips_box$ymin, E_Gips_box$ymax))

length(flii_trim)

New_df$flii <- 
  extract(flii_trim, coordinates)/1000


plot(flii_trim/1000)

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
coordinates <- cbind(New_df$long, New_df$lat)

rain_data <- extract_rain_data(directory, nc_rain_files, coordinates)

New_df$Avg_mnth_rain <- rowMeans(rain_data)


### 1.3.5 Mean Monthly NDVI 2021-2022 ####

NDVI_path <- "Input_Data/NDVI_data"

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
coordinates <- cbind(New_df$long, New_df$lat)

# Extract NDVI data THis takes a long time. Load the rdata file to save time
ndvi_data <- extract_ndvi_data(NDVI_path, ndvi_files, coordinates)


# Calculate average monthly NDVI for each site
New_df$Avg_mnth_ndvi <- rowMeans(ndvi_data)


### 1.3.1 Black Summer Fire severity category ####

#extract fire severity for each deployment ID

# extract tire from raster set

Fire_severity_raster <- raster(paste(directory,"AUS_GEEBAM_Fire_Severity_NIAFED20200224/AUS_GEEBAM_Fire_Severity_NIAFED20200224.tif", sep = "/"))  # Replace "path/to/your/raster/file.tif" with the actual path to your raster file

fire_trim<- crop(Fire_severity_raster, extent(E_Gips_box$xmin, E_Gips_box$xmax, E_Gips_box$ymin, E_Gips_box$ymax))


plot(fire_trim)# 
New_df$fire_sev_rast<- extract(Fire_severity_raster, coordinates)
# 
New_df$fire_sev_rast <- Fire_severity_raster@data@attributes[[1]][["GEEBAM"]][New_df$fire_sev_rast]
# 
New_df$fire_sev_rast[is.na(New_df$fire_sev_rast)] <- "2 - Unburnt"
# 

New_df$fire_sev_rast[New_df$fire_sev_rast == "1 - No Data"] <- "UB"
New_df$fire_sev_rast[New_df$fire_sev_rast == "2 - Unburnt"] <- "UB"
New_df$fire_sev_rast[New_df$fire_sev_rast == "3 - Low and Moderate"] <- "L/M"
New_df$fire_sev_rast[New_df$fire_sev_rast == "4 - High"] <- "H"
New_df$fire_sev_rast[New_df$fire_sev_rast == "5 - Very High"] <- "VH"

colnames(New_df) <- c("longitude", "latitude","elevation" ,"flii","Avg_mnth_tot_rain","Avg_mnth_ndvi","fire_sev_infield")

New_df$flii <- ifelse(New_df$flii <0 , 0, New_df$flii)

#write New_df to file to avoid rerunning

saveRDS(New_df, paste(directory, "/Region_covariates.Rdata", sep = ""))

#load in New DF so it doesnt have to be recalculated
New_df <- readRDS(paste(directory, "/Region_covariates.Rdata", sep = ""))

tic()
cat_1 <- predict(Model1 , type = "state", species = "cat", newdata = New_df, condition=c("-dingo", "-fox", "-P.treidactylus"))
fox_1 <- predict(Model1 , type = "state", species = "fox", newdata = New_df)
dingo_1 <- predict(Model1 , type = "state", species = "dingo", newdata = New_df)
pot_trid_1 <- predict(Model1, type = "state", species = "P.tridactylus", newdata = New_df)
toc()



tic()
cat_2 <- predict(Model2, type = "state", species = "cat", newdata = New_df)
fox_2 <- predict(Model2, type = "state", species = "fox", newdata = New_df)
dingo_2 <- predict(Model2, type = "state", species = "dingo", newdata = New_df)
pot_sp_2 <- predict(Model2, type = "state", species = "P.sp", newdata = New_df)
toc()

summary(dingo_1$lower)

cat_1$lat <- New_df$latitude
cat_1$long <- New_df$longitude

fox_1$lat <- New_df$latitude
fox_1$long <- New_df$longitude

dingo_1$lat <- New_df$latitude
dingo_1$long <- New_df$longitude

pot_trid_1$lat <- New_df$latitude
pot_trid_1$long <- New_df$longitude

cat_2$lat <- New_df$latitude
cat_2$long <- New_df$longitude

fox_2$lat <- New_df$latitude
fox_2$long <- New_df$longitude

dingo_2$lat <- New_df$latitude
dingo_2$long <- New_df$longitude

pot_sp_2$lat <- New_df$latitude
pot_sp_2$long <- New_df$longitude


custom_colors <- #c("#8E0152", "#C51B7D", "#DE77AE", "#F1B6DA", "#FDE0EF", 
#                    "#E6F5D0", "#B8E186", "#7FBC41", "#4D9221", "#276419")

  #c("#1c1612","#E6A1C", "#FF7F00", "#FDBF6F","#FFFF99"
   # ,"#B2DF8A","#33A02C")

c("#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF",  "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2")



pot_species <- st_read("Input_Data/Species_of_National_Environmental_Significance_and_selected_marine_and_cetacean_species/Species_of_National_Environmental_Significance_and_selected_marine_and_cetacean_species.shp")

pot_species_combined <- pot_species %>%
  filter(LISTED_TAX %in% c(86367, 217), PRESENCE_R == 2) %>%
  mutate(species_label = case_when(
    LISTED_TAX == 86367 ~ "P.tridactylus",
    LISTED_TAX == 217 ~ "P.longipes"
  ))

saveRDS(pot_species_combined, paste(directory,"Range_Potoroos.Rdata", sep = "/"))

# read in file:
pot_species_combined <- readRDS(paste(directory,"Range_Potoroos.Rdata", sep = "/"))

# Create the first plot
pot_1_plot <- 
  ggplot(data = pot_trid_1) +
  geom_tile(aes(x = long, y = lat, fill = Predicted)) +
  scale_fill_gradientn(colors = custom_colors, limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  ggtitle("a) Model 1: Potorous longipes") +  # Add title
  
  # Include spatial elements in the legend
  geom_sf(data = pot_species_combined, aes(color = species_label), fill = NA, linewidth = 2.5, show.legend = TRUE) +
  
  scale_color_manual(name = "Species distribution", values = c("P.tridactylus" = "black", "P.longipes" = "blue")) +
  geom_sf(data = pot_species_combined, color = "white", fill = NA, linewidth = 1, show.legend = FALSE) +
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
  coord_sf(xlim = c(E_Gips_box$xmin + 0.08, E_Gips_box$xmax - 0.085),
           ylim = c(E_Gips_box$ymin+0.15, E_Gips_box$ymax - 0.05))

# Create the second plot
pot_2_plot <-  ggplot(data = pot_sp_2) +
  geom_tile(aes(x = long, y = lat, fill = Predicted)) +
  scale_fill_gradientn(colors = custom_colors, limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  ggtitle("b) Model 2: Potorous sp.") +  # Add title
  
  # Include spatial elements in the legend
  geom_sf(data = pot_species_combined, aes(color = species_label), fill = NA, linewidth = 2.5, show.legend = TRUE) +
  
  scale_color_manual(name = "Species distribution", values = c("P.tridactylus" = "black", "P.longipes" = "blue")) +
  geom_sf(data = pot_species_combined, color = "white", fill = NA, linewidth = 1, show.legend = FALSE) +
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
  coord_sf(xlim = c(E_Gips_box$xmin + 0.08, E_Gips_box$xmax - 0.085),
           ylim = c(E_Gips_box$ymin+0.15, E_Gips_box$ymax - 0.05))


## Figure 15: Potroos predicted regional occupancy ####
pot_1_plot / pot_2_plot + plot_layout(guides = "collect") & theme(legend.position = "bottom")

# Create individual plots
plot_fire <- ggplot(pot_trid_1, aes(x = New_df$fire_sev_infield, y = Predicted)) +
  geom_boxplot(aes(color=Predicted)) +

  labs(title = "Predicted vs Fire", x = "Fire", y = "Predicted")

plot_elevation <- ggplot(pot_trid_1, aes(x = New_df$elevation, y = Predicted)) +
  geom_point(aes(color=Predicted)) +

  labs(title = "Predicted vs Elevation", x = "Elevation", y = "Predicted")

# plot_flii <- ggplot(pot_sp_1, aes(x = New_df$flii, y = Predicted)) +
#   geom_point(aes(color=Predicted)) +
# 
#   labs(title = "Predicted vs FLII", x = "FLII", y = "Predicted")

plot_rain <- ggplot(pot_trid_1, aes(x = New_df$Avg_mnth_tot_rain, y = Predicted)) +
  geom_point(aes(color=Predicted)) +
  
  labs(title = "Predicted vs NDVI", x = "NDVI", y = "Predicted")

plot_ndvi <- ggplot(pot_trid_1, aes(x = New_df$Avg_mnth_ndvi, y = Predicted)) +
  geom_point(aes(color=Predicted)) +
 
  labs(title = "Predicted vs NDVI", x = "NDVI", y = "Predicted")

# Arrange the plots in a single column
library(gridExtra)

grid.arrange(plot_fire, plot_elevation, plot_rain, plot_ndvi, ncol = 1)


# Create the first plot
cat_1_plot <- ggplot(data = cat_1) +
  aes(x = long, y = lat, fill = Predicted) +
  geom_tile() +
  scale_fill_gradientn(colors = custom_colors, limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  ggtitle("Model 1: Cat") +  # Add title
  theme_void()+ theme(plot.title = element_text(hjust = 0.5))


# Create the second plot
cat_2_plot <- ggplot(data = cat_2) +
  aes(x = long, y = lat, fill = Predicted) +
  geom_tile() +
  scale_fill_gradientn(colors = custom_colors, limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  ggtitle("Model 2: Cat") +  # Add title
  theme_void()+ theme(plot.title = element_text(hjust = 0.5))

# Create the first plot
fox_1_plot <- ggplot(data = fox_1) +
  aes(x = long, y = lat, fill = Predicted) +
  geom_tile() +
  scale_fill_gradientn(colors = custom_colors, limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  ggtitle("Model 1: Fox") +  # Add title
  theme_void()+ theme(plot.title = element_text(hjust = 0.5))

# Create the second plot
fox_2_plot <- ggplot(data = fox_2) +
  aes(x = long, y = lat, fill = Predicted) +
  geom_tile() +
  scale_fill_gradientn(colors = custom_colors, limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  ggtitle("Model 2: Fox") +  # Add title
  theme_void()+ theme(plot.title = element_text(hjust = 0.5))

# Create the first plot
dingo_1_plot <- ggplot(data = dingo_1) +
  aes(x = long, y = lat, fill = Predicted) +
  geom_tile() +
  scale_fill_gradientn(colors = custom_colors, limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  ggtitle("Model 1: Dingo") +  # Add title
  theme_void()+ theme(plot.title = element_text(hjust = 0.5))

# Create the second plot
dingo_2_plot <- ggplot(data = dingo_2) +
  aes(x = long, y = lat, fill = Predicted) +
  geom_tile() +
  scale_fill_gradientn(colors = custom_colors, limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  ggtitle("Model 2: Dingo") +  # Add title
  theme_void()+ theme(plot.title = element_text(hjust = 0.5))


## Figure 16: Regional occupancy estimates predators (cats, dingoes, foxes) ####
combined_plot <- plot_grid(
  cat_1_plot, cat_2_plot, fox_1_plot, fox_2_plot,
  dingo_1_plot, dingo_2_plot,
  ncol = 2,
  align = "hv"
)

predict(Model1, type= "state", species= c("cat", "dingo"))

head(New_df)


# Plot raster variables data for methods 

# Plot 1
plot1 <- ggplot(data = New_df %>% filter(elevation >= 0)) +
  aes(x = longitude, y = latitude, fill = elevation) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "blue") +
  geom_point(data = site_cov, mapping = aes(x = longitude, y = latitude)) +
  theme(legend.position = "bottom") +
  labs(fill = "Elevation (m)")

# Plot 2
plot2 <- ggplot(data = New_df %>% filter(elevation > 0)) +
  aes(x = longitude, y = latitude, fill = flii) +
  geom_tile() +
  scale_fill_gradient(low = "darkred", high = "lightgreen") +
  geom_point(data = site_cov, mapping = aes(x = longitude, y = latitude)) +
  theme(legend.position = "bottom") +
  labs(fill = "FLII")

# Plot 3
plot3 <- ggplot(data = New_df %>% filter(elevation > 0)) +
  aes(x = longitude, y = latitude, fill = Avg_mnth_tot_rain) +
  geom_tile() +
  geom_point(data = site_cov, mapping = aes(x = longitude, y = latitude)) +
  theme(legend.position = "bottom") +
  labs(fill = "Mean monthly rain (mm)")

# Plot 4
plot4 <- ggplot(data = New_df %>% filter(elevation >= 0)) +
  aes(x = longitude, y = latitude, fill = Avg_mnth_ndvi/1000) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  geom_point(data = site_cov, mapping = aes(x = longitude, y = latitude)) +
  theme(legend.position = "bottom") +
  labs(fill = "Mean monthly NDVI")


my_colors <- c("darkorange", "gold", "darkgreen", "darkred")
# Plot 5
plot5 <-  ggplot(data = New_df %>% filter(elevation > 0)) +
  aes(x = longitude, y = latitude, fill = factor(fire_sev_infield)) +
  geom_tile() +
  scale_fill_manual(values = my_colors) +
  geom_point(data = site_cov, mapping = aes(x = longitude, y = latitude)) +
  theme(legend.position = "bottom") +
  labs(fill = "Black Summer fire severity")

plots <- list(plot1, plot2, plot3, plot4, plot5)

# Combine plots into a single plot object
combined_plot <- wrap_plots(plots, ncol = 2)


# Modify each plot to remove redundant axis titles
plot1 <- plot1 + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
plot2 <- plot2 + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
plot3 <- plot3 + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
plot4 <- plot4 + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
plot5 <- plot5 + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

# Arrange plots into 3 columns
plots <- list(plot1, plot2, plot3, plot4, plot5)

common_xlab <- "Longitude"
common_ylab <- "Latitude"
# Combine plots into a single plot object
combined_plot <- wrap_plots(plots, ncol = 2)

## Figure 3: Covariates regional ####
combined_plot <- combined_plot + 
  plot_layout(guides = "keep") +
  xlab(common_xlab) +
  ylab(common_ylab)

