#Author: Justin Moore

#Project: Fire, Potoroos and Predators

# Data Processing script: Create input matrices for unmarkedFrameOccuMulti object for model fitting ####

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


## Directory setup ####
directory <- "transformed_data"

#Data formating ####

#load presence absence data for each species

# Get the list of files in the directory
files <- list.files(directory)

# Loop through each file
for(file in files) {
  # Check if the file contains "presence-absence"
  if(grepl("presence-absence", file)) {
    # Extract the species name from the file name
    species <- gsub("_presence-absence_matrix.csv", "", gsub("daily_", "", file))
    # Read the file into a variable with the species name
    assign(species, as.matrix(read.csv(paste(directory, file, sep = "/"))[,-1]))
  }
}


starts <- seq(1,371, 7)

col1 <- vector(length=100)

week_function <- function(mat){

weekly_matrix <- data.frame(matrix(0, ncol=ncol(mat)/7, nrow = nrow(mat)))

for (j in 1:(ncol(mat)/7)) {

  start <- starts[j]
  end <- j*7
  
  for (i in 1:nrow(mat)) {
    if (any(mat[i, start:end] == 1, na.rm = TRUE)) {
      result <- 1
    } else if (any(mat[i, start:end] == 0, na.rm = TRUE)) {
      result <- 0
    } else {
      result <- NA
    }
    col1[i] <- result
  
  }
  
  # Move this assignment outside the inner loop
  weekly_matrix[, j] <- col1
}
 return(weekly_matrix)

  }

image(t(as.matrix(cat)))
# 
# inverted_matrix <- t(original_matrix)

# Create matrices for each dataset
cat_week <- as.matrix(week_function(cat))
fox_week <- as.matrix(week_function(fox))
dingo_week <- as.matrix(week_function(dingo))
p_trid_week <- as.matrix(week_function(p_tridactylus))
potorous_week <- as.matrix(week_function(potorous))

# Visualize each matrix as an image
image(t(cat_week), main = "Cat Week Matrix")
image(t(fox_week), main = "Fox Week Matrix")
image(t(dingo_week), main = "Dingo Week Matrix")
image(t(p_trid_week), main = "P longipes Week Matrix")
image(t(potorous_week), main = "Potorous Week Matrix")


#load site covariates

site_cov <- read.csv(paste(directory,"site_level_covariates.csv", sep="/"))[,-1:-2]

as.factor(site_cov$fire_sev_infield)


# #load observation covariates

bait_cov <- read.csv(paste(directory,"daily_bait-type-chr_matrix.csv", sep="/"))[,-1]

# col1 <- numeric(length = nrow(bait_cov))

#load data for matching NA weeks for bait and average detection tiem covariates.

#write bait df to transfer NA values for non deployment dates

# df <- expand.grid(
#   placename = 1:100,
#   date = colnames(cat_week)
# )
# df$value <- c(t(cat_week))
# dfna <- df[is.na(df$value),]
# dfna$value<-dfna$value
# 
# image(cat_week)

Bait_week_function <- function(mat){
  
  weekly_matrix <- data.frame(matrix( NA, ncol=ncol(mat)/7, nrow = nrow(mat)))
  
  for (j in 1:(ncol(mat)/7)) {
    
    start <- starts[j]
    end <- j*7
    
    col1 <- character(nrow(mat))  # Initialize col1
    
    for (i in 1:nrow(mat)) {
      mat_vector <- as.character(mat[i, start:end])
      
      mat_vector[is.na(mat_vector)] <- "zero"
      
      # Count occurrences of each character
      char_count <- table(mat_vector)
      
      # Find the most frequent character(s)
      most_frequent <- names(char_count)[which.max(char_count)]
      
      # Assign the most frequent character to col1
      col1[i] <- as.character(most_frequent)
    }
    
    # Assign col1 to the corresponding column in weekly_matrix
    weekly_matrix[, j] <- col1
  }
  # dfna$placename <- dfna$placename
  # dfna$date <- as.character(dfna$date)
  # 
  # # Then proceed with your loop to update the matrix
  # for (i in 1:nrow(dfna)) {
  #   row <- dfna[i,]$placename
  #   col <- dfna[i,]$date
  #   
  #   weekly_matrix[row, col] = NA
  # }
  return(weekly_matrix)
}

bait_det_wekly <- Bait_week_function(bait_cov)

bait_det_wekly[is.na(cat_week)] <- NA

table(as.matrix(bait_det_wekly))

# Iterate through each row
for (i in 1:nrow(bait_det_wekly)) {
  # Iterate through each column (except the first one)
  for (j in 2:ncol(bait_det_wekly)) {
    # Check if the current cell is not missing and equals "zero"
    if (!is.na(bait_det_wekly[i, j]) && bait_det_wekly[i, j] == "zero") {
      # Check if the cell to the left is NA
      if (is.na(bait_det_wekly[i, j - 1])) {
        bait_det_wekly[i, j] <- "ManualReplace"
      } else {
        bait_det_wekly[i, j] <- bait_det_wekly[i, j - 1]
      }
    }
  }
}

as.data.frame(which(bait_det_wekly == "ManualReplace", arr.ind = TRUE))


bait_det_wekly[bait_det_wekly == "ManualReplace"] <- "Tuna Oil"



Avg_time_det_week_function <- function(mat){
  
  weekly_matrix <- data.frame(matrix( NA, ncol=ncol(mat)/7, nrow = nrow(mat)))
  
  for (j in 1:(ncol(mat)/7)) {
    
    start <- starts[j]
    end <- j*7
    
    col1 <- character(nrow(mat))  # Initialize col1
    
    for (i in 1:nrow(mat)) {
      mat_vector <- mat[i, start:end]
      
      det_times <- as.numeric(unlist(strsplit(mat_vector, ",")))
      
      mean_det <- mean(det_times, na.rm=TRUE)
      
      mean_det <- ifelse(is.na(mean_det), 0, mean_det)
      
         # Assign the most frequent character to col1
      col1[i] <- mean_det
    }
    
    # Assign col1 to the corresponding column in weekly_matrix
    weekly_matrix[, j] <- as.double(col1)
  }
  
 
  
  weekly_matrix[] <- lapply(weekly_matrix, function(x) suppressWarnings(as.numeric(as.character(x))))
 
  weekly_matrix[is.na(cat_week)] <- NA
   
  # dfna$placename <- as.character(dfna$placename)
  # dfna$date <- as.character(dfna$date)
  # 
  # # Then proceed with your loop to update the matrix
  # for (i in 1:nrow(dfna)) {
  #   row <- dfna[i,]$placename
  #   col <- dfna[i,]$date
  #   
  #   weekly_matrix[row, col] = NA
  # }
  
 return(weekly_matrix)
}

# Loop through each file to get average detection times for each species
for(file in files) {
  # Check if the file contains "presence-absence"
  if(grepl("time_matrix", file)) {
    # Extract the species name from the file name
    species <- gsub("_time_matrix.csv", "", gsub("daily_", "time_", file))
    # Read the file into a variable with the species name
    assign(species, as.matrix(read.csv(paste(directory, file, sep = "/"))[,-1]))
    }
}

cat_det=Avg_time_det_week_function(time_cat)
fox_det=Avg_time_det_week_function(time_fox)
dingo_det=Avg_time_det_week_function(time_dingo)
pot_trid_det=Avg_time_det_week_function(time_p_tridactylus)
pot_sp_det=Avg_time_det_week_function(time_potorous)

# pot_trid_det[pot_trid_det == 0] <- 0.000000001
# 
# as.double(pot_trid_det[1,1])




obs_cov <- list(bait=bait_det_wekly,
                 cat_det=cat_det,
                 fox_det=fox_det,
                 dingo_det=dingo_det,
                 pot_trid_det=pot_trid_det,
                 pot_sp_det=pot_sp_det)

saveRDS(obs_cov, paste(directory, "obsservation_covariates.rds", sep="/"))


#ccupancy formulas

#Single Occupancy fitting for each species to for determining optimal covariates for detection and occupancy

pot_trid_umf <- unmarkedFrameOccu(y=p_trid_week, siteCovs =site_cov, obs_cov)

pot_sp_umf <- unmarkedFrameOccu(y=potorous_week, siteCovs =site_cov, obs_cov)

cat_umf <- unmarkedFrameOccu(y=cat_week, siteCovs =site_cov, obs_cov)

dingo_umf <- unmarkedFrameOccu(y=dingo_week, siteCovs =site_cov,obsCovs = obs_cov)

fox_umf <- unmarkedFrameOccu(y=fox_week, siteCovs =site_cov, obs_cov)

#Optimal detection covariates for each species ####

#Potorous tridactylus models

mod0 <- occu(~1~fire_sev_infield +scale(elevation) +scale(flii) +scale(Avg_mnth_tot_rain) + scale(Avg_mnth_ndvi), pot_trid_umf)
mod1 <- occu(~bait~fire_sev_infield +scale(elevation) +scale(flii) +scale(Avg_mnth_tot_rain) + scale(Avg_mnth_ndvi), pot_trid_umf)
mod2 <- occu(~pot_trid_det~fire_sev_infield +scale(elevation) +scale(flii) +scale(Avg_mnth_tot_rain) + scale(Avg_mnth_ndvi), pot_trid_umf)
mod3 <- occu(~bait+pot_trid_det~fire_sev_infield +scale(elevation) +scale(flii) +scale(Avg_mnth_tot_rain) + scale(Avg_mnth_ndvi), pot_trid_umf)

mods <- fitList("1"=mod0,"bait"=mod1,"pot_trid_det"=mod2, "bait+pot_trid_det"= mod3)
P_trid_det_rank <- modSel(mods)

#Potorous sp. models

mod0 <- occu(~1~fire_sev_infield +scale(elevation) +scale(flii) +scale(Avg_mnth_tot_rain) + scale(Avg_mnth_ndvi), pot_sp_umf)
mod1 <- occu(~bait~fire_sev_infield +scale(elevation) +scale(flii) +scale(Avg_mnth_tot_rain) + scale(Avg_mnth_ndvi), pot_sp_umf)
mod2 <- occu(~pot_sp_det~fire_sev_infield +scale(elevation) +scale(flii) +scale(Avg_mnth_tot_rain) + scale(Avg_mnth_ndvi), pot_sp_umf)
mod3 <- occu(~bait+pot_sp_det~fire_sev_infield +scale(elevation) +scale(flii) +scale(Avg_mnth_tot_rain) + scale(Avg_mnth_ndvi), pot_sp_umf)

mods <- fitList("1"=mod0,"bait"=mod1,"pot_sp_det"=mod2, "bait+pot_sp_det"= mod3)
P_sp_det_rank <- modSel(mods)

# Cat models

mod0 <- occu(~1~fire_sev_infield +scale(elevation) +scale(flii) +scale(Avg_mnth_tot_rain) + scale(Avg_mnth_ndvi), cat_umf)
mod1 <- occu(~bait~fire_sev_infield +scale(elevation) +scale(flii) +scale(Avg_mnth_tot_rain) + scale(Avg_mnth_ndvi), cat_umf)
mod2 <- occu(~cat_det~fire_sev_infield +scale(elevation) +scale(flii) +scale(Avg_mnth_tot_rain) + scale(Avg_mnth_ndvi), cat_umf)
mod3 <- occu(~bait+cat_det~fire_sev_infield +scale(elevation) +scale(flii) +scale(Avg_mnth_tot_rain) + scale(Avg_mnth_ndvi), cat_umf)

mods <- fitList("1"=mod0,"bait"=mod1,"cat_det"=mod2, "bait+cat_det"= mod3)
cat_det_rank <- modSel(mods)

# Dingo models

mod0 <- occu(~1~fire_sev_infield +scale(elevation) +scale(flii) +scale(Avg_mnth_tot_rain) + scale(Avg_mnth_ndvi), dingo_umf)
mod1 <- occu(~bait~fire_sev_infield +scale(elevation) +scale(flii) +scale(Avg_mnth_tot_rain) + scale(Avg_mnth_ndvi), dingo_umf)
mod2 <- occu(~dingo_det~fire_sev_infield +scale(elevation) +scale(flii) +scale(Avg_mnth_tot_rain) + scale(Avg_mnth_ndvi), dingo_umf)
mod3 <- occu(~bait+dingo_det~fire_sev_infield +scale(elevation) +scale(flii) +scale(Avg_mnth_tot_rain) + scale(Avg_mnth_ndvi), dingo_umf)

mods <- fitList("1"=mod0,"bait"=mod1,"dingo_det"=mod2, "bait+dingo_det"= mod3)
dingo_det_rank <- modSel(mods)

# Fox models

mod0 <- occu(~1~fire_sev_infield +scale(elevation) +scale(flii) +scale(Avg_mnth_tot_rain) + scale(Avg_mnth_ndvi), fox_umf)
mod1 <- occu(~bait~fire_sev_infield +scale(elevation) +scale(flii) +scale(Avg_mnth_tot_rain) + scale(Avg_mnth_ndvi), fox_umf)
mod2 <- occu(~fox_det~fire_sev_infield +scale(elevation) +scale(flii) +scale(Avg_mnth_tot_rain) + scale(Avg_mnth_ndvi), fox_umf)
mod3 <- occu(~bait+fox_det~fire_sev_infield +scale(elevation) +scale(flii) +scale(Avg_mnth_tot_rain) + scale(Avg_mnth_ndvi), fox_umf)

mods <- fitList("1"=mod0,"bait"=mod1,"fox_det"=mod2, "bait+fox_det"= mod3)
fox_det_rank <- modSel(mods)

#Optimal occupancy covariates for each species ####


create_single_occupancy_models <- function(species, predictors, det_list) {
  # Initialize an empty list to store the models
  model_list <- list()
  
  detection_cov <- grep("bait\\+", det_list@Full[["model"]], value = TRUE)
  
  
  
  # Generate all possible combinations of predictor variables
  predictor_combinations <- unlist(lapply(1:length(predictors), function(x) combn(predictors, x, simplify = FALSE)), recursive = FALSE)
  
  #predictor_combinations[[length(predictor_combinations)+1]] <- "1"
    
  # Iterate over each combination of predictor variables
  for (comb in predictor_combinations) {
    # Construct formula for the occupancy model
    formula <- as.formula(paste("~", detection_cov ,"~", "fire_sev_infield + ", paste(comb, collapse = "+"), sep = ""))
    
    # Create the occupancy model using occu function
    model <- occu(formula, data = get(species))
    
    # Create a string representation of the combination of covariates
    comb_str <- paste(comb, collapse = "+")
    
    # Add the model to the list with the combination name as the list name
    model_list[[comb_str]] <- model
  }
  
  model_list[["Null"]] <-  occu(formula = ~1~1, data = get(species))
  
  return(model_list)
}

predictors <- c("scale(elevation)", "scale(flii)", "scale(Avg_mnth_tot_rain)", "scale(Avg_mnth_ndvi)")

#Creation for ranking of the 128 possible combinatiosn for each species with 7 variables
P_trid_models <- create_single_occupancy_models("pot_trid_umf", predictors, P_trid_det_rank)

P_sp_models <- create_single_occupancy_models("pot_sp_umf", predictors, P_sp_det_rank)

cat_models <- create_single_occupancy_models("cat_umf", predictors, cat_det_rank)

fox_models <- create_single_occupancy_models("fox_umf", predictors, fox_det_rank)

dingo_models <- create_single_occupancy_models("dingo_umf", predictors, dingo_det_rank)

single_occ_list <- list("P.tridactylus"=P_trid_models, "P.sp" = P_sp_models, "cat"=cat_models, "fox" = fox_models, "dingo"=dingo_models)

names(single_occ_list[1])

#Table of ranked single occupancy models for results####
single_sp_model_ranks <- data.frame(
  Species=character(),
  Model = character(),
  logLike = numeric(),
  No.parameters = numeric(),
  AIC = numeric(),
  `ΔAIC` = numeric(),
  AICwt = numeric(),

  
  stringsAsFactors = FALSE
)


#for loop to write ranked single occupancy models for each species list
for(i in 1:length(single_occ_list)){
  
  # Fit models for the current element in single_occ_list
  mods <- fitList(single_occ_list[[i]])
  
  # Model selection
  lists <- modSel(mods)
  
  # Extracting and assigning values to temp data frame
  single_sp_model_ranks[(6 * (i - 1) + 1):(6 * i), "Species"] <- rep(names(single_occ_list[i]), 6)
  single_sp_model_ranks[(6 * (i - 1) + 1):(6 * i), "Model"] <- c(lists@Full[["formula"]][1:5], lists@Full[["formula"]][which(lists@Full[["model"]] == "Null")])
  single_sp_model_ranks[(6 * (i - 1) + 1):(6 * i), "logLike"] <- c(lists@Full[["negLogLike"]][1:5], lists@Full[["negLogLike"]][which(lists@Full[["model"]] == "Null")])
  single_sp_model_ranks[(6 * (i - 1) + 1):(6 * i), "No.parameters"] <- c(lists@Full[["nPars"]][1:5], lists@Full[["nPars"]][which(lists@Full[["model"]] == "Null")])
  single_sp_model_ranks[(6 * (i - 1) + 1):(6 * i), "AIC"] <- c(lists@Full[["AIC"]][1:5], lists@Full[["AIC"]][which(lists@Full[["model"]] == "Null")])
  single_sp_model_ranks[(6 * (i - 1) + 1):(6 * i), "ΔAIC"] <- c(lists@Full[["delta"]][1:5], lists@Full[["delta"]][which(lists@Full[["model"]] == "Null")])
  single_sp_model_ranks[(6 * (i - 1) + 1):(6 * i), "AICwt"] <- c(lists@Full[["AICwt"]][1:5], lists@Full[["AICwt"]][which(lists@Full[["model"]] == "Null")])
  
}


# Multi-Species Model fitting - MSOM ####

#MSOM Presence/Absence dataframe list collation ####
Model_1_list <- list(P.tridactylus=p_trid_week,cat=cat_week,dingo=dingo_week,fox=fox_week)

Model_2_list <- list(P.sp=potorous_week,cat=cat_week,dingo=dingo_week,fox=fox_week)
  
# , 
#      fox = fox_week, 
#      dingo=dingo_week)


#Making unmarked OccuMulti frames for each species x predator combination

Model_1_umf = unmarkedFrameOccuMulti(y=Model_1_list, siteCovs =site_cov, obs_cov)

Model_2_umf = unmarkedFrameOccuMulti(y=Model_2_list, siteCovs =site_cov, obs_cov)


#Model 1 state and detection formulas ####
#Combinations to extract models for 4 species up the 2nd level species interactions
combinations <- list(
  c(1),
  c(2),
  c(3),
  c(4),
  c(1,2),
  c(1,3),
  c(1,4),
  c(2,3),
  c(2,4),
  c(3,4)
)

# List of species
species <- names(Model_1_list)

#Function to extract state and formula models from ranked single occupancy models
results <- lapply(species, function(sp) {
  filtered_data <- single_sp_model_ranks[single_sp_model_ranks$Species == sp, ]
  min_AIC_row <- filtered_data[which.min(filtered_data$AIC), ]
  return(min_AIC_row$Model)
})
# Combine species with their corresponding models
species_models <- data.frame(Species = species, Model = unlist(results), stringsAsFactors = FALSE)

# Split the "Model" column by "~" into "det_formula" and "state_formulas"
split_models <- strsplit(species_models$Model, " ~ ")

# Extract det_formula and state_formula separately
Model_1_det_formula <- paste(sapply(split_models, `[`, 1))
Model_1_state_formula <- paste(paste(sapply(split_models, `[`, 2)))

Model_1_states <- vector("list", length = length(combinations)) # Initialize a list to store the results

for (i in seq_along(combinations)) {
  comb <- combinations[[i]]
  Model_1_states[[i]] <- paste("~", paste(unique(unlist(strsplit(Model_1_state_formula[comb], " \\+ "))), collapse = " + "))
}

print(Model_1_states) #final state formulas for model 1

#Model 2 state and detection formulas ####
# List of species
species <- names(Model_2_list)

#Function to extract state and formula models from ranked single occupancy models
results <- lapply(species, function(sp) {
  filtered_data <- single_sp_model_ranks[single_sp_model_ranks$Species == sp, ]
  min_AIC_row <- filtered_data[which.min(filtered_data$AIC), ]
  return(min_AIC_row$Model)
})
# Combine species with their corresponding models
species_models <- data.frame(Species = species, Model = unlist(results), stringsAsFactors = FALSE)

# Split the "Model" column by "~" into "det_formula" and "state_formulas"
split_models <- strsplit(species_models$Model, " ~ ")

# Extract det_formula and state_formula separately
Model_2_det_formula <- paste(sapply(split_models, `[`, 1))

Model_2_state_formula <- paste(paste(sapply(split_models, `[`, 2)))

Model_2_states <- list(length = length(combinations)) # Initialize a list to store the results

for (i in seq_along(combinations)) {
  comb <- combinations[[i]]
  Model_2_states[i] <- paste("~", paste(unique(unlist(strsplit(Model_2_state_formula[comb], " \\+ "))), collapse = " + "))
}

print(Model_2_states) #final state formulas for model 2

states_mod1 <- c(Model_1_states[[1]],
                 Model_1_states[[2]],
                 Model_1_states[[3]],
                 Model_1_states[[4]],
                 Model_1_states[[5]],
                 Model_1_states[[6]],
                 Model_1_states[[7]],
                 Model_1_states[[8]],
                 Model_1_states[[9]],
                 Model_1_states[[10]])

states_mod2 <- c(Model_2_states[[1]],
                 Model_2_states[[2]],
                 Model_2_states[[3]],
                 Model_2_states[[4]],
                 Model_2_states[[5]],
                 Model_2_states[[6]],
                 Model_2_states[[7]],
                 Model_2_states[[8]],
                 Model_2_states[[9]],
                 Model_2_states[[10]])

# MSOM Fitting ####
set.seed(15)
Model_1 <- occuMulti(detformulas = Model_1_det_formula, stateformulas =  states_mod1[1:10] , data = Model_1_umf, maxOrder = 2)

Model_1_co_species_1 <- occuMulti(detformulas = Model_1_det_formula, stateformulas = c(states_mod1[1:4], rep("~1", 6)), data = Model_1_umf, maxOrder = 2)

Model_1_co_species_0 <- occuMulti(detformulas = rep("~1", 4),  stateformulas =   c(rep("~1", 4),rep("~0",6)), data = Model_1_umf, maxOrder = 2)

set.seed(15)
Model_2 <- occuMulti(detformulas = Model_2_det_formula, stateformulas =  states_mod2[1:10] , data = Model_2_umf, maxOrder = 2, penalty=0.5)

Model_2_co_species_1 <- occuMulti(detformulas = Model_2_det_formula, stateformulas =  c(states_mod2[1:4], rep("~1", 6)) , data = Model_2_umf, maxOrder = 2)

Model_2_co_species_0 <- occuMulti(detformulas = rep("~1", 4),  stateformulas =  c(rep("~1", 4),rep("~0",6)), data = Model_2_umf, maxOrder = 2)


#THis takeas a long time. Better an hour or so.
set.seed(1234)
mod1_penalty <- optimizePenalty(Model_1, penalties=seq(0.5,0.7,0.1)) #completed
set.seed(1234)
mod1_1_penalty <- optimizePenalty(Model_1_co_species_1, penalties=seq(0.1,1.5,0.1)) #completed
set.seed(1234)
mod1_0_penalty <- optimizePenalty(Model_1_co_species_0, penalties=seq(0.1,1.5,0.1)) #completed
# summary(mod1_penalty)
# 
set.seed(123)
mod2_penalty <- optimizePenalty(Model_2, penalties=seq(0.7,1.5,0.1)) #completed
set.seed(123)
mod2_1_penalty <- optimizePenalty(Model_2_co_species_1, penalties=seq(0.5,1.5,0.1)) #completed
set.seed(123)
mod2_0_penalty <- optimizePenalty(Model_2_co_species_0, penalties=seq(0.1,1.5,0.1))
#summary(mod1_penalty)

#Final penalised 
mods_msom1 <- fitList(mod1_penalty,mod1_1_penalty,mod1_0_penalty )
model1_fitlist <- modSel(mods_msom1)


mods_msom2 <- fitList(Model_2,mod2_1_penalty,mod2_0_penalty)
model2_fitlist <- modSel(mods_msom2)


  
#Save model outputs  
saveRDS(mod1_penalty, file = paste(directory, "/Model_1.Rdata", sep = ""))
saveRDS(mod1_1_penalty, file = paste(directory, "/Model_1_co_species_1.Rdata", sep = ""))
saveRDS(mod1_0_penalty, file = paste(directory, "/Model_1_co_species_0.Rdata", sep = ""))
saveRDS(Model_2, file = paste(directory, "/Model_2.Rdata", sep = ""))
saveRDS(mod2_1_penalty, file = paste(directory, "/Model_2_co_species_1.Rdata", sep = ""))
saveRDS(mod2_0_penalty, file = paste(directory, "/Model_2_co_species_0.Rdata", sep = ""))

