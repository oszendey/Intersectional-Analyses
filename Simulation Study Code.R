################################################################################
####             Intersectional Simulation Study Code                       ####
####                        Olivia Szendey                                  ####
################################################################################




#Load Required Packages
library(truncnorm)
library(dplyr)
library(lme4)
library(lmerTest)
library(writexl)
library(openxlsx)
library(ggplot2)
library(gridExtra)
library(grid)



#Set Distribution for Truth Coefficients 
#bimodal dist for coeff
Sample.negative.coeff <- function (n) {
  y0 <- rtruncnorm(n,a=-2.0, b=-0.20, mean=-1, sd = 1)
} 

Sample.positive.coeff <- function (n) {
  y0 <- rtruncnorm(n,a=.20, b=2.0, mean=1, sd = 1)
}

###############################################################################

####Two Categories######

#Set seeds, each seed was randomly generated using: sample(1:10000,6 ) 
s1<-4186 
s2<-2925 
s3<-5333  
s4<-2931 
s5<-3474


#Randomly sample from distributions for truth coefficients 
bA0B0<-0
set.seed(s1)
bA1B0<-Sample.positive.coeff(1)
set.seed(s2)
bA2B0<-Sample.negative.coeff(1)
set.seed(s3)
bA3B0<-Sample.negative.coeff(1)
bA4B0<-0
bA5B0<-0
bA6B0<-0
bA0B1<- 0
set.seed(s4)
bA1B1<-Sample.negative.coeff(1)
set.seed(s5)
bA2B1<-Sample.positive.coeff(1)
bA3B1<-0
bA4B1<-0
bA5B1<-0
bA6B1<-0


A<- c(0, 1, 2, 3, 4, 5, 6)
B<- c(0, 1)
L1<-expand.grid(A=A, B=B)
L1$cluster <-factor(100*(1+L1$A)+ 10*(1+L1$B))


L1$A1 <- ifelse(L1$A==1, 1, 0)
L1$A2<- ifelse(L1$A==2, 1, 0)
L1$A3 <- ifelse(L1$A==3, 1, 0)
L1$A4 <- ifelse(L1$A==4, 1, 0)
L1$A5 <- ifelse(L1$A==5, 1, 0)
L1$A6 <- ifelse(L1$A==6, 1, 0)
L1$A0 <- ifelse(L1$A==0,1,0)
L1$B0<-ifelse(L1$B==0, 1, 0)
L1$B1<-ifelse(L1$B==1, 1, 0)

L1$A0B1 <- ifelse(L1$A == 0 & L1$B == 1, 1, 0)
L1$A1B1 <- ifelse(L1$A == 1 & L1$B == 1, 1, 0)
L1$A2B1 <- ifelse(L1$A == 2 & L1$B == 1, 1, 0)
L1$A3B1 <- ifelse(L1$A == 3 & L1$B == 1, 1, 0)
L1$A4B1 <- ifelse(L1$A == 4 & L1$B == 1, 1, 0)
L1$A5B1 <- ifelse(L1$A == 5 & L1$B == 1, 1, 0)
L1$A6B1 <- ifelse(L1$A == 6 & L1$B == 1, 1, 0)
L1$A0B0 <- ifelse(L1$A == 0 & L1$B == 0, 1, 0)
L1$A1B0 <- ifelse(L1$A == 1 & L1$B == 0, 1, 0)
L1$A2B0 <- ifelse(L1$A == 2 & L1$B == 0, 1, 0)
L1$A3B0 <- ifelse(L1$A == 3 & L1$B == 0, 1, 0)
L1$A4B0 <- ifelse(L1$A == 4 & L1$B == 0, 1, 0)
L1$A5B0 <- ifelse(L1$A == 5 & L1$B == 0, 1, 0)
L1$A6B0 <- ifelse(L1$A == 6 & L1$B == 0, 1, 0)

n.schools <- 100

# Initialize an empty dataframe to store the results
Truth <- data.frame()

# Loop through each school
for (i in 1:n.schools) {
  
  # Replicate the structure of Truth for the current school
  df <- L1
  df$school_ID <- i
  
  # Add the replicated data to the results dataframe
  Truth <- rbind(Truth, df)
}

n.schools <- 100
set.seed(123)
u <- rnorm(n.schools, mean = 0, sd = 0.5)
set.seed(456)
ei <- rnorm(nrow(Truth), mean = 0, sd = 1)



# Generate Outcome Variable
Truth$y <- bA0B1 * Truth$A0B1 + bA1B1 * Truth$A1B1 + bA2B1 * Truth$A2B1 +
  bA3B1 * Truth$A3B1 + bA4B1 * Truth$A4B1 + bA5B1 * Truth$A5B1 +
  bA6B1 * Truth$A6B1 + bA1B0 * Truth$A1B0 + bA2B0 * Truth$A2B0 +
  bA3B0 * Truth$A3B0 + bA4B0 * Truth$A4B0 + bA5B0 * Truth$A5B0 +
  bA6B0 * Truth$A6B0 + u[Truth$school_ID] + ei


library(dplyr)

mA0B1 <- Truth %>% 
  filter(A0B1 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA1B1 <- Truth %>% 
  filter(A1B1 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA2B1 <- Truth %>% 
  filter(A2B1 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA3B1 <- Truth %>% 
  filter(A3B1 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA4B1 <- Truth %>% 
  filter(A4B1 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA5B1 <- Truth %>% 
  filter(A5B1 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA6B1 <- Truth %>% 
  filter(A6B1 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA0B0 <- Truth %>% 
  filter(A0B0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA1B0 <- Truth %>% 
  filter(A0B1 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)


mA2B0 <- Truth %>% 
  filter(A0B1 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA3B0 <- Truth %>% 
  filter(A0B1 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA4B0 <- Truth %>% 
  filter(A0B1 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA5B0 <- Truth %>% 
  filter(A0B1 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA6B0 <- Truth %>% 
  filter(A0B1 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)





#Build Dataset

DL1<- data.frame()


# Sample Size Scenarios
n.list <- c(5000, 10000, 20000)

# Probability Scenarios
prob_list <- list(
  characteristic1 = list(A = c(0.143, .143, .143, .143, .143, .143, .143), B = c(.500, .500)),
  characteristic2 = list(A = c(0.432, .05, .073, .137, .208, .05, .05), B = c(.410, .590)),
  characteristic3 = list(A = c(0.63, .05, .110, .05, .05, .055, .055), B = c(.200, .800))
)

datasets <- list()


# Iterate through sample sizes
for (n in n.list) {
  # Iterate through probability characteristics
  for (char in seq_along(prob_list)) {
    # Create the dataset based on current sample size and probability characteristic
    DL <- data.frame(A = sample(c(0:6), n, prob = prob_list[[char]]$A, replace = TRUE),
                     B = sample(c(0,1), n, prob = prob_list[[char]]$B, replace = TRUE))
    
    # Store in the list with new naming convention
    datasets[[paste0("n_", n, "_p", char)]] <- DL
  }
}




build_dataset <- function(DL1) {
  DL1$cluster <- interaction(DL1$A + 1, DL1$B + 1, sep = "")
  
  # Creating interaction variables for each combination of A and B
  for (i in 0:6) {
    for (j in 0:1) {
      DL1[[paste0("A", i, "B", j)]] <- ifelse(DL1$A == i & DL1$B == j, 1, 0)
    }
  }
  
  # Create separate binary variables for A and B levels
  DL1$A0 <- ifelse(DL1$A == 0, 1, 0)
  DL1$A1 <- ifelse(DL1$A == 1, 1, 0)
  DL1$A2 <- ifelse(DL1$A == 2, 1, 0)
  DL1$A3 <- ifelse(DL1$A == 3, 1, 0)
  DL1$A4 <- ifelse(DL1$A == 4, 1, 0)
  DL1$A5 <- ifelse(DL1$A == 5, 1, 0)
  DL1$A6 <- ifelse(DL1$A == 6, 1, 0)
  DL1$B0 <- ifelse(DL1$B == 0, 1, 0)
  DL1$B1 <- ifelse(DL1$B == 1, 1, 0)
  
  # Randomly assign a school_ID to every single case in datasets
  DL1$school_ID <- sample(1:100, size = nrow(DL1), replace = TRUE)
  
  DL1$cluster <- as.numeric(as.character(DL1$cluster))
  DL1$school_ID <- as.numeric(as.character(DL1$school_ID))
  DL1$UniqueCluster <- DL1$cluster + DL1$school_ID
  
  return(DL1)
}

datasets_2 <- list()
datasets_2 <- lapply(datasets, build_dataset)



#Generate Outcome 

create_outcome_variable <- function(DL) {
  n.schools <- 100
  set.seed(123)
  u <- rnorm(n.schools, mean = 0, sd = 0.5)
  set.seed(456)
  ei <- rnorm(nrow(DL), mean = 0, sd = 1)
  
  
  
  # Generate Outcome Variable
  DL$y <- bA0B1 * DL$A0B1 + bA1B1 * DL$A1B1 + bA2B1 * DL$A2B1 +
    bA3B1 * DL$A3B1 + bA4B1 * DL$A4B1 + bA5B1 * DL$A5B1 +
    bA6B1 * DL$A6B1 + bA1B0 * DL$A1B0 + bA2B0 * DL$A2B0 +
    bA3B0 * DL$A3B0 + bA4B0 * DL$A4B0 + bA5B0 * DL$A5B0 +
    bA6B0 * DL$A6B0 + u[DL$school_ID] + ei
  
  return(DL)
}

# Apply this function to datasets_2 to create outcome
datasets_with_y <- lapply(datasets_2, create_outcome_variable)



# Extract unique clusters from one of the datasets (this is the combo of schoolID & intersectional group)
unique_clusters <- unique(datasets_with_y[[1]]$UniqueCluster)

apply_std_manipulation <- function(dataset, std_setting, large_clusters) {
  # Make a copy of the original dataset
  modified_dataset <- dataset
  
  # Compute the overall standard deviation for reference
  overall_sd <- sd(dataset$y)
  
  for(cluster in unique(dataset$UniqueCluster)) {
    # Identify the indices corresponding to the current cluster
    cluster_indices <- which(dataset$UniqueCluster == cluster)
    
    # Extract the y values for the current cluster
    cluster_y <- dataset$y[cluster_indices]
    
    # If only one observation, skip the adjustment
    if (length(cluster_y) == 1) {
      next
    }
    
    # Compute the mean and standard deviation for the current cluster
    current_mean <- mean(cluster_y)
    current_sd <- sd(cluster_y)
    
    # Adjust standard deviation based on the std_setting
    if (std_setting == "small") {
      scaling_factor <- 1
    } else if (std_setting == "large") {
      scaling_factor <- 5 * (current_sd / overall_sd) 
    } else if (std_setting == "mixed") {
      scaling_factor <- ifelse(cluster %in% large_clusters, 5, 1) * (current_sd / overall_sd)  # Example formula
    } else {
      stop("Invalid std_setting")
    }
    
    # Center, standardize, and then scale the data
    centered_y <- cluster_y - current_mean
    scaled_y <- centered_y * scaling_factor
    
    # Add back the original mean
    modified_y <- scaled_y + current_mean
    
    # Replace the original y values with the modified y values
    modified_dataset$y[cluster_indices] <- modified_y
  }
  
  return(modified_dataset)
}




set.seed(12345) # For reproducibility
large_clusters <- sample(unique_clusters, size = length(unique_clusters) * 0.1)



std_list <- c("small", "large", "mixed")

final_datasets <- list()

# Iterate through all the datasets generated earlier
for (i in seq_along(datasets_with_y)) {
  dataset <- datasets_with_y[[i]]
  
  # Iterate through the standard deviation scenarios
  for (std_setting in std_list) {
    # Apply the standard deviation manipulation
    modified_dataset <- apply_std_manipulation(dataset, std_setting, large_clusters)
    
    # Store in the list with a new naming convention
    final_datasets[[paste0(names(datasets_with_y)[i], "_std_", std_setting)]] <- modified_dataset
  }
}

###Replications#####

# List to store all the conditions
all_conditions <- list()

# Number of replications
num_replications <- 10

# Iterate through the sample sizes, probability characteristics, and standard deviation settings
for (n in n.list) {
  for (char in seq_along(prob_list)) {
    for (std_setting in std_list) {
      # List to store the replications for this condition
      replications <- list()
      
      # Iterate for each replication
      for (replication in 1:num_replications) {
        # Set a different seed for each replication
        set.seed(12345 + replication)
        
        # Create the dataset based on current sample size and probability characteristic
        DL <- data.frame(A = sample(c(0:6), n, prob = prob_list[[char]]$A, replace = TRUE),
                         B = sample(c(0,1), n, prob = prob_list[[char]]$B, replace = TRUE))
        # Build the dataset
        dataset <- build_dataset(DL)
        # Generate outcome variable
        dataset <- create_outcome_variable(dataset)
        # Apply the standard deviation manipulation
        modified_dataset <- apply_std_manipulation(dataset, std_setting, large_clusters)
        
        # Store in the list with a new naming convention
        replications[[paste0("replication_", replication)]] <- modified_dataset
      }
      
      # Store this condition in the all_conditions list
      condition_name <- paste0("n_", n, "_p", char, "_std_", std_setting)
      all_conditions[[condition_name]] <- replications
    }
  }
}



get_fixed_effects <- function(model, covariate_names) {
  result_row <- rep(NA, length(covariate_names))
  coefficients <- fixef(model)
  matched_indices <- match(names(coefficients), covariate_names)
  valid_indices <- !is.na(matched_indices)
  result_row[matched_indices[valid_indices]] <- as.numeric(coefficients[valid_indices])
  return(result_row)
}



#####
#Model & Store Results##

#####


##Categorical & Interaction####

covariate_names_categorical<- c("A1B0", "A2B0", "A3B0", "A4B0", "A5B0", "A6B0", 
                                "A0B1", "A1B1", "A2B1",  "A3B1", "A4B1", "A5B1", "A6B1") 
covariate_names_interaction<-c("A1", "A2", "A3", "A4", "A5", "A6","B1",
                               "A1B1", "A2B1", "A3B1", "A4B1", "A5B1","A6B1")

# Lists to store p-values, AIC and BIC, and categorical results for each condition
categorical_p_values_list <- list()
categorical_aic_bic_values_list <- list()
categorical_results <- list()
categorical_condition_ci_values <- data.frame()


categorical_ci_values_list <- list()
interaction_ci_values_list <- list()

interaction_p_values_list <- list()
interaction_aic_bic_values_list <- list()
interaction_results <- list()
interaction_condition_ci_values <- data.frame()


categorical_icc_values_list <- list()
interaction_icc_values_list <- list()

# Iterate through the conditions
for (condition_name in names(all_conditions)) {
  # Initialize an empty data frame to store p-values for this condition
  categorical_condition_p_values <- data.frame()
  # Data frame to store AIC and BIC for each replication within this condition
  categorical_condition_aic_bic <- data.frame(AIC=numeric(), BIC=numeric())
  # Initialize an empty data frame to store the results for each replication within this condition
  condition_results_categorical <- data.frame()
  
  categorical_condition_ci_values <- matrix(nrow=0, ncol=length(covariate_names_categorical) * 2)
  colnames(categorical_condition_ci_values) <- c(sapply(covariate_names_categorical, function(x) paste0(x, "_LL")), sapply(covariate_names_categorical, function(x) paste0(x, "_UL")))
  
  categorical_condition_icc_values <- c()
  interaction_condition_icc_values <- c()
  
  # Initialize an empty data frame to store p-values for this condition
  interaction_condition_p_values <- data.frame()
  # Data frame to store AIC and BIC for each replication within this condition
  interaction_condition_aic_bic <- data.frame(AIC=numeric(), BIC=numeric())
  # Initialize an empty data frame to store the results for each replication within this condition
  condition_results_interaction <- data.frame()
  
  
  interaction_condition_ci_values <- matrix(nrow=0, ncol=length(covariate_names_interaction) * 2)
  colnames(interaction_condition_ci_values) <- c(sapply(covariate_names_interaction, function(x) paste0(x, "_LL")), sapply(covariate_names_interaction, function(x) paste0(x, "_UL")))
  
  
  # Iterate through the replications within this condition
  for (replication_name in names(all_conditions[[condition_name]])) {
    # Retrieve the dataset
    DL <- all_conditions[[condition_name]][[replication_name]]
    
    # Fit the categorical model
    categorical_model <- lmer(y ~ A1B0 + A2B0 + A3B0 + A4B0 + A5B0 + A6B0 + A0B1 + A1B1 + A2B1 + A3B1 + A4B1 + A5B1 + A6B1 + (1 | school_ID), data = DL)
    
    var_components_cat <- VarCorr(categorical_model)
    
    
    var_random <- attr(var_components_cat$school_ID, "stddev")[1]^2
    var_residual <- attr(var_components_cat, "sc")^2
    icc_categorical <- var_random / (var_random + var_residual)
    
    
    # Store ICC value for this replication in the vector
    categorical_condition_icc_values <- c(categorical_condition_icc_values, icc_categorical)
    
    #fit the interaction model
    
    interaction_model<-lmer(y~A1 + A2 + A3 + A4 + A5 + A6 + B1 +
                              A1B1 + A2B1 + A3B1 + A4B1 + A5B1 + A6B1 + (1 | school_ID), data = DL)
    
    
    var_components_int <- VarCorr(interaction_model)
    var_random <- attr(var_components_int$school_ID, "stddev")[1]^2
    var_residual <- attr(var_components_int, "sc")^2
    icc_interaction <- var_random / (var_random + var_residual)
    
    # Store ICC value for this replication in the vector
    interaction_condition_icc_values <- c(interaction_condition_icc_values, icc_interaction)
    
    
    # Extract fixed effects' coefficients
    categorical_coefficients <- as.data.frame(t(fixef(categorical_model)))
    colnames(categorical_coefficients) <- names(fixef(categorical_model))
    
    interaction_coefficients <- as.data.frame(t(fixef(interaction_model)))
    colnames(interaction_coefficients) <- names(fixef(interaction_model))
    
    # Extract p-values for fixed effects
    categorical_p_values <- as.data.frame(t(summary(categorical_model)$coefficients[, "Pr(>|t|)"]))
    colnames(categorical_p_values) <- names(summary(categorical_model)$coefficients[, "Pr(>|t|)"])
    
    interaction_p_values <- as.data.frame(t(summary(interaction_model)$coefficients[, "Pr(>|t|)"]))
    colnames(interaction_p_values) <- names(summary(interaction_model)$coefficients[, "Pr(>|t|)"])
    
    # Store p-values for this replication
    categorical_condition_p_values <- rbind(categorical_condition_p_values, categorical_p_values)
    interaction_condition_p_values <- rbind(interaction_condition_p_values, interaction_p_values)
    
    
    # Extract AIC and BIC
    categorical_condition_aic_bic <- rbind(categorical_condition_aic_bic, data.frame(AIC=AIC(categorical_model), BIC=BIC(categorical_model)))
    interaction_condition_aic_bic <- rbind(interaction_condition_aic_bic, data.frame(AIC=AIC(interaction_model), BIC=BIC(interaction_model)))
    
    # Store results for this replication
    condition_results_categorical <- rbind(condition_results_categorical, categorical_coefficients)
    condition_results_interaction <- rbind(condition_results_interaction, interaction_coefficients)
    
    
    # Extract the fixed effect coefficients, excluding the intercept
    categorical_coefficients <- fixef(categorical_model)[-1]
    
    # Extract the corresponding standard errors, excluding the intercept
    categorical_se <- sqrt(diag(vcov(categorical_model)))[-1]
    
    # Calculate the z-value for the desired confidence level
    z_value <- qnorm(1 - (1 - 0.95) / 2)
    
    # Calculate the lower and upper confidence limits, excluding the intercept
    categorical_ci <- data.frame(
      LL = categorical_coefficients - z_value * categorical_se,
      UL = categorical_coefficients + z_value * categorical_se
    )
    
    # Reshape and name the confidence intervals
    categorical_ci_reshaped <- as.data.frame(as.list(as.numeric(t(categorical_ci))))
    colnames(categorical_ci_reshaped) <- c(sapply(covariate_names_categorical, function(x) paste0(x, "_LL")), sapply(covariate_names_categorical, function(x) paste0(x, "_UL")))
    
    categorical_condition_ci_values <- rbind(categorical_condition_ci_values, categorical_ci_reshaped)
    
    
    
    
    # Extract the fixed effect coefficients, excluding the intercept
    interaction_coefficients <- fixef(interaction_model)[-1]
    
    # Extract the corresponding standard errors, excluding the intercept
    interaction_se <- sqrt(diag(vcov(interaction_model)))[-1]
    
    # Calculate the z-value for the desired confidence level
    z_value <- qnorm(1 - (1 - 0.95) / 2)
    
    # Calculate the lower and upper confidence limits, excluding the intercept
    interaction_ci <- data.frame(
      LL = interaction_coefficients - z_value * interaction_se,
      UL = interaction_coefficients + z_value * interaction_se
    )
    
    # Reshape and name the confidence intervals
    interaction_ci_reshaped <- as.data.frame(as.list(as.numeric(t(interaction_ci))))
    colnames(interaction_ci_reshaped) <- c(sapply(covariate_names_interaction, function(x) paste0(x, "_LL")), sapply(covariate_names_interaction, function(x) paste0(x, "_UL")))
    
    interaction_condition_ci_values <- rbind(interaction_condition_ci_values, interaction_ci_reshaped)
    
    
    
  }
  
  # Store the results data frame for this condition in the overall results lists
  categorical_results[[condition_name]] <- condition_results_categorical
  categorical_p_values_list[[condition_name]] <- categorical_condition_p_values
  categorical_aic_bic_values_list[[condition_name]] <- categorical_condition_aic_bic
  categorical_ci_values_list[[condition_name]] <- categorical_condition_ci_values
  
  interaction_results[[condition_name]] <- condition_results_interaction
  interaction_p_values_list[[condition_name]] <- interaction_condition_p_values
  interaction_aic_bic_values_list[[condition_name]] <- interaction_condition_aic_bic
  interaction_ci_values_list[[condition_name]] <- interaction_condition_ci_values
  categorical_icc_values_list[[condition_name]] <- categorical_condition_icc_values
  interaction_icc_values_list[[condition_name]] <- interaction_condition_icc_values
  
}




###MAIDHA###

maidha_intercepts_list <- list()
hyp_test_interval <- list()
hyp_test_maidha <- list()
non_converged_instances <- list()

maidha_ci_values_list <- list()
maidha_aic_bic_values_list <- list()

maidha_icc_values_list <- list()


cluster_mapping <- c("11" = "A0B0", "12" = "A0B1", "21" = "A1B0", "22" = "A1B1",
                     "31" = "A2B0", "32" = "A2B1", "41" = "A3B0", "42" = "A3B1",
                     "51" = "A4B0", "52" = "A4B1", "61" = "A5B0", "62" = "A5B1",
                     "71" = "A6B0", "72" = "A6B1")

for (condition_name in names(all_conditions)) {
  
  non_converged_replications <- c()
  
  maidha_condition_icc_values <- c()
  
  condition_ci_values <- data.frame(matrix(ncol = 28, nrow = num_replications))
  maidha_condition_aic_bic <- data.frame(AIC=numeric(), BIC=numeric())
  
  
  
  condition_intercepts <- data.frame(matrix(ncol = 14, nrow = num_replications))
  condition_intervals <- data.frame(matrix(ncol = 28, nrow = num_replications)) 
  condition_hyp_test <- data.frame(matrix(ncol = 14, nrow = num_replications))
  
  
  
  cluster_names <- NULL
  
  for (replication_name in 1:num_replications) {
    
    DL <- all_conditions[[condition_name]][[replication_name]]
    
    conv_check <- FALSE
    
    withCallingHandlers(
      expr = {
        maidha_model <- lmer(y ~ A1 + A2 + A3 + A4 + A5 + A6 + B1 + (1 | cluster) + (1 | school_ID), data = DL)
      },
      warning = function(w) {
        if (grepl("Model failed to converge", w$message)) {
          conv_check <<- TRUE
        }
        invokeRestart("muffleWarning")
      }
    )
    
    if (conv_check) {
      non_converged_replications <- c(non_converged_replications, replication_name)
    }
    var_components_maidha <- VarCorr(maidha_model)
    
    var_random <- attr(var_components_maidha$school_ID, "stddev")[1]^2
    var_residual <- attr(var_components_maidha, "sc")^2
    icc_maidha <- var_random / (var_random + var_residual)
    
    non_converged_instances[[condition_name]] <- non_converged_replications
    maidha_condition_icc_values <- c(maidha_condition_icc_values, icc_maidha)
    
    
    maidha_condition_aic_bic <- rbind(maidha_condition_aic_bic, data.frame(AIC=AIC(maidha_model), BIC=BIC(maidha_model)))
    
    cluster_intercepts <- ranef(maidha_model)$cluster[, 1]
    condition_intercepts[replication_name, ] <- cluster_intercepts
    
    if (is.null(cluster_names)) {
      cluster_names <- rownames(ranef(maidha_model)$cluster)
    }
    
    random_effects <- ranef(maidha_model)$cluster
    postVar <- attr(random_effects, "postVar")
    std_errors <- numeric(nrow(random_effects))
    
    for (i in 1:nrow(random_effects)) {
      std_errors[i] <- sqrt(postVar[, , i])
    }
    
    lower_bound <- cluster_intercepts - 1.96 * std_errors
    upper_bound <- cluster_intercepts + 1.96 * std_errors
    
    condition_intervals[replication_name, seq(1, 28, by = 2)] <- lower_bound
    condition_intervals[replication_name, seq(2, 28, by = 2)] <- upper_bound
    
    # Test whether confidence intervals contain zero
    contains_zero <- ifelse(lower_bound < 0 & upper_bound > 0, 0, 1)
    
    # Store the result in the condition_hyp_test DataFrame
    condition_hyp_test[replication_name, ] <- contains_zero
    
    
    
    vcov_cluster <- VarCorr(maidha_model)$cluster
    se_intercept <- sqrt(vcov_cluster[1, 1])
    z_value <- 1.96 
    
    
    # Compute CI values
    ci_values_LL <- cluster_intercepts - z_value * se_intercept
    ci_values_UL <- cluster_intercepts + z_value * se_intercept
    
    # Combine LL and UL in a single row
    combined_ci_values <- as.numeric(c(ci_values_LL, ci_values_UL))
    condition_ci_values[replication_name, ] <- combined_ci_values
    
    
  }
  
  # Setting column names
  lower_cols <- sapply(cluster_mapping[cluster_names], function(x) paste0("LL_", x))
  upper_cols <- sapply(cluster_mapping[cluster_names], function(x) paste0("UL_", x))
  
  colnames(condition_intercepts) <- colnames(condition_hyp_test) <- cluster_mapping[cluster_names]
  colnames(condition_intervals) <- c(rbind(lower_cols, upper_cols))
  new_colnames <- c(paste0(cluster_mapping[cluster_names], "_LL"), paste0(cluster_mapping[cluster_names], "_UL"))
  
  maidha_intercepts_list[[condition_name]] <- condition_intercepts
  hyp_test_interval[[condition_name]] <- condition_intervals
  hyp_test_maidha[[condition_name]] <- condition_hyp_test
  colnames(condition_ci_values) <- new_colnames
  maidha_aic_bic_values_list[[condition_name]] <- maidha_condition_aic_bic
  maidha_ci_values_list[[condition_name]] <- condition_ci_values  
  
  maidha_icc_values_list[[condition_name]] <- maidha_condition_icc_values
  
  
}



###Outcome Generation####

#Average of estiamtes across 1000 reps 



categorical_avg_coefficients <- data.frame()
interaction_avg_coefficients <- data.frame()

# Iterate through the conditions
for (condition_name in names(categorical_results)) {
  # Retrieve the coefficients for this condition for both models
  categorical_condition_coefficients <- categorical_results[[condition_name]]
  interaction_condition_coefficients <- interaction_results[[condition_name]]
  
  # Calculate the average across the replications for both models
  avg_categorical_coefficients <- colMeans(categorical_condition_coefficients)
  avg_interaction_coefficients <- colMeans(interaction_condition_coefficients)
  
  # Add condition name as a row name and bind to the overall results
  avg_categorical_coefficients <- data.frame(t(avg_categorical_coefficients))
  avg_interaction_coefficients <- data.frame(t(avg_interaction_coefficients))
  rownames(avg_categorical_coefficients) <- condition_name
  rownames(avg_interaction_coefficients) <- condition_name
  categorical_avg_coefficients <- rbind(categorical_avg_coefficients, avg_categorical_coefficients)
  interaction_avg_coefficients <- rbind(interaction_avg_coefficients, avg_interaction_coefficients)
}

colnames(categorical_avg_coefficients) <- colnames(categorical_results[[1]])
colnames(interaction_avg_coefficients) <- colnames(interaction_results[[1]])

#Then get the average for intersections of the interaction model
intersectional_effects <- interaction_avg_coefficients
intersectional_effects$A1B1 <- interaction_avg_coefficients$A1 + interaction_avg_coefficients$B1 + interaction_avg_coefficients$A1B1
intersectional_effects$A2B1 <- interaction_avg_coefficients$A2 + interaction_avg_coefficients$B1 + interaction_avg_coefficients$A2B1
intersectional_effects$A3B1 <- interaction_avg_coefficients$A3 + interaction_avg_coefficients$B1 + interaction_avg_coefficients$A3B1
intersectional_effects$A4B1 <- interaction_avg_coefficients$A4 + interaction_avg_coefficients$B1 + interaction_avg_coefficients$A4B1
intersectional_effects$A5B1 <- interaction_avg_coefficients$A5 + interaction_avg_coefficients$B1 + interaction_avg_coefficients$A5B1
intersectional_effects$A6B1 <- interaction_avg_coefficients$A6 + interaction_avg_coefficients$B1 + interaction_avg_coefficients$A6B1
interaction_int_effects<- intersectional_effects 



##MAIDHA

# Initialize an empty data frame to store the average intercepts for MAIDHA
maidha_avg_intercepts <- data.frame()

# Iterate through the conditions
for (condition_name in names(maidha_intercepts_list)) {
  # Retrieve the intercepts for this condition
  
  
  
  maidha_condition_intercepts <- maidha_intercepts_list[[condition_name]]
  
  # Calculate the average across the replications
  avg_maidha_intercepts <- colMeans(maidha_condition_intercepts)
  
  # Convert to a data frame and set the condition name as a row name
  avg_maidha_intercepts_df <- data.frame(t(avg_maidha_intercepts))
  rownames(avg_maidha_intercepts_df) <- condition_name
  
  # Bind it to the overall results
  maidha_avg_intercepts <- rbind(maidha_avg_intercepts, avg_maidha_intercepts_df)
}

# Set the column names
colnames(maidha_avg_intercepts) <- colnames(maidha_intercepts_list[[1]])


#Average AIC BIC 

#Categorial
# Initialize an empty data frame to store the average AIC and BIC for each condition
average_aic_bic_categorical <- data.frame()

# Iterate through the list to calculate the averages
for (condition_name in names(categorical_aic_bic_values_list)) {
  condition_df <- categorical_aic_bic_values_list[[condition_name]]
  avg_values <- colMeans(condition_df, na.rm = TRUE)  # Calculate the column-wise mean
  avg_values <- as.data.frame(t(avg_values))  # Transpose to row format
  rownames(avg_values) <- condition_name  # Assign condition name as row name
  
  average_aic_bic_categorical <- rbind(average_aic_bic_categorical, avg_values)  # Append to the final dataframe
}


#Interaction

# Initialize an empty data frame to store the average AIC and BIC for each condition
average_aic_bic_interaction <- data.frame()

# Iterate through the list to calculate the averages
for (condition_name in names(interaction_aic_bic_values_list)) {
  condition_df <- interaction_aic_bic_values_list[[condition_name]]
  avg_values <- colMeans(condition_df, na.rm = TRUE)  # Calculate the column-wise mean
  avg_values <- as.data.frame(t(avg_values))  # Transpose to row format
  rownames(avg_values) <- condition_name  # Assign condition name as row name
  
  average_aic_bic_interaction <- rbind(average_aic_bic_interaction, avg_values)  # Append to the final dataframe
}

#Maidha

# Initialize an empty data frame to store the average AIC and BIC for each condition
average_aic_bic_maidha <- data.frame()

# Iterate through the list to calculate the averages
for (condition_name in names(maidha_aic_bic_values_list)) {
  condition_df <- maidha_aic_bic_values_list[[condition_name]]
  avg_values <- colMeans(condition_df, na.rm = TRUE)  # Calculate the column-wise mean
  avg_values <- as.data.frame(t(avg_values))  # Transpose to row format
  rownames(avg_values) <- condition_name  # Assign condition name as row name
  
  average_aic_bic_maidha <- rbind(average_aic_bic_maidha, avg_values)  # Append to the final dataframe
}


### Average CI Values 

# Initialize an empty list to hold the average CI values for each condition
avg_categorical_ci_values <- data.frame()

# Loop over each condition to calculate the average UL and LL for each coefficient
for (condition_name in names(categorical_ci_values_list)) {
  condition_data <- categorical_ci_values_list[[condition_name]]
  
  # Calculate the mean of UL and LL for each coefficient
  avg_condition_data <- colMeans(condition_data, na.rm = TRUE)
  
  # Convert to a data frame and set row names as condition_name
  avg_condition_data_df <- as.data.frame(t(avg_condition_data))
  rownames(avg_condition_data_df) <- condition_name
  
  # rbind this row to the overall data frame
  avg_categorical_ci_values <- rbind(avg_categorical_ci_values, avg_condition_data_df)
}

# Assign the column names based on one of the original data frames inside your list
colnames(avg_categorical_ci_values) <- colnames(categorical_ci_values_list[[1]])




#Interaction


intersectional_ci_values_list <- list()

for (condition_name in names(interaction_ci_values_list)) {
  condition_ci_values <- interaction_ci_values_list[[condition_name]]
  
  # Directly create the data frame with the calculated values
  intersectional_effects_ci <- data.frame(
    A1B1_LL = condition_ci_values$A1_LL + condition_ci_values$B1_LL + condition_ci_values$A1B1_LL,
    A1B1_UL = condition_ci_values$A1_UL + condition_ci_values$B1_UL + condition_ci_values$A1B1_UL,
    A2B1_LL = condition_ci_values$A2_LL + condition_ci_values$B1_LL + condition_ci_values$A2B1_LL,
    A2B1_UL = condition_ci_values$A2_UL + condition_ci_values$B1_UL + condition_ci_values$A2B1_UL,
    A3B1_LL = condition_ci_values$A3_LL + condition_ci_values$B1_LL + condition_ci_values$A3B1_LL,
    A3B1_UL = condition_ci_values$A3_UL + condition_ci_values$B1_UL + condition_ci_values$A3B1_UL,
    A4B1_LL = condition_ci_values$A4_LL + condition_ci_values$B1_LL + condition_ci_values$A4B1_LL,
    A4B1_UL = condition_ci_values$A4_UL + condition_ci_values$B1_UL + condition_ci_values$A4B1_UL,
    A5B1_LL = condition_ci_values$A5_LL + condition_ci_values$B1_LL + condition_ci_values$A5B1_LL,
    A5B1_UL = condition_ci_values$A5_UL + condition_ci_values$B1_UL + condition_ci_values$A5B1_UL,
    A6B1_LL = condition_ci_values$A6_LL + condition_ci_values$B1_LL + condition_ci_values$A6B1_LL,
    A6B1_UL = condition_ci_values$A6_UL + condition_ci_values$B1_UL + condition_ci_values$A6B1_UL
    
  )
  
  intersectional_ci_values_list[[condition_name]] <- intersectional_effects_ci
}


avg_interaction_ci_values_list <- list()

# Loop over each condition to calculate the average UL and LL for each coefficient
for (condition_name in names(intersectional_ci_values_list)) {
  condition_data <- intersectional_ci_values_list[[condition_name]]
  
  # Calculate the mean of UL and LL for each coefficient
  avg_condition_data <- sapply(condition_data, mean, na.rm = TRUE)
  
  # Add this to the list
  avg_interaction_ci_values_list[[condition_name]] <- avg_condition_data
}

# Convert the list of averages to a data frame
avg_interaction_ci_values <- data.frame(matrix(unlist(avg_interaction_ci_values_list), nrow=length(avg_interaction_ci_values_list), byrow=T))

# Assign the column names based on one of the original data frames inside your list
colnames(avg_interaction_ci_values) <- colnames(intersectional_ci_values_list[[1]])

# Assign the row names to be the condition names
rownames(avg_interaction_ci_values) <- names(intersectional_ci_values_list)


# Maidha


# Initialize an empty list to hold the average CI values for each condition
avg_maidha_ci_values_list <- list()

# Loop over each condition to calculate the average UL and LL for each coefficient
for (condition_name in names( maidha_ci_values_list)) {
  condition_data <-  maidha_ci_values_list[[condition_name]]
  
  # Calculate the mean of UL and LL for each coefficient
  avg_condition_data <- sapply(condition_data, mean, na.rm = TRUE)
  
  # Add this to the list
  avg_maidha_ci_values_list[[condition_name]] <- avg_condition_data
}

# Convert the list of averages to a data frame
avg_maidha_ci_values <- data.frame(matrix(unlist(avg_maidha_ci_values_list), nrow=length(avg_maidha_ci_values_list), byrow=T))

# Assign the column names based on one of the original data frames inside your list
colnames(avg_maidha_ci_values) <- colnames(maidha_ci_values_list[[1]])

# Assign the row names to be the condition names
rownames(avg_maidha_ci_values) <- names(maidha_ci_values_list)



####Average P values##

# Initialize an empty list to hold the average p-values for each condition
avg_categorical_p_values_list <- list()

# Loop over each condition to calculate the average p-values for each coefficient
for (condition_name in names(categorical_p_values_list)) {
  condition_data_p <- categorical_p_values_list[[condition_name]]
  
  # Calculate the mean of p-values for each coefficient
  avg_condition_data_p <- sapply(condition_data_p, mean, na.rm = TRUE)
  
  # Add this to the list
  avg_categorical_p_values_list[[condition_name]] <- avg_condition_data_p
}

# Convert the list of averages to a data frame
avg_categorical_p_values <- data.frame(matrix(unlist(avg_categorical_p_values_list), nrow=length(avg_categorical_p_values_list), byrow=T))

# Assign the column names based on one of the original data frames inside your list
colnames(avg_categorical_p_values) <- colnames(categorical_p_values_list[[1]])

# Assign the row names to be the condition names
rownames(avg_categorical_p_values) <- names(categorical_p_values_list)

# Remove the '(Intercept)' column from avg_categorical_p_values
avg_categorical_p_values <- avg_categorical_p_values[, colnames(avg_categorical_p_values) != "(Intercept)"]





#Interaction
# Remove specified columns from each data frame in the list
interaction_p_values_list_cleaned <- lapply(interaction_p_values_list, function(df) {
  df[, !colnames(df) %in% c("(Intercept)", "A1", "A2", "A3", "A4", "A5", "A6", "B1")]
})


# Initialize an empty list to hold the average p-values for each condition
avg_interaction_p_values_list <- list()

# Loop over each condition to calculate the average p-values for each coefficient
for (condition_name in names(interaction_p_values_list_cleaned)) {
  condition_data_p <- interaction_p_values_list_cleaned[[condition_name]]
  
  # Calculate the mean of p-values for each coefficient
  avg_condition_data_p <- sapply(condition_data_p, mean, na.rm = TRUE)
  
  # Add this to the list
  avg_interaction_p_values_list[[condition_name]] <- avg_condition_data_p
}

# Convert the list of averages to a data frame
avg_interaction_p_values <- data.frame(matrix(unlist(avg_interaction_p_values_list), nrow=length(avg_interaction_p_values_list), byrow=T))

# Assign the column names based on one of the original data frames inside your list
colnames(avg_interaction_p_values) <- colnames(interaction_p_values_list_cleaned[[1]])

# Assign the row names to be the condition names
rownames(avg_interaction_p_values) <- names(interaction_p_values_list_cleaned)

# Remove the '(Intercept)' column from avg_categorical_p_values
avg_interaction_p_values <- avg_interaction_p_values[, colnames(avg_interaction_p_values) != "(Intercept)"]


# Maidha 
# Initialize an empty dataframe to hold the results
interval_means_maidha <- data.frame()

# Loop through each condition
for (condition_name in names(hyp_test_interval)) {
  condition_data <- hyp_test_interval[[condition_name]]
  
  # Extract the lower bounds and upper bounds separately
  lower_bounds <- condition_data[, seq(1, 28, by = 2)]
  upper_bounds <- condition_data[, seq(2, 28, by = 2)]
  
  # Calculate the mean of each interval for each replication
  interval_means <- (lower_bounds + upper_bounds) / 2
  
  # Calculate the average of these interval means across all replications
  avg_interval_means <- colMeans(interval_means, na.rm = TRUE)
  
  # Create a dataframe with the condition name and calculated averages
  condition_result <- data.frame(condition = condition_name, t(avg_interval_means))
  
  # Append this to the overall result dataframe
  interval_means_maidha <- rbind(interval_means_maidha, condition_result)
}

# Making the first column as rownames
rownames(interval_means_maidha) <- interval_means_maidha$condition
interval_means_maidha$condition <- NULL

# Replace the column names
colnames(interval_means_maidha) <- colnames(hyp_test_maidha[[1]])





###SEB####


# Create an empty data frame with the updated column names
seb_categorical_df <- data.frame(matrix(ncol = ncol(categorical_avg_coefficients), nrow = 0))
colnames(seb_categorical_df) <- colnames(categorical_avg_coefficients)
rownames(seb_categorical_df) <- NULL  # Reset once initially

# Initialize a variable to hold row names
row_name_list <- c()


# Iterate through the conditions for the categorical model
for (condition_name in rownames(categorical_avg_coefficients)) {
  
  # Retrieve the coefficients for this condition
  categorical_condition_coefficients <- categorical_results[[condition_name]][, colnames(categorical_avg_coefficients), drop = FALSE]
  avg_categorical_coefficients <- as.numeric(categorical_avg_coefficients[condition_name,]) # Convert to a numeric vector
  
  # Compute SEB for each coefficient
  seb <- sapply(seq_along(avg_categorical_coefficients), function(i) {
    coef <- categorical_condition_coefficients[,i]
    sqrt(sum((coef - avg_categorical_coefficients[i])^2) / length(coef))
  })
  
  # Create a one-row data frame for the new SEB values
  seb_df_to_append <- data.frame(t(seb))
  colnames(seb_df_to_append) <- colnames(categorical_avg_coefficients)
  
  # Append the SEB values to the data frame
  seb_categorical_df <- rbind(seb_categorical_df, seb_df_to_append)
  
  # Update row names
  row_name_list <- c(row_name_list, condition_name)
}

# Finally, set the row names for the data frame
rownames(seb_categorical_df) <- row_name_list
seb_categorical_df <- seb_categorical_df %>% 
  select(-c('(Intercept)'))






# Print the final data frame
seb_interaction <- list()



# Remove specified columns
intersectional_effects <- intersectional_effects %>% 
  select(-c(A1, A2, A3, A4, A5, A6, B1))

# Iterate through the conditions for interaction model
for (condition_name in names(interaction_results)) {
  # Retrieve the coefficients for this condition
  interaction_condition_coefficients <- interaction_results[[condition_name]]
  avg_interaction_coefficients <- interaction_avg_coefficients[condition_name,]
  
  # Create intersectional effect coefficients
  intersectional_effects <- interaction_condition_coefficients
  intersectional_effects$A1B1 <- interaction_condition_coefficients$A1 + interaction_condition_coefficients$B1 + interaction_condition_coefficients$A1B1
  intersectional_effects$A2B1 <- interaction_condition_coefficients$A2 + interaction_condition_coefficients$B1 + interaction_condition_coefficients$A2B1
  intersectional_effects$A3B1 <- interaction_condition_coefficients$A3 + interaction_condition_coefficients$B1 + interaction_condition_coefficients$A3B1  
  intersectional_effects$A4B1 <- interaction_condition_coefficients$A4 + interaction_condition_coefficients$B1 + interaction_condition_coefficients$A4B1
  intersectional_effects$A5B1 <- interaction_condition_coefficients$A5 + interaction_condition_coefficients$B1 + interaction_condition_coefficients$A5B1
  intersectional_effects$A6B1 <- interaction_condition_coefficients$A6 + interaction_condition_coefficients$B1 + interaction_condition_coefficients$A6B1
  
  # Keep only intersectional effects
  intersectional_effects <- intersectional_effects[, !(colnames(intersectional_effects) %in% c('(Intercept)', 'A1', 'A2', 'A3', 'A4', 'A5', 'A6', 'B1'))]
  
  # Compute SEB for each intersectional effect
  seb <- apply(intersectional_effects, 2, function(coef) {
    sqrt(sum((coef - avg_interaction_coefficients)^2) / length(coef))
  })
  
  # Store the SEB values
  seb_interaction[[condition_name]] <- seb
}

# Convert the SEB lists to data frames
seb_interaction_df <- do.call(rbind, seb_interaction)

# Ensure the columns are named appropriately
colnames(seb_interaction_df) <- colnames(intersectional_effects)

seb_interaction_df <- as.data.frame(seb_interaction_df)





# Initialize a list to store the SEB values for each condition in the MAIDHA model
seb_maidha <- list()

# Iterate through the conditions for MAIDHA model
for (condition_name in names(maidha_intercepts_list)) {
  # Retrieve the intercepts for this condition
  maidha_condition_intercepts <- maidha_intercepts_list[[condition_name]]
  
  # Retrieve the average intercepts from the previously computed maidha_avg_intercepts
  avg_maidha_intercepts <- maidha_avg_intercepts[condition_name,]
  
  # Compute SEB for each cluster intercept
  seb <- apply(maidha_condition_intercepts, 2, function(intercept) {
    sqrt(sum((intercept - avg_maidha_intercepts)^2) / length(intercept))
  })
  
  # Store the SEB values in the list
  seb_maidha[[condition_name]] <- seb
}

# Convert the SEB list to a data frame
seb_maidha_df <- do.call(rbind, seb_maidha)

# Ensure the columns are named appropriately
colnames(seb_maidha_df) <- colnames(maidha_intercepts_list[[1]])

seb_maidha_df <- as.data.frame(seb_maidha_df)




######Bias########


#categorical

true_values_categorical <- c(bA1B0, bA2B0, bA3B0, bA4B0, bA5B0, bA6B0, bA0B1, bA1B1,
                             bA2B1, bA3B1, bA4B1, bA5B1, bA6B1)

categorical_avg_coefficients <- categorical_avg_coefficients[, !(colnames(categorical_avg_coefficients) %in% c('(Intercept)'))]


bias_categorical <- sweep(categorical_avg_coefficients, 2, unlist(true_values_categorical), "-")

#interaction

true_values_interaction <- c(bA1B1, bA2B1, bA3B1, bA4B1, bA5B1, bA6B1) # Replace VALUE with the actual true values


columns_to_remove <- c('(Intercept)', 'A1', 'A2', 'A3', 'A4', 'A5', 'A6', 'B1')
interaction_int_effects <- interaction_int_effects[, !(colnames(interaction_int_effects) %in% columns_to_remove)]

# Calculate bias for intersectional effects
bias_interaction <- sweep(interaction_int_effects, 2, unlist(true_values_interaction), "-")

#Maidha

true_values_maidha<- c(mA0B0, mA0B1, mA1B0, mA1B1, mA2B0, mA2B1, mA3B0, mA3B1, 
                       mA4B0, mA4B1, mA5B0, mA5B1, mA6B0, mA6B1)


bias_maidha<- sweep(maidha_avg_intercepts, 2, unlist(true_values_maidha), "-")


####Accuracy##############
# Categorical
accuracy_categorical <- bias_categorical^2 + seb_categorical_df^2

# Interaction
accuracy_interaction <- bias_interaction^2 + seb_interaction_df^2


# Maidha

accuracy_maidha<-bias_maidha^2 + seb_maidha_df^2


#####Coverage########

# Initialize an empty data frame to store overall coverage percentages for all conditions and coefficients
overall_coverage_percentages_categorical <- data.frame()
coverage_results_categorical<-list()
# Iterate through the conditions
for (condition_name in names(categorical_ci_values_list)) {
  
  # Extract the CI values for this condition
  condition_ci_values <- categorical_ci_values_list[[condition_name]]
  
  # Initialize the data frame to store the coverage results for this condition
  condition_coverage <- data.frame(
    # Add the coefficients for the categorical model
    A1B0 = ifelse(bA1B0 > condition_ci_values$A1B0_LL & bA1B0 < condition_ci_values$A1B0_UL, 1, 0),
    A2B0 = ifelse(bA2B0 > condition_ci_values$A2B0_LL & bA2B0 < condition_ci_values$A2B0_UL, 1, 0),
    A3B0 = ifelse(bA3B0 > condition_ci_values$A3B0_LL & bA3B0 < condition_ci_values$A3B0_UL, 1, 0),
    A4B0 = ifelse(bA4B0 > condition_ci_values$A4B0_LL & bA4B0 < condition_ci_values$A4B0_UL, 1, 0),
    A5B0 = ifelse(bA5B0 > condition_ci_values$A5B0_LL & bA5B0 < condition_ci_values$A5B0_UL, 1, 0),
    A6B0 = ifelse(bA6B0 > condition_ci_values$A6B0_LL & bA6B0 < condition_ci_values$A6B0_UL, 1, 0),
    A0B1 = ifelse(bA0B1 > condition_ci_values$A0B1_LL & bA0B1 < condition_ci_values$A0B1_UL, 1, 0),
    A1B1 = ifelse(bA1B1 > condition_ci_values$A1B1_LL & bA1B1 < condition_ci_values$A1B1_UL, 1, 0),
    A2B1 = ifelse(bA2B1 > condition_ci_values$A2B1_LL & bA2B1 < condition_ci_values$A2B1_UL, 1, 0),
    A3B1 = ifelse(bA3B1 > condition_ci_values$A3B1_LL & bA3B1 < condition_ci_values$A3B1_UL, 1, 0),
    A4B1 = ifelse(bA4B1 > condition_ci_values$A4B1_LL & bA4B1 < condition_ci_values$A4B1_UL, 1, 0),
    A5B1 = ifelse(bA5B1 > condition_ci_values$A5B1_LL & bA5B1 < condition_ci_values$A5B1_UL, 1, 0),
    A6B1 = ifelse(bA6B1 > condition_ci_values$A6B1_LL & bA6B1 < condition_ci_values$A6B1_UL, 1, 0)
  )
  
  # Store the coverage results data frame for this condition in the overall results list
  coverage_results_categorical[[condition_name]] <- condition_coverage
  
  # Calculate the overall % coverage for each coefficient
  coverage_percentages <- sapply(condition_coverage, function(col) sum(col) / length(col) * 100)
  
  # Convert to data frame and set row name to the condition name
  coverage_percentages_df <- as.data.frame(t(coverage_percentages))
  rownames(coverage_percentages_df) <- condition_name
  
  # Combine with the overall data frame
  overall_coverage_percentages_categorical <- rbind(overall_coverage_percentages_categorical, coverage_percentages_df)
}




# Initialize the list to store coverage tables for each condition
coverage_results_interaction <- list()

# Initialize an empty data frame to store overall coverage percentages for all conditions and coefficients
overall_coverage_percentages_interaction <- data.frame()

# Iterate through the conditions
for (condition_name in names(intersectional_ci_values_list)) {
  
  # Extract the CI values for this condition
  condition_ci_values <- intersectional_ci_values_list[[condition_name]]
  
  # Initialize the data frame to store the coverage results for this condition
  condition_coverage <- data.frame(
    A1B1 = ifelse(bA1B1 > condition_ci_values$A1B1_LL & bA1B1 < condition_ci_values$A1B1_UL, 1, 0),
    A2B1 = ifelse(bA2B1 > condition_ci_values$A2B1_LL & bA2B1 < condition_ci_values$A2B1_UL, 1, 0),
    A3B1 = ifelse(bA3B1 > condition_ci_values$A3B1_LL & bA3B1 < condition_ci_values$A3B1_UL, 1, 0),
    A4B1 = ifelse(bA4B1 > condition_ci_values$A4B1_LL & bA4B1 < condition_ci_values$A4B1_UL, 1, 0),
    A5B1 = ifelse(bA5B1 > condition_ci_values$A5B1_LL & bA5B1 < condition_ci_values$A5B1_UL, 1, 0),
    A6B1 = ifelse(bA6B1 > condition_ci_values$A6B1_LL & bA6B1 < condition_ci_values$A6B1_UL, 1, 0)
  )
  
  # Store the coverage results data frame for this condition in the overall results list
  coverage_results_interaction[[condition_name]] <- condition_coverage
  
  # Calculate the overall % coverage for each coefficient
  coverage_percentages <- sapply(condition_coverage, function(col) sum(col) / length(col) * 100)
  
  # Convert to data frame and set row name to the condition name
  coverage_percentages_df <- as.data.frame(t(coverage_percentages))
  rownames(coverage_percentages_df) <- condition_name
  
  # Combine with the overall data frame
  overall_coverage_percentages_interaction <- rbind(overall_coverage_percentages_interaction, coverage_percentages_df)
}





# MAidha


# Initialize an empty data frame to store overall coverage percentages for all conditions and coefficients
overall_coverage_percentages_maidha <- data.frame()
coverage_results_maidha<-list()
# Iterate through the conditions
for (condition_name in names(maidha_ci_values_list)) {
  
  # Extract the CI values for this condition
  condition_ci_values <- maidha_ci_values_list[[condition_name]]
  
  # Initialize the data frame to store the coverage results for this condition
  condition_coverage <- data.frame(
    # Add the coefficients for the categorical model
    A0B0 = ifelse(mA0B0 > condition_ci_values$A0B0_LL & mA0B0 < condition_ci_values$A0B0_UL, 1, 0),
    A1B0 = ifelse(mA1B0 > condition_ci_values$A1B0_LL & mA1B0 < condition_ci_values$A1B0_UL, 1, 0),
    A2B0 = ifelse(mA2B0 > condition_ci_values$A2B0_LL & mA2B0 < condition_ci_values$A2B0_UL, 1, 0),
    A3B0 = ifelse(mA3B0 > condition_ci_values$A3B0_LL & mA3B0 < condition_ci_values$A3B0_UL, 1, 0),
    A4B0 = ifelse(mA4B0 > condition_ci_values$A4B0_LL & mA4B0 < condition_ci_values$A4B0_UL, 1, 0),
    A5B0 = ifelse(mA5B0 > condition_ci_values$A5B0_LL & mA5B0 < condition_ci_values$A5B0_UL, 1, 0),
    A6B0 = ifelse(mA6B0 > condition_ci_values$A6B0_LL & mA6B0 < condition_ci_values$A6B0_UL, 1, 0),
    A1B1 = ifelse(mA1B1 > condition_ci_values$A1B1_LL & mA1B1 < condition_ci_values$A1B1_UL, 1, 0),
    A2B1 = ifelse(mA2B1 > condition_ci_values$A2B1_LL & mA2B1 < condition_ci_values$A2B1_UL, 1, 0),
    A3B1 = ifelse(mA3B1 > condition_ci_values$A3B1_LL & mA3B1 < condition_ci_values$A3B1_UL, 1, 0),
    A4B1 = ifelse(mA4B1 > condition_ci_values$A4B1_LL & mA4B1 < condition_ci_values$A4B1_UL, 1, 0),
    A5B1 = ifelse(mA5B1 > condition_ci_values$A5B1_LL & mA5B1 < condition_ci_values$A5B1_UL, 1, 0),
    A6B1 = ifelse(mA6B1 > condition_ci_values$A6B1_LL & mA6B1 < condition_ci_values$A6B1_UL, 1, 0)
  )
  
  # Store the coverage results data frame for this condition in the overall results list
  coverage_results_maidha[[condition_name]] <- condition_coverage
  
  # Calculate the overall % coverage for each coefficient
  coverage_percentages <- sapply(condition_coverage, function(col) sum(col) / length(col) * 100)
  
  # Convert to data frame and set row name to the condition name
  coverage_percentages_df <- as.data.frame(t(coverage_percentages))
  rownames(coverage_percentages_df) <- condition_name
  
  # Combine with the overall data frame
  overall_coverage_percentages_maidha <- rbind(overall_coverage_percentages_maidha, coverage_percentages_df)
}





#######Power and Type 1 error#########


#Categorical

# Initialize an empty data frame to store the final percentages
overall_percentages_df <- data.frame()

# Extract condition names
condition_names <- names(categorical_p_values_list)

# Loop through each condition
for (condition_name in condition_names) {
  
  # Get the data frame corresponding to the current condition
  condition_df <- categorical_p_values_list[[condition_name]]
  
  # Calculate the percentage of p-values less than 0.05 for each coefficient
  percentage_vector <- colMeans(condition_df < 0.05, na.rm = TRUE) * 100
  
  # Append this to the overall data frame
  overall_percentages_df <- rbind(overall_percentages_df, as.data.frame(t(percentage_vector)))
}

# Assign the row names and column names to the overall data frame
rownames(overall_percentages_df) <- condition_names
colnames(overall_percentages_df) <- colnames(categorical_p_values_list[[1]])

p_value_percent_categorical<-overall_percentages_df
p_value_percent_categorical <- p_value_percent_categorical[, !(colnames(p_value_percent_categorical) == "(Intercept)")]





# Interaction



# Initialize another empty data frame to store the final percentages for interactions
interaction_overall_percentages_df <- data.frame()

# Remove unwanted columns from all dataframes in the list
interaction_p_values_list_cleaned <- lapply(interaction_p_values_list, function(df) {
  df[, !colnames(df) %in% c("A1","A2","A3","A4","A5","A6","B1")]
})

# Extract condition names for interactions
interaction_condition_names <- names(interaction_p_values_list_cleaned)

# Loop through each condition for interactions
for (interaction_condition_name in interaction_condition_names) {
  
  # Get the data frame corresponding to the current interaction condition
  interaction_condition_df <- interaction_p_values_list_cleaned[[interaction_condition_name]]
  
  # Calculate the percentage of p-values less than 0.05 for each coefficient
  interaction_percentage_vector <- colMeans(interaction_condition_df < 0.05, na.rm = TRUE) * 100
  
  # Append this to the overall data frame for interactions
  interaction_overall_percentages_df <- rbind(interaction_overall_percentages_df, as.data.frame(t(interaction_percentage_vector)))
}

# Assign the row names and column names to the overall data frame for interactions
rownames(interaction_overall_percentages_df) <- interaction_condition_names
colnames(interaction_overall_percentages_df) <- colnames(interaction_p_values_list_cleaned[[1]])

p_value_percent_interaction <- interaction_overall_percentages_df[, !(colnames(interaction_overall_percentages_df) == "(Intercept)")]




#Maidha 


# Initialize an empty dataframe to hold the results
sig_percent_maidha <- data.frame()

# Loop through each condition
for (condition_name in names(hyp_test_maidha)) {
  condition_data <- hyp_test_maidha[[condition_name]]
  
  # Calculate the percentage of times each coefficient has a "1"
  percentage_ones <- colMeans(condition_data, na.rm = TRUE) * 100  # Assuming that NAs should be removed
  
  # Create a dataframe with the condition name and calculated percentages
  condition_result <- data.frame(condition = condition_name, t(percentage_ones))
  
  # Append this to the overall result dataframe
  sig_percent_maidha <- rbind(sig_percent_maidha, condition_result)
}

# Making the first column as rownames
rownames(sig_percent_maidha) <- sig_percent_maidha$condition
sig_percent_maidha$condition <- NULL









######Flagging Values#############



####Bias#####

#Categorical

# Initialize flagged_bias_categorical with the same dimensions as bias_categorical
flagged_extremebias_categorical <- as.data.frame(matrix(0, nrow=nrow(bias_categorical), ncol=ncol(bias_categorical)))
colnames(flagged_extremebias_categorical) <- colnames(bias_categorical)
rownames(flagged_extremebias_categorical) <- rownames(bias_categorical)  # Retain the row names

for (i in 1:nrow(bias_categorical)) {
  for (j in 1:ncol(bias_categorical)) {
    if (is.na(bias_categorical[i, j]) || is.na(seb_categorical_df[i, j])) {
      flagged_extremebias_categorical[i, j] <- NA
    } else {
      if (abs(bias_categorical[i, j]) >= 2 * seb_categorical_df[i, j]) {
        flagged_extremebias_categorical[i, j] <- 1
      }
    }
  }
}



flagged_modbias_categorical <- as.data.frame(matrix(0, nrow=nrow(bias_categorical), ncol=ncol(bias_categorical)))
colnames(flagged_modbias_categorical) <- colnames(bias_categorical)
rownames(flagged_modbias_categorical) <- rownames(bias_categorical)  # Retain the row names

for (i in 1:nrow(bias_categorical)) {
  for (j in 1:ncol(bias_categorical)) {
    if (is.na(bias_categorical[i, j]) || is.na(seb_categorical_df[i, j])) {
      flagged_modbias_categorical[i, j] <- NA
    } else {
      if (abs(bias_categorical[i, j]) >= .5 * seb_categorical_df[i, j] &&
          abs(bias_categorical[i, j]) <2 * seb_categorical_df[i, j]) {
        flagged_modbias_categorical[i, j] <- 1 
      }
    }
  }
}


flagged_bias_categorical <- flagged_extremebias_categorical + flagged_modbias_categorical

flagged_bias_categorical[flagged_bias_categorical > 1] <- 1

# Interaction
flagged_extremebias_interaction <- as.data.frame(matrix(0, nrow=nrow(bias_interaction), ncol=ncol(bias_interaction)))
colnames(flagged_extremebias_interaction) <- colnames(bias_interaction)
rownames(flagged_extremebias_interaction) <- rownames(bias_interaction)  


for (i in 1:nrow(bias_interaction)) {
  for (j in 1:ncol(bias_interaction)) {
    if (is.na(bias_interaction[i, j]) || is.na(seb_interaction_df[i, j])) {
      flagged_bias_categorical[i, j] <- NA
    } else {
      if (abs(bias_interaction[i, j]) >= 2 * seb_interaction_df[i, j]) {
        flagged_extremebias_interaction[i, j] <- 1
      }
    }
  }
}



flagged_modbias_interaction <- as.data.frame(matrix(0, nrow=nrow(bias_interaction), ncol=ncol(bias_interaction)))
colnames(flagged_modbias_interaction) <- colnames(bias_interaction)
rownames(flagged_modbias_interaction) <- rownames(bias_interaction)  # Retain the row names

for (i in 1:nrow(bias_interaction)) {
  for (j in 1:ncol(bias_interaction)) {
    if (is.na(bias_interaction[i, j]) || is.na(seb_interaction_df[i, j])) {
      flagged_modbias_interaction[i, j] <- NA
    } else {
      if (abs(bias_interaction[i, j]) >= .5 * seb_interaction_df[i, j] &&
          abs(bias_interaction[i, j]) <2 * seb_interaction_df[i, j]) {
        flagged_modbias_interaction[i, j] <- 1 
      }
    }
  }
}


flagged_bias_interaction <- flagged_extremebias_interaction + flagged_modbias_interaction

flagged_bias_interaction[flagged_bias_interaction > 1] <- 1


#Maidha

# Initialize flagged_bias_categorical with the same dimensions as bias_categorical
flagged_extremebias_maidha <- as.data.frame(matrix(0, nrow=nrow(bias_maidha), ncol=ncol(bias_maidha)))
colnames(flagged_extremebias_maidha) <- colnames(bias_maidha)
rownames(flagged_extremebias_maidha) <- rownames(bias_maidha)  # Retain the row names


for (i in 1:nrow(bias_maidha)) {
  for (j in 1:ncol(bias_maidha)) {
    if (is.na(bias_maidha[i, j]) || is.na(seb_maidha_df[i, j])) {
      flagged_bias_maidha[i, j] <- NA
    } else {
      if (abs(bias_maidha[i, j]) >= 2 * seb_maidha_df[i, j]) {
        flagged_extremebias_maidha[i, j] <- 1
      }
    }
  }
}

flagged_modbias_maidha <- as.data.frame(matrix(0, nrow=nrow(bias_maidha), ncol=ncol(bias_maidha)))
colnames(flagged_modbias_maidha) <- colnames(bias_maidha)
rownames(flagged_modbias_maidha) <- rownames(bias_maidha)  # Retain the row names

for (i in 1:nrow(bias_maidha)) {
  for (j in 1:ncol(bias_maidha)) {
    if (is.na(bias_maidha[i, j]) || is.na(seb_maidha_df[i, j])) {
      flagged_modbias_maidha[i, j] <- NA
    } else {
      if (abs(bias_maidha[i, j]) >= .5 * seb_maidha_df[i, j] &&
          abs(bias_maidha[i, j]) <2 * seb_maidha_df[i, j]) {
        flagged_modbias_maidha[i, j] <- 1 
      }
    }
  }
}


flagged_bias_maidha<- flagged_extremebias_maidha + flagged_modbias_maidha

flagged_bias_maidha[flagged_bias_maidha > 1] <- 1

#####Acccuracy#### 

#Categorical
flagged_accuracy_categorical <- as.data.frame(matrix(0, nrow=nrow(accuracy_categorical), ncol=ncol(accuracy_categorical)))
colnames(flagged_accuracy_categorical) <- colnames(accuracy_categorical)
rownames(flagged_accuracy_categorical) <- rownames(accuracy_categorical)


# Flag cells where the condition is met
for (i in 1:nrow(accuracy_categorical)) {
  for (j in 1:ncol(accuracy_categorical)) {
    if (is.na(accuracy_categorical[i, j]) || is.na(seb_categorical_df[i, j])) {
      flagged_accuracy_categorical[i, j] <- NA
    } else{
      if (accuracy_categorical[i, j] > 0.5) {
        flagged_accuracy_categorical[i, j] <- 1
      }
    }
  }
}

#Interaction


flagged_accuracy_interaction <- as.data.frame(matrix(0, nrow=nrow(accuracy_interaction), ncol=ncol(accuracy_interaction)))
colnames(flagged_accuracy_interaction) <- colnames(accuracy_interaction)
rownames(flagged_accuracy_interaction) <- rownames(accuracy_interaction)  # Retain the row names

# Flag cells where the condition is met
for (i in 1:nrow(accuracy_interaction)) {
  for (j in 1:ncol(accuracy_interaction)) {
    if (is.na(accuracy_interaction[i, j]) || is.na(seb_interaction_df[i, j])) {
      flagged_accuracy_interaction[i, j] <- NA
    } else{
      if (accuracy_interaction[i, j] > 0.5) {
        flagged_accuracy_interaction[i, j] <- 1
      }
    }
  }
}

#Maidha


flagged_accuracy_maidha <- as.data.frame(matrix(0, nrow=nrow(accuracy_maidha), ncol=ncol(accuracy_maidha)))
colnames(flagged_accuracy_maidha) <- colnames(accuracy_maidha)
rownames(flagged_accuracy_maidha) <- rownames(accuracy_maidha)  # Retain the row names

# Flag cells where the condition is met
for (i in 1:nrow(accuracy_maidha)) {
  for (j in 1:ncol(accuracy_maidha)) {
    if (is.na(accuracy_maidha[i, j]) || is.na(seb_maidha_df[i, j])) {
      flagged_accuracy_maidha[i, j] <- NA
    } else{
      if (accuracy_maidha[i, j] > 0.5) {
        flagged_accuracy_maidha[i, j] <- 1
      }
    }
  }
}


#####Power and Type 1 Error #######


#Categorical

# Initialize flagged_power_error_categorical table
flagged_power_error_categorical <- as.data.frame(matrix(0, nrow = nrow(p_value_percent_categorical), ncol = ncol(p_value_percent_categorical)))
colnames(flagged_power_error_categorical) <- colnames(p_value_percent_categorical)
rownames(flagged_power_error_categorical) <- rownames(p_value_percent_categorical)  # Retain the row names

# Coefficients that should have a true effect
true_effect_cols_categorical <- c("A1B0", "A2B0", "A3B0", "A1B1", "A2B1")

# Coefficients that should not have a true effect
no_effect_cols_categorical <- c("A4B0", "A5B0", "A6B0", "A0B1", "A3B1", "A4B1", "A5B1", "A6B1")

# Flag values for insufficient power (< 80%)
for (col in true_effect_cols_categorical) {
  flagged_power_error_categorical[, col] <- ifelse(p_value_percent_categorical[, col] < 80, 1, 0)
}

# Flag values for too much type 1 error (> 5%)
for (col in no_effect_cols_categorical) {
  flagged_power_error_categorical[, col] <- ifelse(p_value_percent_categorical[, col] > 5, 1, 0)
}



#Interaction



# Initialize flagged_power_error_categorical table
flagged_power_error_interaction <- as.data.frame(matrix(0, nrow = nrow(interaction_overall_percentages_df), ncol = ncol(interaction_overall_percentages_df)))
colnames(flagged_power_error_interaction) <- colnames(interaction_overall_percentages_df)
rownames(flagged_power_error_interaction) <- rownames(interaction_overall_percentages_df)  # Retain the row names

# Coefficients that should have a true effect
true_effect_cols_interaction <- c("A1B1", "A2B1")

# Coefficients that should not have a true effect
no_effect_cols_interaction <- c("A3B1", "A4B1", "A5B1", "A6B1")

# Flag values for insufficient power (< 80%)
for (col in true_effect_cols_interaction) {
  flagged_power_error_interaction[, col] <- ifelse(interaction_overall_percentages_df[, col] < 80, 1, 0)
}

# Flag values for too much type 1 error (> 5%)
for (col in no_effect_cols_interaction) {
  flagged_power_error_interaction[, col] <- ifelse(interaction_overall_percentages_df[, col] > 5, 1, 0)
}





#Maidha 

flagged_power_error_maidha<- as.data.frame(matrix(0, nrow = nrow(sig_percent_maidha), ncol = ncol(sig_percent_maidha)))
colnames(flagged_power_error_maidha) <- colnames(sig_percent_maidha)
rownames(flagged_power_error_maidha) <- rownames(sig_percent_maidha)  # Retain the row names

# Coefficients that should have a true effect
true_effect_cols_maidha <- c("A1B0", "A2B0", "A3B0", "A1B1", "A2B1")

# Coefficients that should not have a true effect
no_effect_cols_maidha <- c("A0B0", "A4B0", "A5B0", "A6B0", "A0B1", "A3B1", "A4B1", "A5B1", "A6B1")

# Flag values for insufficient power (< 80%)
for (col in true_effect_cols_maidha) {
  flagged_power_error_maidha[, col] <- ifelse(sig_percent_maidha[, col] < 80, 1, 0)
}

# Flag values for too much type 1 error (> 5%)
for (col in no_effect_cols_maidha) {
  flagged_power_error_maidha[, col] <- ifelse(sig_percent_maidha[, col] > 5, 1, 0)
}






############Coverage##############

# Categorical 


flagged_coverage_categorical <- data.frame(
  Flag = ifelse(overall_coverage_percentages_categorical < 92.5, 1, 0)
)

# To keep row names
rownames(flagged_coverage_categorical) <- rownames(overall_coverage_percentages_categorical)


flagged_uprcoverage_categorical <- data.frame(
  Flag = ifelse(overall_coverage_percentages_categorical > 97.5, 1, 0)
)

rownames(flagged_uprcoverage_categorical) <- rownames(overall_coverage_percentages_categorical)

flagged_fullcoverage_categorical<- flagged_coverage_categorical + flagged_uprcoverage_categorical

flagged_fullcoverage_categorical[flagged_fullcoverage_categorical > 1] <- 1




#Interaction


flagged_coverage_interaction <- data.frame(
  Flag = ifelse(overall_coverage_percentages_interaction < 92.5, 1, 0)
)

# To keep row names
rownames(flagged_coverage_interaction) <- rownames(overall_coverage_percentages_interaction)

flagged_uprcoverage_interaction <- data.frame(
  Flag = ifelse(overall_coverage_percentages_interaction > 97.5, 1, 0)
)

rownames(flagged_uprcoverage_interaction) <- rownames(overall_coverage_percentages_interaction)

flagged_fullcoverage_interaction<- flagged_coverage_interaction + flagged_uprcoverage_interaction

flagged_fullcoverage_interaction[flagged_fullcoverage_interaction > 1] <- 1


#Maidha

flagged_coverage_maidha <- data.frame(
  Flag = ifelse(overall_coverage_percentages_maidha < 92.5, 1, 0)
)

rownames(flagged_coverage_maidha) <- rownames(overall_coverage_percentages_maidha)

flagged_uprcoverage_maidha<- data.frame(
  Flag = ifelse(overall_coverage_percentages_maidha > 97.5, 1, 0)
)

rownames(flagged_uprcoverage_maidha) <- rownames(overall_coverage_percentages_maidha)

flagged_fullcoverage_maidha<- flagged_coverage_maidha + flagged_uprcoverage_maidha

flagged_fullcoverage_maidha[flagged_fullcoverage_maidha> 1] <- 1



####### Summaries of Flags #######


#Categorical 

# Sum across the rows to get the number of flags for each condition in each table

sum_flagged_modbias <- rowSums(flagged_modbias_categorical, na.rm = TRUE)
sum_flagged_extremebias <- rowSums(flagged_extremebias_categorical, na.rm = TRUE)
sum_flagged_bias <- rowSums(flagged_bias_categorical, na.rm = TRUE)
sum_flagged_accuracy <- rowSums(flagged_accuracy_categorical, na.rm = TRUE)
sum_flagged_coverage <- rowSums(flagged_coverage_categorical, na.rm = TRUE)
sum_flagged_uprcoverage <- rowSums(flagged_uprcoverage_categorical, na.rm = TRUE)
sum_flagged_fullcoverage <- rowSums(flagged_fullcoverage_categorical, na.rm = TRUE)
sum_flagged_power <- rowSums(flagged_power_error_categorical[, true_effect_cols_categorical], na.rm = TRUE)
sum_flagged_error <- rowSums(flagged_power_error_categorical[, no_effect_cols_categorical], na.rm = TRUE)

flag_sum_all_categorical <- data.frame(
  ModBias= sum_flagged_modbias,
  ExtremeBias= sum_flagged_extremebias,
  Bias = sum_flagged_bias,
  Accuracy = sum_flagged_accuracy,
  LwrCoverage = sum_flagged_coverage,
  UprCoverage = sum_flagged_uprcoverage,
  FullCoverage = sum_flagged_fullcoverage,
  Power = sum_flagged_power,
  Type1Error = sum_flagged_error
)


flag_sum_categorical <- data.frame(
  Bias = sum_flagged_bias,
  Accuracy = sum_flagged_accuracy,
  Coverage = sum_flagged_coverage,
  Power = sum_flagged_power,
  Type1Error = sum_flagged_error
)



# Count the total number of coefficients for each outcome type
total_coefficients <- ncol(flagged_bias_categorical)
total_power_coefficients <- length(true_effect_cols_categorical)
total_error_coefficients <- length(no_effect_cols_categorical)

# Calculate the percentages
flag_percent_categorical <- data.frame(
  Bias = (flag_sum_categorical$Bias / total_coefficients) * 100,
  Accuracy = (flag_sum_categorical$Accuracy / total_coefficients) * 100,
  Coverage = (flag_sum_categorical$Coverage / total_coefficients) * 100,
  Power = (flag_sum_categorical$Power / total_power_coefficients) * 100,
  Type1Error = (flag_sum_categorical$Type1Error / total_error_coefficients) * 100
)
row.names(flag_percent_categorical) <- row.names(flag_sum_categorical)


# Calculate the percentages
flag_percent_all_categorical <- data.frame(
  ModBias = (flag_sum_all_categorical$ModBias / total_coefficients)* 100,
  ExtremeBias= (flag_sum_all_categorical$ExtremeBias/total_coefficients) * 100 ,
  Bias = (flag_sum_all_categorical$Bias / total_coefficients) *100,
  Accuracy = (flag_sum_all_categorical$Accuracy / total_coefficients) * 100,
  LwrCoverage = (flag_sum_all_categorical$LwrCoverage/ total_coefficients) * 100,
  UprCoverage = (flag_sum_all_categorical$UprCoverage / total_coefficients) * 100,
  FullCoverage = (flag_sum_all_categorical$FullCoverage / total_coefficients) * 100,
  Power = (flag_sum_all_categorical$Power / total_power_coefficients) * 100,
  Type1Error = (flag_sum_all_categorical$Type1Error / total_error_coefficients) * 100
)
row.names(flag_percent_all_categorical) <- row.names(flag_sum_all_categorical)



# Interaction

sum_flagged_bias <- rowSums(flagged_bias_interaction, na.rm = TRUE)
sum_flagged_modbias <- rowSums(flagged_modbias_interaction, na.rm = TRUE)
sum_flagged_extremebias <- rowSums(flagged_extremebias_interaction, na.rm = TRUE)
sum_flagged_accuracy <- rowSums(flagged_accuracy_interaction, na.rm = TRUE)
sum_flagged_coverage <- rowSums(flagged_coverage_interaction, na.rm = TRUE)
sum_flagged_power <- rowSums(flagged_power_error_interaction[, true_effect_cols_interaction], na.rm = TRUE)
sum_flagged_error <- rowSums(flagged_power_error_interaction[, no_effect_cols_interaction], na.rm = TRUE)
sum_flagged_uprcoverage <- rowSums(flagged_uprcoverage_interaction, na.rm = TRUE)
sum_flagged_fullcoverage <- rowSums(flagged_fullcoverage_interaction, na.rm = TRUE)



flag_sum_all_interaction <- data.frame(
  ModBias= sum_flagged_modbias,
  ExtremeBias= sum_flagged_extremebias,
  Bias = sum_flagged_bias,
  Accuracy = sum_flagged_accuracy,
  LwrCoverage = sum_flagged_coverage,
  UprCoverage = sum_flagged_uprcoverage,
  FullCoverage = sum_flagged_fullcoverage,
  Power = sum_flagged_power,
  Type1Error = sum_flagged_error
)


# Create a new dataframe to hold these sums
flag_sum_interaction <- data.frame(
  Bias = sum_flagged_bias,
  Accuracy = sum_flagged_accuracy,
  Coverage = sum_flagged_coverage,
  Power = sum_flagged_power,
  Type1Error = sum_flagged_error
)

# Count the total number of coefficients for each outcome type
total_coefficients <- ncol(flagged_bias_interaction)
total_power_coefficients <- length(true_effect_cols_interaction)
total_error_coefficients <- length(no_effect_cols_interaction)

# Calculate the percentages
flag_percent_interaction <- data.frame(
  Bias = (flag_sum_interaction$Bias / total_coefficients) * 100,
  Accuracy = (flag_sum_interaction$Accuracy / total_coefficients) * 100,
  Coverage = (flag_sum_interaction$Coverage / total_coefficients) * 100,
  Power = (flag_sum_interaction$Power / total_power_coefficients) * 100,
  Type1Error = (flag_sum_interaction$Type1Error / total_error_coefficients) * 100
)
row.names(flag_percent_interaction) <- row.names(flag_sum_interaction)

# Calculate the percentages
flag_percent_all_interaction <- data.frame(
  ModBias = (flag_sum_all_interaction$ModBias/total_coefficients)*100,
  ExtremeBias= (flag_sum_all_interaction$ExtremeBias/total_coefficients)*100,
  Bias = (flag_sum_all_interaction$Bias / total_coefficients) * 100,
  Accuracy = (flag_sum_all_interaction$Accuracy / total_coefficients) * 100,
  LwrCoverage = (flag_sum_all_interaction$LwrCoverage/ total_coefficients) * 100,
  UprCoverage = (flag_sum_all_interaction$UprCoverage / total_coefficients) * 100,
  FullCoverage = (flag_sum_all_interaction$FullCoverage / total_coefficients) * 100,
  Power = (flag_sum_all_interaction$Power / total_power_coefficients) * 100,
  Type1Error = (flag_sum_all_interaction$Type1Error / total_error_coefficients) * 100
)
row.names(flag_percent_all_interaction) <- row.names(flag_sum_all_interaction)



#Maida

sum_flagged_bias <- rowSums(flagged_bias_maidha, na.rm = TRUE)
sum_flagged_modbias <- rowSums(flagged_modbias_maidha, na.rm = TRUE)
sum_flagged_extremebias <- rowSums(flagged_extremebias_maidha, na.rm = TRUE)
sum_flagged_accuracy <- rowSums(flagged_accuracy_maidha, na.rm = TRUE)
sum_flagged_coverage <- rowSums(flagged_coverage_maidha, na.rm = TRUE)
sum_flagged_power <- rowSums(flagged_power_error_maidha[, true_effect_cols_maidha], na.rm = TRUE)
sum_flagged_error <- rowSums(flagged_power_error_maidha[, no_effect_cols_maidha], na.rm = TRUE)
sum_flagged_uprcoverage <- rowSums(flagged_uprcoverage_maidha, na.rm = TRUE)
sum_flagged_fullcoverage <- rowSums(flagged_fullcoverage_maidha, na.rm = TRUE)


flag_sum_all_maidha<- data.frame(
  ModBias= sum_flagged_modbias,
  ExtremeBias= sum_flagged_extremebias,
  Bias = sum_flagged_bias,
  Accuracy = sum_flagged_accuracy,
  LwrCoverage = sum_flagged_coverage,
  UprCoverage = sum_flagged_uprcoverage,
  FullCoverage = sum_flagged_fullcoverage,
  Power = sum_flagged_power,
  Type1Error = sum_flagged_error
)




# Create a new dataframe to hold these sums
flag_sum_maidha<- data.frame(
  Bias = sum_flagged_bias,
  Accuracy = sum_flagged_accuracy,
  Coverage = sum_flagged_coverage,
  Power = sum_flagged_power,
  Type1Error = sum_flagged_error
)

# Count the total number of coefficients for each outcome type
total_coefficients <- ncol(flagged_bias_maidha)
total_power_coefficients <- length(true_effect_cols_maidha)
total_error_coefficients <- length(no_effect_cols_maidha)

# Calculate the percentages
flag_percent_maidha <- data.frame(
  Bias = (flag_sum_maidha$Bias / total_coefficients) * 100,
  Accuracy = (flag_sum_maidha$Accuracy / total_coefficients) * 100,
  Coverage = (flag_sum_maidha$Coverage / total_coefficients) * 100,
  Power = (flag_sum_maidha$Power / total_power_coefficients) * 100,
  Type1Error = (flag_sum_maidha$Type1Error / total_error_coefficients) * 100
)
row.names(flag_percent_maidha) <- row.names(flag_sum_maidha)

# Calculate the percentages
flag_percent_all_maidha <- data.frame(
  ModBias = (flag_sum_all_maidha$ModBias/total_coefficients) *100,
  ExtremeBias= (flag_sum_all_maidha$ExtremeBias/total_coefficients) *100, 
  Bias = (flag_sum_all_maidha$Bias / total_coefficients) * 100,
  Accuracy = (flag_sum_all_maidha$Accuracy / total_coefficients) * 100,
  LwrCoverage = (flag_sum_all_maidha$LwrCoverage/ total_coefficients) * 100,
  UprCoverage = (flag_sum_all_maidha$UprCoverage / total_coefficients) * 100,
  FullCoverage = (flag_sum_all_maidha$FullCoverage / total_coefficients) * 100,
  Power = (flag_sum_all_maidha$Power / total_power_coefficients) * 100,
  Type1Error = (flag_sum_all_maidha$Type1Error / total_error_coefficients) * 100
)
row.names(flag_percent_all_maidha) <- row.names(flag_sum_all_maidha)





### Average for each model

#Categorical
# Initialize the new data frame
categorical_outcomes_avg <- data.frame(matrix(ncol = 8, nrow = length(rownames(bias_categorical))))
colnames(categorical_outcomes_avg) <- c("Bias", "Accuracy", "P-Value True Effect", "P-Value No Effect", "CI True Effect LL", "CI True Effect UL", "CI No Effect LL", "CI No Effect UL")
rownames(categorical_outcomes_avg) <- rownames(bias_categorical)

# Average Bias and Accuracy across all coefficients for each condition
categorical_outcomes_avg$Bias <- rowMeans(bias_categorical, na.rm = TRUE)
categorical_outcomes_avg$Accuracy <- rowMeans(accuracy_categorical, na.rm = TRUE)

# Average P-values for true effect and no effect
categorical_outcomes_avg$`P-Value True Effect` <- rowMeans(avg_categorical_p_values[, true_effect_cols_categorical], na.rm = TRUE)
categorical_outcomes_avg$`P-Value No Effect` <- rowMeans(avg_categorical_p_values[, no_effect_cols_categorical], na.rm = TRUE)

# Calculate average CI True Effect and No Effect (LL and UL separately)
true_effect_ci_LL_cols <- paste0(true_effect_cols_categorical, "_LL")
true_effect_ci_UL_cols <- paste0(true_effect_cols_categorical, "_UL")
no_effect_ci_LL_cols <- paste0(no_effect_cols_categorical, "_LL")
no_effect_ci_UL_cols <- paste0(no_effect_cols_categorical, "_UL")

categorical_outcomes_avg$`CI True Effect LL` <- rowMeans(avg_categorical_ci_values[, true_effect_ci_LL_cols], na.rm = TRUE)
categorical_outcomes_avg$`CI True Effect UL` <- rowMeans(avg_categorical_ci_values[, true_effect_ci_UL_cols], na.rm = TRUE)
categorical_outcomes_avg$`CI No Effect LL` <- rowMeans(avg_categorical_ci_values[, no_effect_ci_LL_cols], na.rm = TRUE)
categorical_outcomes_avg$`CI No Effect UL` <- rowMeans(avg_categorical_ci_values[, no_effect_ci_UL_cols], na.rm = TRUE)







#Interaction

# Initialize the new data frame
interaction_outcomes_avg <- data.frame(matrix(ncol = 8, nrow = length(rownames(bias_interaction))))
colnames(interaction_outcomes_avg) <- c("Bias", "Accuracy", "P-Value True Effect", "P-Value No Effect", "CI True Effect LL", "CI True Effect UL", "CI No Effect LL", "CI No Effect UL")
rownames(interaction_outcomes_avg) <- rownames(bias_interaction)

# Average Bias and Accuracy across all coefficients for each condition
interaction_outcomes_avg$Bias <- rowMeans(bias_interaction, na.rm = TRUE)
interaction_outcomes_avg$Accuracy <- rowMeans(accuracy_interaction, na.rm = TRUE)

# Average P-values for true effect and no effect
interaction_outcomes_avg$`P-Value True Effect` <- rowMeans(avg_interaction_p_values[, true_effect_cols_interaction], na.rm = TRUE)
interaction_outcomes_avg$`P-Value No Effect` <- rowMeans(avg_interaction_p_values[, no_effect_cols_interaction], na.rm = TRUE)

# Calculate average CI True Effect and No Effect (LL and UL separately)
true_effect_ci_LL_cols <- paste0(true_effect_cols_interaction, "_LL")
true_effect_ci_UL_cols <- paste0(true_effect_cols_interaction, "_UL")
no_effect_ci_LL_cols <- paste0(no_effect_cols_interaction, "_LL")
no_effect_ci_UL_cols <- paste0(no_effect_cols_interaction, "_UL")

interaction_outcomes_avg$`CI True Effect LL` <- rowMeans(avg_interaction_ci_values[, true_effect_ci_LL_cols], na.rm = TRUE)
interaction_outcomes_avg$`CI True Effect UL` <- rowMeans(avg_interaction_ci_values[, true_effect_ci_UL_cols], na.rm = TRUE)
interaction_outcomes_avg$`CI No Effect LL` <- rowMeans(avg_interaction_ci_values[, no_effect_ci_LL_cols], na.rm = TRUE)
interaction_outcomes_avg$`CI No Effect UL` <- rowMeans(avg_interaction_ci_values[, no_effect_ci_UL_cols], na.rm = TRUE)


#Maidha

# Initialize the new data frame
maidha_outcomes_avg <- data.frame(matrix(ncol = 8, nrow = length(rownames(bias_maidha))))
colnames(maidha_outcomes_avg) <- c("Bias", "Accuracy", "P-Value True Effect", "P-Value No Effect", "CI True Effect LL", "CI True Effect UL", "CI No Effect LL", "CI No Effect UL")
rownames(maidha_outcomes_avg) <- rownames(bias_maidha)

# Average Bias and Accuracy across all coefficients for each condition
maidha_outcomes_avg$Bias <- rowMeans(bias_maidha, na.rm = TRUE)
maidha_outcomes_avg$Accuracy <- rowMeans(accuracy_maidha, na.rm = TRUE)

# Average P-values for true effect and no effect
maidha_outcomes_avg$`P-Value True Effect` <- rowMeans(interval_means_maidha[, true_effect_cols_maidha], na.rm = TRUE)
maidha_outcomes_avg$`P-Value No Effect` <- rowMeans(interval_means_maidha[, no_effect_cols_maidha], na.rm = TRUE)

# Calculate average CI True Effect and No Effect (LL and UL separately)
true_effect_ci_LL_cols <- paste0(true_effect_cols_maidha, "_LL")
true_effect_ci_UL_cols <- paste0(true_effect_cols_maidha, "_UL")
no_effect_ci_LL_cols <- paste0(no_effect_cols_maidha, "_LL")
no_effect_ci_UL_cols <- paste0(no_effect_cols_maidha, "_UL")

maidha_outcomes_avg$`CI True Effect LL` <- rowMeans(avg_maidha_ci_values[, true_effect_ci_LL_cols], na.rm = TRUE)
maidha_outcomes_avg$`CI True Effect UL` <- rowMeans(avg_maidha_ci_values[, true_effect_ci_UL_cols], na.rm = TRUE)
maidha_outcomes_avg$`CI No Effect LL` <- rowMeans(avg_maidha_ci_values[, no_effect_ci_LL_cols], na.rm = TRUE)
maidha_outcomes_avg$`CI No Effect UL` <- rowMeans(avg_maidha_ci_values[, no_effect_ci_UL_cols], na.rm = TRUE)






##Summary Tables

# Initialize an empty data frame for total flags



total_flags_table_2 <- data.frame(matrix(ncol = 3, nrow = 5))
colnames(total_flags_table_2) <- c("Categorical", "Interaction", "Maidha")
rownames(total_flags_table_2) <- c("Bias", "Accuracy", "Coverage", "Power", "Type1Error")

# Fill in the data frame with total flag counts
total_flags_table_2$Categorical <- colSums(flag_sum_categorical)
total_flags_table_2$Interaction <- colSums(flag_sum_interaction)
total_flags_table_2$Maidha <- colSums(flag_sum_maidha)







#Total sums (assuming none dropped to RD)

#Interaction-- Bias: 162; Accuracy: 162; Coverage: 162; Power: 54; Type 1 Error: 108
#Categorical-- Bias: 351; Accuracy: 351; Coverage: 351; Power: 135; Type 1 Error: 216
#Maidha-- Bias: 378; Accuracy: 378; Coverage: 378; Power: 135; Type 1 Error: 242





# Define the outcome names
outcomes <- c("Bias", "Accuracy", "Coverage", "Power", "Type1Error")

# Define the total possible flags for each model and each outcome

# Define the total possible flags for each model and each outcome
total_flags_interaction <- c(162, 162, 162, 54, 108)
total_flags_categorical <- c(351, 351, 351, 135, 216)
total_flags_maidha <- c(378, 378, 378, 135, 242)


# Initialize an empty data frame
summary_table_2 <- data.frame(matrix(ncol = 2, nrow = length(outcomes)))
colnames(summary_table_2) <- c("Categorical", "Interaction")
row.names(summary_table_2) <- outcomes

# Fill in the data frame with percentages
summary_table_2$Categorical <- (colSums(flag_sum_categorical) / total_flags_categorical) * 100
summary_table_2$Interaction <- (colSums(flag_sum_interaction) / total_flags_interaction) * 100
summary_table_2$Maidha <- (colSums(flag_sum_maidha) / total_flags_maidha) * 100



#Average Outcomes Across Method 

# Define the outcomes
outcomes <- c("Bias", "Accuracy", "P-Value True Effect", "P-Value No Effect", "CI True Effect LL", "CI True Effect UL", "CI No Effect LL", "CI No Effect UL")

# Initialize the final data frame to hold averages
average_outcomes_2 <- data.frame(matrix(ncol = 3, nrow = length(outcomes)))
colnames(average_outcomes_2) <- c("Interaction", "Categorical", "Maidha")
rownames(average_outcomes_2) <- outcomes

# Calculate the average for each outcome across conditions for each model
average_outcomes_2$Interaction <- colMeans(interaction_outcomes_avg, na.rm = TRUE)
average_outcomes_2$Categorical <- colMeans(categorical_outcomes_avg, na.rm = TRUE)
average_outcomes_2$Maidha <- colMeans(maidha_outcomes_avg, na.rm = TRUE)



###AIC and BIC Values


# Create an empty data frame with model names as column names and conditions as row names
final_aic_bic_table <- data.frame(matrix(ncol = 6, nrow = nrow(average_aic_bic_categorical)))
colnames(final_aic_bic_table) <- c("Interaction_AIC", "Interaction_BIC", "Categorical_AIC", "Categorical_BIC", "Maidha_AIC", "Maidha_BIC")
rownames(final_aic_bic_table) <- rownames(average_aic_bic_categorical)

# Fill in the AIC and BIC values for each model
final_aic_bic_table$Interaction_AIC <- average_aic_bic_interaction$AIC
final_aic_bic_table$Interaction_BIC <- average_aic_bic_interaction$BIC

final_aic_bic_table$Categorical_AIC <- average_aic_bic_categorical$AIC
final_aic_bic_table$Categorical_BIC <- average_aic_bic_categorical$BIC

final_aic_bic_table$Maidha_AIC <- average_aic_bic_maidha$AIC
final_aic_bic_table$Maidha_BIC <- average_aic_bic_maidha$BIC


###############################################################################
###########################Save Tables#########################################

######Overall Results#####


###Lists

save(interaction_results,categorical_results, maidha_intercepts_list, file =
       "Documents/Dissertation_Results/2cat_intersection_results.RData")

save(interaction_p_values_list_cleaned, categorical_p_values_list, hyp_test_maidha,
     file= "Documents/Dissertation_Results/2cat_p_values.RData")

save(interaction_aic_bic_values_list, categorical_aic_bic_values_list, maidha_aic_bic_values_list,
     file= "Documents/Dissertation_Results/2cat_aic_bic.RData")

save(intersectional_ci_values_list, categorical_ci_values_list, maidha_ci_values_list,
     file = "Documents/Dissertation_Results/2_cat_ci_values.RData")

save(coverage_results_interaction, coverage_results_categorical, coverage_results_maidha, 
     file = "Documents/Dissertation_Results/2cat_coverage_results.RData")

save(categorical_icc_values_list, interaction_icc_values_list, maidha_icc_values_list, 
     file = "Documents/Dissertation_Results/2cat_ICC_results.RData")



###Data Frames
average_ci_values <- list(
  "Interaction" = avg_interaction_ci_values,
  "Categorical" = avg_categorical_ci_values,
  "Maidha" = avg_maidha_ci_values
)



average_p_values <- list(
  "Interaction" = avg_interaction_p_values,
  "Categorical" = avg_categorical_p_values,
  "Maidha" = interval_means_maidha
)




average_intersectional_values <- list(
  "Interaction" = interaction_int_effects,
  "Categorical" = categorical_avg_coefficients,
  "Maidha" = maidha_avg_intercepts
)



average_aic_bic_values <- list(
  "Interaction" = average_aic_bic_interaction,
  "Categorical" = average_aic_bic_categorical,
  "Maidha" = average_aic_bic_maidha
)



average_seb_values <- list(
  "Interaction" = seb_interaction_df,
  "Categorical" = seb_categorical_df,
  "Maidha" = seb_maidha_df
)



average_bias_values <- list(
  "Interaction" = bias_interaction,
  "Categorical" = bias_categorical,
  "Maidha" = bias_maidha
)



average_accuracy_values <- list(
  "Interaction" = accuracy_interaction,
  "Categorical" = accuracy_categorical,
  "Maidha" = accuracy_maidha
)


coverage_percent <- list(
  "Interaction" = overall_coverage_percentages_interaction,
  "Categorical" = overall_coverage_percentages_categorical,
  "Maidha" = overall_coverage_percentages_maidha
)



p_value_percent <- list(
  "Interaction" = p_value_percent_interaction,
  "Categorical" = p_value_percent_categorical,
  "Maidha" = sig_percent_maidha
)




outcomeavg_model <- list(
  "Interaction" = interaction_outcomes_avg,
  "Categorical" = categorical_outcomes_avg,
  "Maidha" = maidha_outcomes_avg
)




####Flagging########

flagged_bias_tables <- list(
  "Categorical" = flagged_bias_categorical,
  "Interaction" = flagged_bias_interaction,
  "Maidha" = flagged_bias_maidha
)



flagged_accuracy_tables <- list(
  "Interaction" = flagged_accuracy_interaction,
  "Categorical" = flagged_accuracy_categorical,
  "Maidha" = flagged_accuracy_maidha
)


flagged_power_error_tables <- list(
  "Interaction" = flagged_power_error_interaction,
  "Categorical" = flagged_power_error_categorical,
  "Maidha" = flagged_power_error_maidha
)


flagged_coverage_tables <- list(
  "Interaction" = flagged_coverage_interaction,
  "Categorical" = flagged_coverage_categorical,
  "Maidha" = flagged_coverage_maidha
)



flagged_percent_tables <- list(
  "Interaction" = flag_percent_interaction,
  "Categorical" = flag_percent_categorical,
  "Maidha" = flag_percent_maidha
)


flag_sum_tables <- list(
  "Interaction" = flag_sum_interaction,
  "Categorical" = flag_sum_categorical,
  "Maidha" = flag_sum_maidha
)



flagged_percent_all_tables <- list(
  "Interaction" = flag_percent_all_interaction,
  "Categorical" = flag_percent_all_categorical,
  "Maidha" = flag_percent_all_maidha
)


flag_sum_all_tables <- list(
  "Interaction" = flag_sum_all_interaction,
  "Categorical" = flag_sum_all_categorical,
  "Maidha" = flag_sum_all_maidha
)



# Save each of your data frame lists into Excel, and include the row names
write.xlsx(average_ci_values, "Documents/Dissertation_Results/2cat_average_ci_values.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(average_p_values, "Documents/Dissertation_Results/2cat_average_p_values.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(average_intersectional_values, "Documents/Dissertation_Results/2cat_average_intersectioanl_values.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(average_aic_bic_values, "Documents/Dissertation_Results/2cat_average_aic_bic_values.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(average_seb_values, "Documents/Dissertation_Results/2cat_average_seb_values.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(average_bias_values, "Documents/Dissertation_Results/2cat_average_bias_values.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(average_accuracy_values, "Documents/Dissertation_Results/2cat_average_accuracy_values.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(coverage_percent, "Documents/Dissertation_Results/2cat_coverage_percent.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(p_value_percent, "Documents/Dissertation_Results/2cat_p_value_percent.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(outcomeavg_model, "Documents/Dissertation_Results/2cat_outcomeavg_model.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(flagged_bias_tables, "Documents/Dissertation_Results/2cat_flagged_bias.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(flagged_accuracy_tables, "Documents/Dissertation_Results/2cat_flagged_accuracy.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(flagged_power_error_tables, "Documents/Dissertation_Results/2cat_flagged_power_error.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(flagged_coverage_tables, "Documents/Dissertation_Results/2cat_flagged_coverage.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(flagged_percent_tables, "Documents/Dissertation_Results/2cat_flagged_percent.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(flag_sum_tables, "Documents/Dissertation_Results/2cat_flag_sums.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(flagged_percent_all_tables, "Documents/Dissertation_Results/2cat_flagged_percent_all.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(flag_sum_all_tables, "Documents/Dissertation_Results/2cat_flag_sums_all.xlsx", asTable = TRUE, rowNames = TRUE)


####Summarizing#####

write.xlsx(total_flags_table_2, "mmfs/data/szendey/Documents/Dissertation_Results/2cat_total_flags.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(summary_table_2, "mmfs/data/szendey/Documents/Dissertation_Results/2cat_percent_flags.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(average_outcomes_2, "mmfs/data/szendey/Documents/Dissertation_Results/2cat_average_outcomes.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(final_aic_bic_table, "mmfs/data/szendey/Documents/Dissertation_Results/2cat_AIC_BIC.xlsx", asTable = TRUE, rowNames = TRUE)



###Distributions of Outcomes

#Bias

#Interaction

# Extract the "interaction" dataframe from the list "average_bias_outcomes"
interaction_df <- average_bias_values$Interaction

# Flatten the dataframe to get a single vector of all data points
all_data_points <- as.vector(as.matrix(interaction_df))

# Create a new dataframe to store all data points
flattened_df <- data.frame(Value = all_data_points)


# Extract the "Interaction" dataframe from the list "average_seb_values"
interaction_seb_df <- average_seb_values$Interaction

# Calculate the mean of all values in the dataframe
mean_value <- mean(as.vector(as.matrix(interaction_seb_df)))

# Calculate 2x the mean value
double_mean_value <- 2 * mean_value
negative_double_mean_value <- -double_mean_value
moderate_bias<-.5*mean_value
negative_moderate_bias<- -moderate_bias



# Plot the histogram
interaction_extremebias<-ggplot(flattened_df, aes(x=Value)) +
  geom_histogram(binwidth=0.05, fill="blue", alpha=0.7, color="black") +
  ggtitle("Interaction Extreme Bias") +
  geom_vline(aes(xintercept=double_mean_value), color="red", linetype="dashed", linewidth=1) +
  geom_vline(aes(xintercept=negative_double_mean_value), color="red", linetype="dashed", linewidth=1) +
  xlab("Value") +
  ylab("Frequency") 


# Plot the histogram
interaction_modbias<-ggplot(flattened_df, aes(x=Value)) +
  geom_histogram(binwidth=0.05, fill="blue", alpha=0.7, color="black") +
  ggtitle("Interaction Moderate Bias") +
  geom_vline(aes(xintercept=moderate_bias), color="red", linetype="dashed", linewidth=1) +
  geom_vline(aes(xintercept=negative_moderate_bias), color="red", linetype="dashed", linewidth=1) +
  xlab("Value") +
  ylab("Frequency") 



#Categorical

# Extract the "interaction" dataframe from the list "average_bias_outcomes"
categorical_df <- average_bias_values$Categorical

# Flatten the dataframe to get a single vector of all data points
all_data_points <- as.vector(as.matrix(categorical_df))

# Create a new dataframe to store all data points
flattened_df <- data.frame(Value = all_data_points)

# Extract the "Interaction" dataframe from the list "average_seb_values"
categorical_seb_df <- average_seb_values$Categorical

# Calculate the mean of all values in the dataframe
mean_value <- mean(as.vector(as.matrix(categorical_seb_df)))

# Calculate 2x the mean value
double_mean_value <- 2 * mean_value
negative_double_mean_value <- -double_mean_value
moderate_bias<-.5*mean_value
negative_moderate_bias<- -moderate_bias

# Plot the histogram
categorical_extremebias<-ggplot(flattened_df, aes(x=Value)) +
  geom_histogram(binwidth=0.05, fill="blue", alpha=0.7, color="black") +
  ggtitle("Categorical Extreme Bias") +
  geom_vline(aes(xintercept=double_mean_value), color="red", linetype="dashed", linewidth=1) +
  geom_vline(aes(xintercept=negative_double_mean_value), color="red", linetype="dashed", linewidth=1) +
  xlab("Value") +
  ylab("Frequency")

# Plot the histogram
categorical_modbias<-ggplot(flattened_df, aes(x=Value)) +
  geom_histogram(binwidth=0.05, fill="blue", alpha=0.7, color="black") +
  ggtitle("Categorical Moderate Bias") +
  geom_vline(aes(xintercept=moderate_bias), color="red", linetype="dashed", linewidth=1) +
  geom_vline(aes(xintercept=negative_moderate_bias), color="red", linetype="dashed", linewidth=1) +
  xlab("Value") +
  ylab("Frequency")



#Maidha
# Extract the "interaction" dataframe from the list "average_bias_outcomes"
maidha_df <- average_bias_values$Maidha

# Flatten the dataframe to get a single vector of all data points
all_data_points <- as.vector(as.matrix(maidha_df))

# Create a new dataframe to store all data points
flattened_df <- data.frame(Value = all_data_points)


# Extract the "Interaction" dataframe from the list "average_seb_values"
maidha_seb_df <- average_seb_values$Maidha
# Calculate the mean of all values in the dataframe
mean_value <- mean(as.vector(as.matrix(maidha_seb_df)))



# Calculate 2x the mean value
double_mean_value <- 2 * mean_value
negative_double_mean_value <- -double_mean_value


moderate_bias<-.5*mean_value
negative_moderate_bias<- -moderate_bias


# Plot the histogram
maidha_extremebias<-ggplot(flattened_df, aes(x=Value)) +
  geom_histogram(binwidth=0.05, fill="blue", alpha=0.7, color="black") +
  ggtitle("MAIDHA Extreme Bias") +
  geom_vline(aes(xintercept=double_mean_value), color="red", linetype="dashed", linewidth=1) +
  geom_vline(aes(xintercept=negative_double_mean_value), color="red", linetype="dashed", linewidth=1) +
  xlab("Value") +
  ylab("Frequency")

maidha_modbias<-ggplot(flattened_df, aes(x=Value)) +
  geom_histogram(binwidth=0.05, fill="blue", alpha=0.7, color="black") +
  ggtitle("MAIDHA Moderate Bias") +
  geom_vline(aes(xintercept=moderate_bias), color="red", linetype="dashed", linewidth=1) +
  geom_vline(aes(xintercept=negative_moderate_bias), color="red", linetype="dashed", linewidth=1) +
  xlab("Value") +
  ylab("Frequency")



#Accuracy

#Interaction

# Extract the "interaction" dataframe from the list "average_bias_outcomes"
interaction_df <- average_accuracy_values$Interaction

# Flatten the dataframe to get a single vector of all data points
all_data_points <- as.vector(as.matrix(interaction_df))

# Create a new dataframe to store all data points
flattened_df <- data.frame(Value = all_data_points)


# Plot the histogram
interaction_accuracy<-ggplot(flattened_df, aes(x=Value)) +
  geom_histogram(binwidth=0.05, fill="blue", alpha=0.7, color="black") +
  ggtitle("Interaction Accuracy") +
  geom_vline(aes(xintercept=.5), color="red", linetype="dashed", linewidth=1) +
  xlab("Value") +
  ylab("Frequency") 




#Categorical

# Extract the "interaction" dataframe from the list "average_bias_outcomes"
categorical_df <- average_accuracy_values$Categorical

# Flatten the dataframe to get a single vector of all data points
all_data_points <- as.vector(as.matrix(categorical_df))

# Create a new dataframe to store all data points
flattened_df <- data.frame(Value = all_data_points)



# Plot the histogram
categorical_accuracy<-ggplot(flattened_df, aes(x=Value)) +
  geom_histogram(binwidth=0.05, fill="blue", alpha=0.7, color="black") +
  ggtitle("Categorical Accuracy") +
  geom_vline(aes(xintercept=.5), color="red", linetype="dashed", linewidth=1) +
  xlab("Value") +
  ylab("Frequency")


#Maidha
# Extract the "interaction" dataframe from the list "average_bias_outcomes"
maidha_df <- average_accuracy_values$Maidha

# Flatten the dataframe to get a single vector of all data points
all_data_points <- as.vector(as.matrix(maidha_df))

# Create a new dataframe to store all data points
flattened_df <- data.frame(Value = all_data_points)


# Plot the histogram
maidha_accuracy<-ggplot(flattened_df, aes(x=Value)) +
  geom_histogram(binwidth=0.05, fill="blue", alpha=0.7, color="black") +
  ggtitle("MAIDHA Accuracy") +
  geom_vline(aes(xintercept=.5), color="red", linetype="dashed", linewidth=1) +
  xlab("Value") +
  ylab("Frequency")




###True Effect P-Values

#Interaction

true_effect_data <- avg_interaction_p_values[, true_effect_cols_interaction]
true_effect_data_melted <- as.data.frame(as.table(as.matrix(true_effect_data)))

# Create a long format data frame for no effect columns
no_effect_data <- avg_interaction_p_values[, no_effect_cols_interaction]
no_effect_data_melted <- as.data.frame(as.table(as.matrix(no_effect_data)))


# Create histograms for True Effect Coefficients
interaction_true <- ggplot(true_effect_data_melted, aes(x=Freq)) +
  geom_histogram(binwidth=0.05, fill="blue", alpha=0.7, color="black") +
  geom_vline(aes(xintercept=0.05), color="red", linetype="dashed", linewidth=1) +
  ggtitle("True Effect P-Values Interaction") +
  xlab("Value") +
  ylab("Frequency")


# Create histograms for No Effect Coefficients
interaction_none <- ggplot(no_effect_data_melted, aes(x=Freq)) +
  geom_histogram(binwidth=0.05, fill="blue", alpha=0.7, color="black") +
  geom_vline(aes(xintercept=0.05), color="red", linetype="dashed", linewidth=1) +
  ggtitle("No Effect P-Values Interaction") +
  xlab("Value") +
  ylab("Frequency")




#Categorical


# Create a long format data frame for true effect columns
true_effect_data <- avg_categorical_p_values[, true_effect_cols_categorical]
true_effect_data_melted <- as.data.frame(as.table(as.matrix(true_effect_data)))

# Create a long format data frame for no effect columns
no_effect_data <- avg_categorical_p_values[, no_effect_cols_categorical]
no_effect_data_melted <- as.data.frame(as.table(as.matrix(no_effect_data)))


# Create histograms for True Effect Coefficients
categorical_true <- ggplot(true_effect_data_melted, aes(x=Freq)) +
  geom_histogram(binwidth=0.05, fill="blue", alpha=0.7, color="black") +
  geom_vline(aes(xintercept=0.05), color="red", linetype="dashed", linewidth=1) +
  ggtitle("True Effect P-Values Categorical") +
  xlab("Value") +
  ylab("Frequency")


# Create histograms for No Effect Coefficients
categorical_none <- ggplot(no_effect_data_melted, aes(x=Freq)) +
  geom_histogram(binwidth=0.05, fill="blue", alpha=0.7, color="black") +
  geom_vline(aes(xintercept=0.05), color="red", linetype="dashed", linewidth=1) +
  ggtitle("No Effect P-Values Categorical") +
  xlab("Value") +
  ylab("Frequency")



##Percent of P-values 

#Interaction

# Extract the specific 'categorical_data' data frame from the list
# Replace 'categorical_data' with the correct name if different
interaction_data <- p_value_percent$Interaction

# Data for true effect columns
true_effect_data <- interaction_data[, true_effect_cols_interaction, drop = FALSE]
true_effect_data_melted <- as.data.frame(as.table(as.matrix(true_effect_data)))

# Data for no effect columns
no_effect_data <- interaction_data[, no_effect_cols_interaction, drop = FALSE]
no_effect_data_melted <- as.data.frame(as.table(as.matrix(no_effect_data)))

# Plot for true effect columns
true_pct_pval_interaction <- ggplot(true_effect_data_melted, aes(x=Freq)) +
  geom_histogram(binwidth=5, fill="blue", alpha=0.7, color="black") +
  geom_vline(aes(xintercept=95), color="red", linetype="dashed", linewidth=1) +
  ggtitle("True Effect - Interaction") +
  xlab("Value") +
  ylab("Frequency")

# Plot for no effect columns
none_pct_pval_interaction <- ggplot(no_effect_data_melted, aes(x=Freq)) +
  geom_histogram(binwidth=5, fill="blue", alpha=0.7, color="black") +
  geom_vline(aes(xintercept=5), color="red", linetype="dashed", linewidth=1) +
  ggtitle("No Effect - Interaction") +
  xlab("Value") +
  ylab("Frequency")





#Categorical


categorical_data <- p_value_percent$Categorical

# Data for true effect columns
true_effect_data <- categorical_data[, true_effect_cols_categorical, drop = FALSE]
true_effect_data_melted <- as.data.frame(as.table(as.matrix(true_effect_data)))

# Data for no effect columns
no_effect_data <- categorical_data[, no_effect_cols_categorical, drop = FALSE]
no_effect_data_melted <- as.data.frame(as.table(as.matrix(no_effect_data)))

# Plot for true effect columns
true_pct_pval_categorical <- ggplot(true_effect_data_melted, aes(x=Freq)) +
  geom_histogram(binwidth=5, fill="blue", alpha=0.7, color="black") +
  geom_vline(aes(xintercept=95), color="red", linetype="dashed", linewidth=1) +
  ggtitle("True Effect - Categorical ") +
  xlab("Value") +
  ylab("Frequency")


# Plot for no effect columns
none_pct_pval_categorical <- ggplot(no_effect_data_melted, aes(x=Freq)) +
  geom_histogram(binwidth=5, fill="blue", alpha=0.7, color="black") +
  geom_vline(aes(xintercept=5), color="red", linetype="dashed", linewidth=1) +
  ggtitle("No Effect - Categorical ") +
  xlab("Value") +
  ylab("Frequency")


#Maidha

# Extract the specific 'categorical_data' data frame from the list
# Replace 'categorical_data' with the correct name if different
maidha_data <- p_value_percent$Maidha

# Data for true effect columns
true_effect_data <- maidha_data[, true_effect_cols_maidha, drop = FALSE]
true_effect_data_melted <- as.data.frame(as.table(as.matrix(true_effect_data)))

# Data for no effect columns
no_effect_data <- maidha_data[, no_effect_cols_maidha, drop = FALSE]
no_effect_data_melted <- as.data.frame(as.table(as.matrix(no_effect_data)))

# Plot for true effect columns
true_pct_pval_maidha <- ggplot(true_effect_data_melted, aes(x=Freq)) +
  geom_histogram(binwidth=5, fill="blue", alpha=0.7, color="black") +
  geom_vline(aes(xintercept=95), color="red", linetype="dashed", linewidth=1) +
  ggtitle("True Effect - MAIDHA") +
  xlab("Value") +
  ylab("Frequency")


# Plot for no effect columns
none_pct_pval_maidha <- ggplot(no_effect_data_melted, aes(x=Freq)) +
  geom_histogram(binwidth=5, fill="blue", alpha=0.7, color="black") +
  geom_vline(aes(xintercept=5), color="red", linetype="dashed", linewidth=1) +
  ggtitle("No Effect - MAIDHA") +
  xlab("Value") +
  ylab("Frequency")



#Coverage

#Interaction
melted_data <- as.data.frame(as.table(as.matrix(overall_coverage_percentages_interaction)))

# Create the histogram
coverage_interaction <- ggplot(melted_data, aes(x=Freq)) +
  geom_histogram(binwidth=1, fill="blue", alpha=0.7, color="black") +
  geom_vline(aes(xintercept=92.5), color="red", linetype="dashed", linewidth=1) +
  ggtitle("Overall Coverage Percentages Interaction") +
  xlab("Value") +
  ylab("Frequency")



#Interaction
melted_data <- as.data.frame(as.table(as.matrix(overall_coverage_percentages_categorical)))

# Create the histogram
coverage_categorical <- ggplot(melted_data, aes(x=Freq)) +
  geom_histogram(binwidth=1, fill="blue", alpha=0.7, color="black") +
  geom_vline(aes(xintercept=92.5), color="red", linetype="dashed", linewidth=1) +
  ggtitle("Overall Coverage Percentages Categorical") +
  xlab("Value") +
  ylab("Frequency")


#Interaction
melted_data <- as.data.frame(as.table(as.matrix(overall_coverage_percentages_maidha)))

# Create the histogram
coverage_maidha <- ggplot(melted_data, aes(x=Freq)) +
  geom_histogram(binwidth=1, fill="blue", alpha=0.7, color="black") +
  geom_vline(aes(xintercept=92.5), color="red", linetype="dashed", linewidth=1) +
  geom_vline(aes(xintercept=97.5), color="red", linetype="dashed", linewidth=1) +
  ggtitle("Overall Coverage Percentages MAIDHA") +
  xlab("Value") +
  ylab("Frequency")




# Function to save grid to file
save_grid <- function(grid_obj, filename) {
  pdf(filename)
  grid.draw(grid_obj)
  dev.off()
}

# Create and save the grid arrangements
viz_extreme_bias <- arrangeGrob(interaction_extremebias, categorical_extremebias, maidha_extremebias)
save_grid(viz_extreme_bias, "Dissertation_Results/Plots/2cat_viz_extreme_bias.pdf")

viz_mod_bias <- arrangeGrob(interaction_modbias, categorical_modbias, maidha_modbias)
save_grid(viz_mod_bias, "Dissertation_Results/Plots/2cat_viz_mod_bias.pdf")

viz_accuracy <- arrangeGrob(interaction_accuracy, categorical_accuracy, maidha_accuracy)
save_grid(viz_accuracy, "Dissertation_Results/Plots/2cat_viz_accuracy.pdf")

viz_p_values <- arrangeGrob(interaction_true, categorical_true, interaction_none, categorical_none)
save_grid(viz_p_values, "Dissertation_Results/Plots/2cat_viz_p_values.pdf")

viz_pct_pvals_true <- arrangeGrob(true_pct_pval_interaction, true_pct_pval_categorical, true_pct_pval_maidha)
save_grid(viz_pct_pvals_true, "Dissertation_Results/Plots/2cat_viz_pct_pvals_true.pdf")

viz_pct_pvals_none <- arrangeGrob(none_pct_pval_interaction, none_pct_pval_categorical, none_pct_pval_maidha)
save_grid(viz_pct_pvals_none, "Dissertation_Results/Plots/2cat_viz_pct_pvals_none.pdf")

viz_coverage <- arrangeGrob(coverage_interaction, coverage_categorical, coverage_maidha)
save_grid(viz_coverage, "Dissertation_Results/Plots/2cat_viz_coverage.pdf")


save.image("Documents/Dissertation_Results/2cat_envi.RData")


################################################################################################
###############THREE CATEGORIES#################################################
###############################################################################

####Three Categories######

S1<-8540 
S2<-5048
S3<-591 
S4<-4163 
S5<-7131
S6<-4184
S7<-2770
S8<-5153
S9<-5178
S10<-603
S11<-2422
S12<-4817

#Randomly sample from distributions for truth coefficients 

bA0B0C0<-0
set.seed(S1)
bA1B0C0<-Sample.positive.coeff(1)
set.seed(S2)
bA2B0C0<-Sample.positive.coeff(1)
set.seed(S3)
bA3B0C0<-Sample.negative.coeff(1) 
bA4B0C0<-0
bA5B0C0<-0
bA6B0C0<-0
bA0B1C0<-0
bA1B1C0<-0
set.seed(S4)
bA2B1C0<-Sample.positive.coeff(1)
bA3B1C0<-0
bA4B1C0<-0
bA5B1C0<-0
bA6B1C0<-0
bA0B0C1<-0
bA1B0C1<-0
set.seed(S5)
bA2B0C1<-Sample.positive.coeff(1)
set.seed(S6)
bA3B0C1<-Sample.negative.coeff(1)
bA4B0C1<-0
bA5B0C1<-0
bA6B0C1<-0
bA0B1C1<-0
bA1B1C1<-0
set.seed(S7)
bA2B1C1<-Sample.positive.coeff(1)
bA3B1C1<-0
bA4B1C1<-0
bA5B1C1<-0
set.seed(S8)
bA6B1C1<-Sample.negative.coeff(1)
bA0B0C2<-0
bA1B0C2<-0
bA2B0C2<-0
bA3B0C2<-0
bA4B0C2<-0
set.seed(S9)
bA5B0C2<-Sample.negative.coeff(1)
bA6B0C2<-0
bA0B1C2<-0
set.seed(S10)
bA1B1C2<-Sample.negative.coeff(1)
bA2B1C2<-0
set.seed(S11)
bA3B1C2<-Sample.negative.coeff(1)
bA4B1C2<-0
bA5B1C2<-0
set.seed(S12)
bA6B1C2<-Sample.negative.coeff(1)

#Build Dataset

A<- c(0, 1, 2, 3, 4, 5, 6)
B<- c(0, 1)
C<- c(0,1,2)
L1<-expand.grid(A=A, B=B, C=C)
L1$cluster <-factor(100*(1+L1$A)+ 10*(1+L1$B)+ 1*(1+L1$C))


#Build Dataset

A<- c(0, 1, 2, 3, 4, 5, 6)
B<- c(0, 1)
C<- c(0,1,2)
L1<-expand.grid(A=A, B=B, C=C)
L1$cluster <-factor(100*(1+L1$A)+ 10*(1+L1$B)+ 1*(1+L1$C))


L1$A1 <- ifelse(L1$A==1, 1, 0)
L1$A2<- ifelse(L1$A==2, 1, 0)
L1$A3 <- ifelse(L1$A==3, 1, 0)
L1$A4 <- ifelse(L1$A==4, 1, 0)
L1$A5 <- ifelse(L1$A==5, 1, 0)
L1$A6 <- ifelse(L1$A==6, 1, 0)
L1$A0 <- ifelse(L1$A==0,1,0)
L1$B0<-ifelse(L1$B==0, 1, 0)
L1$B1<-ifelse(L1$B==1, 1, 0)
L1$C0<-ifelse(L1$C==0, 1, 0)
L1$C1<-ifelse(L1$C==1, 1, 0)
L1$C2<-ifelse(L1$C==2, 1, 0)


L1$A0B0C0 <- ifelse(L1$A == 0 & L1$B == 0 & L1$C == 0, 1, 0)
L1$A1B0C0 <- ifelse(L1$A == 1 & L1$B == 0 & L1$C == 0, 1, 0)
L1$A2B0C0 <- ifelse(L1$A == 2 & L1$B == 0 & L1$C == 0, 1, 0)
L1$A3B0C0 <- ifelse(L1$A == 3 & L1$B == 0 & L1$C == 0, 1, 0)
L1$A4B0C0 <- ifelse(L1$A == 4 & L1$B == 0 & L1$C == 0, 1, 0)
L1$A5B0C0 <- ifelse(L1$A == 5 & L1$B == 0 & L1$C == 0, 1, 0)
L1$A6B0C0 <- ifelse(L1$A == 6 & L1$B == 0 & L1$C == 0, 1, 0)
L1$A0B1C0 <- ifelse(L1$A == 0 & L1$B == 1 & L1$C == 0, 1, 0)
L1$A1B1C0 <- ifelse(L1$A == 1 & L1$B == 1 & L1$C == 0, 1, 0)
L1$A2B1C0 <- ifelse(L1$A == 2 & L1$B == 1 & L1$C == 0, 1, 0)
L1$A3B1C0 <- ifelse(L1$A == 3 & L1$B == 1 & L1$C == 0, 1, 0)
L1$A4B1C0 <- ifelse(L1$A == 4 & L1$B == 1 & L1$C == 0, 1, 0)
L1$A5B1C0 <- ifelse(L1$A == 5 & L1$B == 1 & L1$C == 0, 1, 0)
L1$A6B1C0 <- ifelse(L1$A == 6 & L1$B == 1 & L1$C == 0, 1, 0)
L1$A0B0C0 <- ifelse(L1$A == 0 & L1$B == 0 & L1$C == 0, 1, 0)
L1$A1B0C0 <- ifelse(L1$A == 1 & L1$B == 0 & L1$C == 0, 1, 0)
L1$A2B0C0 <- ifelse(L1$A == 2 & L1$B == 0 & L1$C == 0, 1, 0)
L1$A3B0C0 <- ifelse(L1$A == 3 & L1$B == 0 & L1$C == 0, 1, 0)
L1$A4B0C0 <- ifelse(L1$A == 4 & L1$B == 0 & L1$C == 0, 1, 0)
L1$A5B0C0 <- ifelse(L1$A == 5 & L1$B == 0 & L1$C == 0, 1, 0)
L1$A6B0C0 <- ifelse(L1$A == 6 & L1$B == 0 & L1$C == 0, 1, 0)


L1$A0B0C1 <- ifelse(L1$A == 0 & L1$B == 0 & L1$C == 1, 1, 0)
L1$A1B0C1 <- ifelse(L1$A == 1 & L1$B == 0 & L1$C == 1, 1, 0)
L1$A2B0C1 <- ifelse(L1$A == 2 & L1$B == 0 & L1$C == 1, 1, 0)
L1$A3B0C1 <- ifelse(L1$A == 3 & L1$B == 0 & L1$C == 1, 1, 0)
L1$A4B0C1 <- ifelse(L1$A == 4 & L1$B == 0 & L1$C == 1, 1, 0)
L1$A5B0C1 <- ifelse(L1$A == 5 & L1$B == 0 & L1$C == 1, 1, 0)
L1$A6B0C1 <- ifelse(L1$A == 6 & L1$B == 0 & L1$C == 1, 1, 0)
L1$A0B1C1 <- ifelse(L1$A == 0 & L1$B == 1 & L1$C == 1, 1, 0)
L1$A1B1C1 <- ifelse(L1$A == 1 & L1$B == 1 & L1$C == 1, 1, 0)
L1$A2B1C1 <- ifelse(L1$A == 2 & L1$B == 1 & L1$C == 1, 1, 0)
L1$A3B1C1 <- ifelse(L1$A == 3 & L1$B == 1 & L1$C == 1, 1, 0)
L1$A4B1C1 <- ifelse(L1$A == 4 & L1$B == 1 & L1$C == 1, 1, 0)
L1$A5B1C1 <- ifelse(L1$A == 5 & L1$B == 1 & L1$C == 1, 1, 0)
L1$A6B1C1 <- ifelse(L1$A == 6 & L1$B == 1 & L1$C == 1, 1, 0)
L1$A0B0C1 <- ifelse(L1$A == 0 & L1$B == 0 & L1$C == 1, 1, 0)
L1$A1B0C1 <- ifelse(L1$A == 1 & L1$B == 0 & L1$C == 1, 1, 0)
L1$A2B0C1 <- ifelse(L1$A == 2 & L1$B == 0 & L1$C == 1, 1, 0)
L1$A3B0C1 <- ifelse(L1$A == 3 & L1$B == 0 & L1$C == 1, 1, 0)
L1$A4B0C1 <- ifelse(L1$A == 4 & L1$B == 0 & L1$C == 1, 1, 0)
L1$A5B0C1 <- ifelse(L1$A == 5 & L1$B == 0 & L1$C == 1, 1, 0)
L1$A6B0C1 <- ifelse(L1$A == 6 & L1$B == 0 & L1$C == 1, 1, 0)



L1$A0B0C2 <- ifelse(L1$A == 0 & L1$B == 0 & L1$C == 2, 1, 0)
L1$A1B0C2 <- ifelse(L1$A == 1 & L1$B == 0 & L1$C == 2, 1, 0)
L1$A2B0C2 <- ifelse(L1$A == 2 & L1$B == 0 & L1$C == 2, 1, 0)
L1$A3B0C2 <- ifelse(L1$A == 3 & L1$B == 0 & L1$C == 2, 1, 0)
L1$A4B0C2 <- ifelse(L1$A == 4 & L1$B == 0 & L1$C == 2, 1, 0)
L1$A5B0C2 <- ifelse(L1$A == 5 & L1$B == 0 & L1$C == 2, 1, 0)
L1$A6B0C2 <- ifelse(L1$A == 6 & L1$B == 0 & L1$C == 2, 1, 0)
L1$A0B1C2 <- ifelse(L1$A == 0 & L1$B == 1 & L1$C == 2, 1, 0)
L1$A1B1C2 <- ifelse(L1$A == 1 & L1$B == 1 & L1$C == 2, 1, 0)
L1$A2B1C2 <- ifelse(L1$A == 2 & L1$B == 1 & L1$C == 2, 1, 0)
L1$A3B1C2 <- ifelse(L1$A == 3 & L1$B == 1 & L1$C == 2, 1, 0)
L1$A4B1C2 <- ifelse(L1$A == 4 & L1$B == 1 & L1$C == 2, 1, 0)
L1$A5B1C2 <- ifelse(L1$A == 5 & L1$B == 1 & L1$C == 2, 1, 0)
L1$A6B1C2 <- ifelse(L1$A == 6 & L1$B == 1 & L1$C == 2, 1, 0)
L1$A0B0C2 <- ifelse(L1$A == 0 & L1$B == 0 & L1$C == 2, 1, 0)
L1$A1B0C2 <- ifelse(L1$A == 1 & L1$B == 0 & L1$C == 2, 1, 0)
L1$A2B0C2 <- ifelse(L1$A == 2 & L1$B == 0 & L1$C == 2, 1, 0)
L1$A3B0C2 <- ifelse(L1$A == 3 & L1$B == 0 & L1$C == 2, 1, 0)
L1$A4B0C2 <- ifelse(L1$A == 4 & L1$B == 0 & L1$C == 2, 1, 0)
L1$A5B0C2 <- ifelse(L1$A == 5 & L1$B == 0 & L1$C == 2, 1, 0)
L1$A6B0C2 <- ifelse(L1$A == 6 & L1$B == 0 & L1$C == 2, 1, 0)





#Build Level two out
# Define the number of schools
n.schools <- 100

# Initialize an empty dataframe to store the results
Truth <- data.frame()

# Loop through each school
for (i in 1:n.schools) {
  
  # Replicate the structure of Truth for the current school
  df <- L1
  df$school_ID <- i
  
  # Add the replicated data to the results dataframe
  Truth <- rbind(Truth, df)
}





# Initialize an empty dataframe to store the results
Truth <- data.frame()

# Loop through each school
for (i in 1:n.schools) {
  
  # Replicate the structure of Truth for the current school
  df <- L1
  df$school_ID <- i
  
  # Add the replicated data to the results dataframe
  Truth <- rbind(Truth, df)
}

n.schools <- 100
set.seed(123)
u <- rnorm(n.schools, mean = 0, sd = 0.5)
set.seed(456)
ei <- rnorm(nrow(Truth), mean = 0, sd = 1)


#GenerateOutcome Variable 
Truth$y<- bA0B1C0*Truth$A0B1C0+ bA1B1C0*Truth$A1B1C0+bA2B1C0*Truth$A2B1C0+
  bA3B1C0*Truth$A3B1C0+ bA4B1C0*Truth$A4B1C0+ bA5B1C0*Truth$A5B1C0+
  bA6B1C0*Truth$A6B1C0 +bA1B0C0*Truth$A1B0C0 + bA2B0C0*Truth$A2B0C0 +
  bA3B0C0*Truth$A3B0C0 +bA4B0C0*Truth$A4B0C0+ bA5B0C0*Truth$A5B0C0 +
  bA6B0C0*Truth$A6B0C0 + 
  bA0B1C1*Truth$A0B1C1+ bA1B1C1*Truth$A1B1C1+bA2B1C1*Truth$A2B1C1+
  bA3B1C1*Truth$A3B1C1+ bA4B1C1*Truth$A4B1C1+ bA5B1C1*Truth$A5B1C1+
  bA6B1C1*Truth$A6B1C1 +bA1B0C1*Truth$A1B0C1 + bA2B0C1*Truth$A2B0C1 +
  bA3B0C1*Truth$A3B0C1 +bA4B0C1*Truth$A4B0C1+ bA5B0C1*Truth$A5B0C1 +
  bA6B0C1*Truth$A6B0C1 + bA0B0C1*Truth$A0B0C1 +
  bA0B1C2*Truth$A0B1C2+ bA1B1C2*Truth$A1B1C2+bA2B1C2*Truth$A2B1C2+
  bA3B1C2*Truth$A3B1C2+ bA4B1C2*Truth$A4B1C2+ bA5B1C2*Truth$A5B1C2+
  bA6B1C2*Truth$A6B1C2 +bA1B0C2*Truth$A1B0C2 + bA2B0C2*Truth$A2B0C2 +
  bA3B0C2*Truth$A3B0C2 +bA4B0C2*Truth$A4B0C2+ bA5B0C2*Truth$A5B0C2 +
  bA6B0C2*Truth$A6B0C2 +bA0B0C2*Truth$A0B0C2 + u+ ei 


mA0B0C0 <- Truth %>% 
  filter(A0B0C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)


mA1B0C0 <- Truth %>% 
  filter(A0B0C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA2B0C0 <- Truth %>% 
  filter(A0B0C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA3B0C0 <- Truth %>% 
  filter(A0B0C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)


mA4B0C0 <- Truth %>% 
  filter(A0B0C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA5B0C0 <- Truth %>% 
  filter(A0B0C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA6B0C0 <- Truth %>% 
  filter(A0B0C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA0B1C0 <- Truth %>% 
  filter(A0B1C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)


mA1B1C0 <- Truth %>% 
  filter(A1B1C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA2B1C0 <- Truth %>% 
  filter(A1B1C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA3B1C0 <- Truth %>% 
  filter(A1B1C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA4B1C0 <- Truth %>% 
  filter(A1B1C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA5B1C0 <- Truth %>% 
  filter(A1B1C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA6B1C0 <- Truth %>% 
  filter(A1B1C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA0B0C1 <- Truth %>% 
  filter(A0B0C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA1B0C1 <- Truth %>% 
  filter(A0B0C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)


mA2B0C1 <- Truth %>% 
  filter(A0B0C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA3B0C1 <- Truth %>% 
  filter(A0B0C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)


mA4B0C1 <- Truth %>% 
  filter(A0B0C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA5B0C1 <- Truth %>% 
  filter(A0B0C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA6B0C1 <- Truth %>% 
  filter(A0B0C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA0B1C1 <- Truth %>% 
  filter(A0B0C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA1B1C1 <- Truth %>% 
  filter(A0B0C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA2B1C1 <- Truth %>% 
  filter(A0B0C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA3B1C1 <- Truth %>% 
  filter(A0B0C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA4B1C1 <- Truth %>% 
  filter(A0B0C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA5B1C1 <- Truth %>% 
  filter(A0B0C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA6B1C1 <- Truth %>% 
  filter(A0B0C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA0B0C2 <- Truth %>% 
  filter(A0B0C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA1B0C2 <- Truth %>% 
  filter(A0B0C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)


mA2B0C2 <- Truth %>% 
  filter(A0B0C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA3B0C2 <- Truth %>% 
  filter(A0B0C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)


mA4B0C2 <- Truth %>% 
  filter(A0B0C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA5B0C2 <- Truth %>% 
  filter(A0B0C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA6B0C2 <- Truth %>% 
  filter(A0B0C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA0B1C2 <- Truth %>% 
  filter(A0B0C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA1B1C2 <- Truth %>% 
  filter(A0B0C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)


mA2B1C2 <- Truth %>% 
  filter(A0B0C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA3B1C2 <- Truth %>% 
  filter(A0B0C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA4B1C2 <- Truth %>% 
  filter(A0B0C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA5B1C2 <- Truth %>% 
  filter(A0B0C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)

mA6B1C2 <- Truth %>% 
  filter(A0B0C0 == 1) %>% 
  summarise(avg_y = mean(y)) %>% 
  pull(avg_y)


#Build Dataset

DL1<- data.frame()


# Sample Size Scenarios
n.list <- c(5000, 10000, 20000)

# Probability Scenarios
prob_list <- list(
  
  characteristic1 = list(A = c(0.143, .143, .143, .143, .143, .143, .143), B = c(.500, .500), C = c(.333, .333, .334)),
  characteristic2 = list(A = c(0.432, .05, .073, .137, .208, .05, .05), B = c(.410, .590), C = c(.145, .467, .700)),
  characteristic3 = list(A = c(0.63, .05, .110, .05, .05, .055, .055), B = c(.200, .800), C = c(.100, .700, .200))
)
datasets <- list()


# Iterate through sample sizes
for (n in n.list) {
  # Iterate through probability characteristics
  for (char in seq_along(prob_list)) {
    # Create the dataset based on current sample size and probability characteristic
    DL <- data.frame(A = sample(c(0:6), n, prob = prob_list[[char]]$A, replace = TRUE),
                     B = sample(c(0,1), n, prob = prob_list[[char]]$B, replace = TRUE),
                     C = sample(c(0:2), n, prob = prob_list[[char]]$C, replace = TRUE))
    # Store in the list with new naming convention
    datasets[[paste0("n_", n, "_p", char)]] <- DL
  }
}




build_dataset <- function(DL1) {
  DL1$cluster <- interaction(DL1$A + 1, DL1$B + 1, DL1$C + 1, sep = "")
  
  
  for (i in 0:6) {
    for (j in 0:1) {
      for (k in 0:2) {
        # Create interaction variable for each combination of A, B, and C
        DL1[[paste0("A", i, "B", j, "C", k)]] <- ifelse(DL1$A == i & DL1$B == j & DL1$C == k, 1, 0)
      }
    }
  }
  
  # Create separate binary variables for A and B levels
  DL1$A0 <- ifelse(DL1$A == 0, 1, 0)
  DL1$A1 <- ifelse(DL1$A == 1, 1, 0)
  DL1$A2 <- ifelse(DL1$A == 2, 1, 0)
  DL1$A3 <- ifelse(DL1$A == 3, 1, 0)
  DL1$A4 <- ifelse(DL1$A == 4, 1, 0)
  DL1$A5 <- ifelse(DL1$A == 5, 1, 0)
  DL1$A6 <- ifelse(DL1$A == 6, 1, 0)
  DL1$B0 <- ifelse(DL1$B == 0, 1, 0)
  DL1$B1 <- ifelse(DL1$B == 1, 1, 0)
  DL1$C1 <- ifelse(DL1$C == 1, 1, 0)
  DL1$C2 <- ifelse(DL1$C == 2, 1, 0)
  
  
  
  
  #Add School ID 
  DL1$school_ID <- sample(1:100, size = nrow(DL1), replace = TRUE)
  
  
  DL1$cluster <- as.numeric(as.character(DL1$cluster))
  DL1$school_ID <- as.numeric(as.character(DL1$school_ID))
  DL1$UniqueCluster <- DL1$cluster + DL1$school_ID
  
  return(DL1)
}

datasets_2 <- list()
datasets_2 <- lapply(datasets, build_dataset)


#Generate Outcome and Apply Standard Deviation

create_outcome_variable <- function(DL) {
  n.schools <- 100
  set.seed(123)
  u <- rnorm(n.schools, mean = 0, sd = 0.5)
  set.seed(456)
  ei <- rnorm(nrow(DL), mean = 0, sd = 1)
  
  # Generate Outcome Variable
  DL$y <- bA0B1C0*DL$A0B1C0+ bA1B1C0*DL$A1B1C0+bA2B1C0*DL$A2B1C0+
    bA3B1C0*DL$A3B1C0+ bA4B1C0*DL$A4B1C0+ bA5B1C0*DL$A5B1C0+
    bA6B1C0*DL$A6B1C0 +bA1B0C0*DL$A1B0C0 + bA2B0C0*DL$A2B0C0 +
    bA3B0C0*DL$A3B0C0 +bA4B0C0*DL$A4B0C0+ bA5B0C0*DL$A5B0C0 +
    bA6B0C0*DL$A6B0C0 + 
    bA0B1C1*DL$A0B1C1+ bA1B1C1*DL$A1B1C1+bA2B1C1*DL$A2B1C1+
    bA3B1C1*DL$A3B1C1+ bA4B1C1*DL$A4B1C1+ bA5B1C1*DL$A5B1C1+
    bA6B1C1*DL$A6B1C1 +bA1B0C1*DL$A1B0C1 + bA2B0C1*DL$A2B0C1 +
    bA3B0C1*DL$A3B0C1 +bA4B0C1*DL$A4B0C1+ bA5B0C1*DL$A5B0C1 +
    bA6B0C1*DL$A6B0C1 + bA0B0C1*DL$A0B0C1 +
    bA0B1C2*DL$A0B1C2+ bA1B1C2*DL$A1B1C2+bA2B1C2*DL$A2B1C2+
    bA3B1C2*DL$A3B1C2+ bA4B1C2*DL$A4B1C2+ bA5B1C2*DL$A5B1C2+
    bA6B1C2*DL$A6B1C2 +bA1B0C2*DL$A1B0C2 + bA2B0C2*DL$A2B0C2 +
    bA3B0C2*DL$A3B0C2 +bA4B0C2*DL$A4B0C2+ bA5B0C2*DL$A5B0C2 +
    bA6B0C2*DL$A6B0C2 +bA0B0C2*DL$A0B0C2 +  u[DL$school_ID] + ei
  
  return(DL)
}

# Apply this function to your datasets_2
datasets_with_y <- lapply(datasets_2, create_outcome_variable)



# Extract unique clusters from one of the datasets
unique_clusters <- unique(datasets_with_y[[1]]$UniqueCluster)


set.seed(12345) 
large_clusters <- sample(unique_clusters, size = length(unique_clusters) * 0.1)


apply_std_manipulation <- function(dataset, std_setting, large_clusters) {
  # Make a copy of the original dataset
  modified_dataset <- dataset
  
  # Compute the overall standard deviation for reference
  overall_sd <- sd(dataset$y)
  
  for(cluster in unique(dataset$UniqueCluster)) {
    # Identify the indices corresponding to the current cluster
    cluster_indices <- which(dataset$UniqueCluster == cluster)
    
    # Extract the y values for the current cluster
    cluster_y <- dataset$y[cluster_indices]
    
    # If only one observation, skip the adjustment
    if (length(cluster_y) == 1) {
      next
    }
    
    # Compute the mean and standard deviation for the current cluster
    current_mean <- mean(cluster_y)
    current_sd <- sd(cluster_y)
    
    # Adjust standard deviation based on the std_setting
    if (std_setting == "small") {
      scaling_factor <- 1
    } else if (std_setting == "large") {
      scaling_factor <- 5 * (current_sd / overall_sd)  
    } else if (std_setting == "mixed") {
      scaling_factor <- ifelse(cluster %in% large_clusters, 5, 1) * (current_sd / overall_sd)  
    } else {
      stop("Invalid std_setting")
    }
    
    # Center, standardize, and then scale the data
    centered_y <- cluster_y - current_mean
    scaled_y <- centered_y * scaling_factor
    
    # Add back the original mean
    modified_y <- scaled_y + current_mean
    
    # Replace the original y values with the modified y values
    modified_dataset$y[cluster_indices] <- modified_y
  }
  
  return(modified_dataset)
}




# Standard Deviation Scenarios
std_list <- c("small", "large", "mixed")

final_datasets <- list()

# Iterate through all the datasets generated earlier
for (i in seq_along(datasets_with_y)) {
  dataset <- datasets_with_y[[i]]
  
  # Iterate through the standard deviation scenarios
  for (std_setting in std_list) {
    # Apply the standard deviation manipulation
    modified_dataset <- apply_std_manipulation(dataset, std_setting, large_clusters)
    
    # Store in the list with a new naming convention
    final_datasets[[paste0(names(datasets_with_y)[i], "_std_", std_setting)]] <- modified_dataset
  }
}

# Now, final_datasets contains 27 datasets, each with a different combination of probability, sample size, and standard deviation.



# List to store all the conditions
all_conditions <- list()

# Number of replications
num_replications <- 10


# Iterate through the sample sizes, probability characteristics, and standard deviation settings

# Iterate through the sample sizes, probability characteristics, and standard deviation settings
for (n in n.list) {
  for (char in seq_along(prob_list)) {
    for (std_setting in std_list) {
      # List to store the replications for this condition
      replications <- list()
      
      # Iterate for each replication
      for (replication in 1:num_replications) {
        # Set a different seed for each replication
        set.seed(12345 + replication)
        
        # Create the dataset based on current sample size and probability characteristic
        DL <- data.frame(A = sample(c(0:6), n, prob = prob_list[[char]]$A, replace = TRUE),
                         B = sample(c(0,1), n, prob = prob_list[[char]]$B, replace = TRUE),
                         C = sample(c(0:2), n, prob = prob_list[[char]]$C, replace = TRUE))
        # Build the dataset
        dataset <- build_dataset(DL)
        # Generate outcome variable
        dataset <- create_outcome_variable(dataset)
        # Apply the standard deviation manipulation
        modified_dataset <- apply_std_manipulation(dataset, std_setting, large_clusters)
        
        # Store in the list with a new naming convention
        replications[[paste0("replication_", replication)]] <- modified_dataset
      }
      
      # Store this condition in the all_conditions list
      condition_name <- paste0("n_", n, "_p", char, "_std_", std_setting)
      all_conditions[[condition_name]] <- replications
    }
  }
}


#####

#####

#Model & Store Results




covariate_names_categorical <- c("A1B0C0", "A2B0C0", "A3B0C0", "A4B0C0", "A5B0C0", "A6B0C0", 
                                 "A0B1C0", "A1B1C0", "A2B1C0", "A3B1C0", "A4B1C0", "A5B1C0", "A6B1C0",
                                 "A1B0C1", "A2B0C1", "A3B0C1", "A4B0C1", "A5B0C1", "A6B0C1", 
                                 "A0B1C1", "A1B1C1", "A2B1C1", "A3B1C1", "A4B1C1", "A5B1C1", "A6B1C1", 
                                 
                                 "A1B0C2", "A2B0C2", "A3B0C2", "A4B0C2", "A5B0C2", "A6B0C2", 
                                 "A0B1C2", "A1B1C2", "A2B1C2", "A3B1C2", "A4B1C2", "A5B1C2", "A6B1C2",
                                 "A0B0C1", "A0B0C2"
                                 
                                 
)

covariate_names_interaction<- c( "A1", "A2", "A3", "A4", "A5", "A6", "B1", "C1", "C2", 
                                 "A1B1C1", "A2B1C1", "A3B1C1", "A4B1C1", "A5B1C1", "A6B1C1",
                                 "A1B1C2", "A2B1C2", "A3B1C2", "A4B1C2", "A5B1C2", "A6B1C2")




get_fixed_effects <- function(model, covariate_names) {
  result_row <- setNames(data.frame(matrix(ncol = length(covariate_names), nrow = 1)), covariate_names)
  coefficients <- fixef(model)
  matched_indices <- match(names(coefficients), covariate_names)
  valid_indices <- !is.na(matched_indices)
  result_row[, matched_indices[valid_indices]] <- coefficients[valid_indices]
  return(result_row)
}

# Custom function to get p-values
get_p_values <- function(model, covariate_names) {
  result_row <- setNames(data.frame(matrix(ncol = length(covariate_names), nrow = 1)), covariate_names)
  p_values <- summary(model)$coefficients[, "Pr(>|t|)"]
  matched_indices <- match(names(p_values), covariate_names)
  valid_indices <- !is.na(matched_indices)
  result_row[, matched_indices[valid_indices]] <- p_values[valid_indices]
  return(result_row)
}

# Lists to store results for each condition
categorical_p_values_list <- list()
categorical_results <- list()

interaction_p_values_list <- list()
interaction_results <- list()

dropped_coefficients_info <- list()
categorical_aic_bic_values_list <- list()
categorical_condition_ci_values <- data.frame()
categorical_ci_values_list <- list()
interaction_aic_bic_values_list <- list()
interaction_condition_ci_values <- data.frame()
interaction_ci_values_list <- list()

nested_categorical_results <- list()
nested_interaction_results <- list()

categorical_icc_values_list <- list()
interaction_icc_values_list <- list()



#####

#Model & Store Results


# Iterate through the conditions


for (condition_name in names(all_conditions)) {
  # Initialize an empty data frame to store p-values for this condition
  
  condition_results_categorical <- data.frame(matrix(ncol = length(covariate_names_categorical), nrow = 0))
  colnames(condition_results_categorical) <- covariate_names_categorical
  
  condition_results_interaction <- data.frame(matrix(ncol = length(covariate_names_interaction), nrow = 0))
  colnames(condition_results_interaction) <- covariate_names_interaction
  
  
  categorical_condition_aic_bic <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(categorical_condition_aic_bic) <- c("AIC", "BIC")
  
  interaction_condition_aic_bic <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(interaction_condition_aic_bic) <- c("AIC", "BIC")
  
  
  condition_p_values_categorical <- data.frame(matrix(ncol = length(covariate_names_categorical), nrow = 0))
  colnames(condition_p_values_categorical) <- covariate_names_categorical
  
  condition_p_values_interaction <- data.frame(matrix(ncol = length(covariate_names_interaction), nrow = 0))
  colnames(condition_p_values_interaction) <- covariate_names_interaction
  
  
  dropped_info_categorical <- list()
  dropped_info_interaction <- list()
  
  categorical_condition_icc_values <- c()
  interaction_condition_icc_values <- c()
  
  num_coefficients <- ncol(condition_results_categorical)
  condition_ci_values_categorical <- data.frame(matrix(ncol = 2 * num_coefficients, nrow = 0))
  colnames(condition_ci_values_categorical) <- c(sapply(colnames(condition_results_categorical), function(x) paste0(x, "_LL")), sapply(colnames(condition_results_categorical), function(x) paste0(x, "_UL")))
  
  
  num_coefficients <- ncol(condition_results_interaction)
  condition_ci_values_interaction<- data.frame(matrix(ncol = 2 * num_coefficients, nrow = 0))
  colnames(condition_ci_values_interaction) <- c(sapply(colnames(condition_results_interaction), function(x) paste0(x, "_LL")), sapply(colnames(condition_results_interaction), function(x) paste0(x, "_UL")))
  
  
  # Iterate through the replications within this condition
  for (replication_name in names(all_conditions[[condition_name]])) {
    # Retrieve the dataset
    DL <- all_conditions[[condition_name]][[replication_name]]
    
    
    
    # Fit the categorical model
    categorical_model <- lmer(y ~ A1B0C0 + A2B0C0 + A3B0C0 + A4B0C0 + A5B0C0 + A6B0C0 + A0B1C0 + A1B1C0 + A2B1C0
                              + A3B1C0 + A4B1C0 + A5B1C0 + A6B1C0 
                              + A1B0C1 + A2B0C1 + A3B0C1 + A4B0C1 + A5B0C1 + A6B0C1 + A0B1C1 + A1B1C1 + A2B1C1
                              + A3B1C1 + A4B1C1 + A5B1C1 + A6B1C1 
                              + A1B0C2 + A2B0C2 + A3B0C2 + A4B0C2 + A5B0C2 + A6B0C2 + A0B1C2 + A1B1C2 + A2B1C2
                              + A3B1C2 + A4B1C2 + A5B1C2 + A6B1C2 + A0B0C1 + A0B0C2 +(1 | school_ID), data = DL)
    
    
    
    var_components_cat <- VarCorr(categorical_model)
    
    
    var_random <- attr(var_components_cat$school_ID, "stddev")[1]^2
    var_residual <- attr(var_components_cat, "sc")^2
    icc_categorical <- var_random / (var_random + var_residual)
    
    
    # Store ICC value for this replication in the vector
    categorical_condition_icc_values <- c(categorical_condition_icc_values, icc_categorical)
    
    #fit the interaction model
    
    interaction_model<-lmer(y~A1 + A2 + A3 + A4 + A5 + A6 + B1 + C1 + C2 
                            +  A1B1C1 + A2B1C1 + A3B1C1 + A4B1C1 + A5B1C1 + A6B1C1
                            + A1B1C2 + A2B1C2 + A3B1C2 + A4B1C2 + A5B1C2 + A6B1C2
                            + (1 | school_ID), data = DL)
    
    var_components_int <- VarCorr(interaction_model)
    var_random <- attr(var_components_int$school_ID, "stddev")[1]^2
    var_residual <- attr(var_components_int, "sc")^2
    icc_interaction <- var_random / (var_random + var_residual)
    
    # Store ICC value for this replication in the vector
    interaction_condition_icc_values <- c(interaction_condition_icc_values, icc_interaction)
    
    
    # Extract AIC and BIC
    categorical_condition_aic_bic <- rbind(categorical_condition_aic_bic, data.frame(AIC=AIC(categorical_model), BIC=BIC(categorical_model)))
    interaction_condition_aic_bic <- rbind(interaction_condition_aic_bic, data.frame(AIC=AIC(interaction_model), BIC=BIC(interaction_model)))
    
    
    # Retrieve and store results
    condition_results_categorical <- rbind(condition_results_categorical, get_fixed_effects(categorical_model, covariate_names_categorical))
    condition_results_interaction <- rbind(condition_results_interaction, get_fixed_effects(interaction_model, covariate_names_interaction))
    
    
    categorical_coefficients <- condition_results_categorical[nrow(condition_results_categorical),, drop = FALSE]
    
    condition_p_values_categorical <- rbind(condition_p_values_categorical, get_p_values(categorical_model, covariate_names_categorical))
    condition_p_values_interaction <- rbind(condition_p_values_interaction, get_p_values(interaction_model, covariate_names_interaction))
    
    
    
    
    # Compute standard errors from your model (or however you wish)
    categorical_se <- sqrt(diag(vcov(categorical_model)))[-1]
    
    # Account for rank deficiency by using NA for dropped coefficients
    categorical_se[is.na(categorical_coefficients)] <- NA
    
    # Calculate the z-value for the desired confidence level
    z_value <- qnorm(1 - (1 - 0.95) / 2)
    
    categorical_ci <- data.frame(
      LL = ifelse(is.na(categorical_coefficients), NA, unlist(categorical_coefficients) - z_value * categorical_se),
      UL = ifelse(is.na(categorical_coefficients), NA, unlist(categorical_coefficients) + z_value * categorical_se)
    )
    
    # Flatten and combine LL and UL into one vector
    categorical_ci_flattened <- as.vector(t(categorical_ci))
    
    # Turn this vector into a one-row data.frame
    categorical_ci_reshaped <- data.frame(t(categorical_ci_flattened), stringsAsFactors = FALSE)
    
    # Ensure the column names match those of condition_ci_values_categorical
    colnames(categorical_ci_reshaped) <- colnames(condition_ci_values_categorical)
    
    # Append to the data frame storing CI values for this condition
    condition_ci_values_categorical <- rbind(condition_ci_values_categorical, categorical_ci_reshaped)
    
    
    
    
    
    interaction_coefficients <- condition_results_interaction[nrow(condition_results_interaction),, drop = FALSE]
    
    
    # Compute standard errors from your model (or however you wish)
    interaction_se <- sqrt(diag(vcov(interaction_model)))[-1]
    
    # Account for rank deficiency by using NA for dropped coefficients
    interaction_se[is.na(interaction_coefficients)] <- NA
    
    # Calculate the z-value for the desired confidence level
    z_value <- qnorm(1 - (1 - 0.95) / 2)
    
    interaction_ci <- data.frame(
      LL = ifelse(is.na(interaction_coefficients), NA, unlist(interaction_coefficients) - z_value * interaction_se),
      UL = ifelse(is.na(interaction_coefficients), NA, unlist(interaction_coefficients) + z_value * interaction_se)
    )
    
    # Flatten and combine LL and UL into one vector
    interaction_ci_flattened <- as.vector(t(interaction_ci))
    
    # Turn this vector into a one-row data.frame
    interaction_ci_reshaped <- data.frame(t(interaction_ci_flattened), stringsAsFactors = FALSE)
    
    # Ensure the column names match those of condition_ci_values_categorical
    colnames(interaction_ci_reshaped) <- colnames(condition_ci_values_interaction)
    
    # Append to the data frame storing CI values for this condition
    condition_ci_values_interaction <- rbind(condition_ci_values_interaction, interaction_ci_reshaped)
    
    
    # Iterate through the replications within this condition
    for (replication_num in seq_len(nrow(condition_results_categorical))) {
      
      # Check for dropped coefficients in the categorical model
      na_indices_categorical <- which(is.na(condition_results_categorical[replication_num, ]))
      if (length(na_indices_categorical) > 0) {
        dropped_info_categorical[[as.character(replication_num)]] <- covariate_names_categorical[na_indices_categorical]
      }
      
      # Check for dropped coefficients in the interaction model
      na_indices_interaction <- which(is.na(condition_results_interaction[replication_num, ]))
      if (length(na_indices_interaction) > 0) {
        dropped_info_interaction[[as.character(replication_num)]] <- covariate_names_interaction[na_indices_interaction]
      }
    }
    
    # Store the dropped coefficients info for this condition
    if (length(dropped_info_categorical) > 0) {
      dropped_coefficients_info[[paste0("Categorical_", condition_name)]] <- dropped_info_categorical
    }
    if (length(dropped_info_interaction) > 0) {
      dropped_coefficients_info[[paste0("Interaction_", condition_name)]] <- dropped_info_interaction
    }
    
  }
  # Store results for each condition
  
  nested_categorical_results[[condition_name]] <- condition_results_categorical
  nested_interaction_results[[condition_name]] <- condition_results_interaction
  
  
  categorical_aic_bic_values_list[[condition_name]] <- categorical_condition_aic_bic
  interaction_aic_bic_values_list[[condition_name]] <- interaction_condition_aic_bic
  
  categorical_ci_values_list[[condition_name]] <- condition_ci_values_categorical
  interaction_ci_values_list[[condition_name]] <- condition_ci_values_interaction
  
  
  categorical_p_values_list[[condition_name]] <- condition_p_values_categorical
  interaction_p_values_list[[condition_name]] <- condition_p_values_interaction
  
  # Explicitly setting the column names
  colnames(condition_results_categorical) <- covariate_names_categorical
  colnames(condition_results_interaction) <- covariate_names_interaction
  categorical_icc_values_list[[condition_name]] <- categorical_condition_icc_values
  interaction_icc_values_list[[condition_name]] <- interaction_condition_icc_values
  
}


categorical_results<- nested_categorical_results
interaction_results<-nested_interaction_results



# MAidha




# Separate list to store the intercepts for each condition
maidha_intercepts_list <- list()
maidha_ci_values_list <- list()
maidha_aic_bic_values_list <- list()

hyp_test_interval <- list()
hyp_test_maidha <- list()
non_converged_instances <- list()
dropped_coefficients_info <- list()

maidha_icc_values_list <- list()


# Create a cluster mapping
cluster_mapping <- c("111" = "A0B0C0", "112" = "A0B0C1", "113" = "A0B0C2",
                     "121" = "A0B1C0", "122" = "A0B1C1", "123" = "A0B1C2",
                     "211" = "A1B0C0", "212" = "A1B0C1", "213" = "A1B0C2",
                     "221" = "A1B1C0", "222" = "A1B1C1", "223" = "A1B1C2",
                     "311" = "A2B0C0", "312" = "A2B0C1", "313" = "A2B0C2",
                     "321" = "A2B1C0", "322" = "A2B1C1", "323" = "A2B1C2",
                     "411" = "A3B0C0", "412" = "A3B0C1", "413" = "A3B0C2",
                     "421" = "A3B1C0", "422" = "A3B1C1", "423" = "A3B1C2",
                     "511" = "A4B0C0", "512" = "A4B0C1", "513" = "A4B0C2",
                     "521" = "A4B1C0", "522" = "A4B1C1", "523" = "A4B1C2",
                     "611" = "A5B0C0", "612" = "A5B0C1", "613" = "A5B0C2",
                     "621" = "A5B1C0", "622" = "A5B1C1", "623" = "A5B1C2",
                     "711" = "A6B0C0", "712" = "A6B0C1", "713" = "A6B0C2",
                     "721" = "A6B1C0", "722" = "A6B1C1", "723" = "A6B1C2")

# Iterate through the conditions
for (condition_name in names(all_conditions)) {
  all_unique_clusters <- sort(unique(unlist(lapply(all_conditions[[condition_name]], function(x) unique(x$cluster)))))
  maidha_condition_icc_values <- numeric()
  
  maidha_condition_icc_values <- c()
  non_converged_replications <- c()
  # Initialize condition_intercepts with NAs
  condition_intercepts <- data.frame(matrix(NA, ncol = length(all_unique_clusters), nrow = num_replications))
  condition_ci_values <- data.frame(matrix(NA, ncol = length(all_unique_clusters) * 2, nrow = num_replications))
  
  # Initialize data frames to store condition-specific intercepts and CI values
  maidha_condition_aic_bic <- data.frame(AIC=numeric(), BIC=numeric())
  
  
  condition_intervals <- data.frame(matrix(ncol = 84, nrow = num_replications))  
  condition_hyp_test <- data.frame(matrix(ncol = 42, nrow = num_replications))
  
  
  cluster_names<- NULL
  # Iterate through the replications within this condition
  for (replication_name in 1:num_replications) {
    # Retrieve the dataset
    DL <- all_conditions[[condition_name]][[replication_name]]
    
    
    conv_check <- FALSE
    
    # Validate cluster ids before model fitting
    valid_clusters = all_unique_clusters %in% unique(DL$cluster)
    if (any(!valid_clusters)) {
      print("Mismatch in cluster ids.")
      next
    }
    
    withCallingHandlers(
      expr = {
        maidha_model <- lmer(y ~ A1 + A2 + A3 + A4 + A5 + A6 + B1 + C1 + C2 + (1 | cluster) + (1 | school_ID), data = DL)
      },
      warning = function(w) {
        if (grepl("Model failed to converge", w$message)) {
          conv_check <<- TRUE
        }
        invokeRestart("muffleWarning")
      }
    )
    
    if (conv_check) {
      non_converged_replications <- c(non_converged_replications, replication_name)
    }
    
    
    
    non_converged_instances[[condition_name]] <- non_converged_replications
    
    var_components_maidha <- VarCorr(maidha_model)
    
    var_random <- attr(var_components_maidha$school_ID, "stddev")[1]^2
    var_residual <- attr(var_components_maidha, "sc")^2
    icc_maidha <- var_random / (var_random + var_residual)
    
    non_converged_instances[[condition_name]] <- non_converged_replications
    maidha_condition_icc_values <- c(maidha_condition_icc_values, icc_maidha)
    
    
    
    # Extract random effects for clusters
    
    
    cluster_intercepts <- ranef(maidha_model)$cluster[,1] 
    condition_intercepts[replication_name, ] <- cluster_intercepts
    
    # Create a vector to store the full set of intercepts
    full_cluster_intercepts <- rep(NA, length(all_unique_clusters))
    
    # Extract the cluster names from the model
    model_cluster_names <- rownames(ranef(maidha_model)$cluster)
    
    # Assign the model's cluster intercepts to their corresponding positions in the full vector
    #full_cluster_intercepts[match(model_cluster_names, all_unique_clusters)] <- cluster_intercepts
    
    # Assign the full set of intercepts to the condition_intercepts data frame
    condition_intercepts[replication_name, ] <- full_cluster_intercepts
    
    
    maidha_condition_aic_bic <- rbind(maidha_condition_aic_bic, data.frame(AIC=AIC(maidha_model), BIC=BIC(maidha_model)))
    
    
    # Assign the model's cluster intercepts to their corresponding positions in the full vector
    condition_intercepts[replication_name, match(model_cluster_names, all_unique_clusters)] <- cluster_intercepts
    condition_intercepts[replication_name, match(model_cluster_names, all_unique_clusters)] <- cluster_intercepts
    
    
    rep_intercepts <- setNames(rep(NA, length(all_unique_clusters)), all_unique_clusters)
    
    # Fill in the NAs with the calculated intercepts
    rep_intercepts[model_cluster_names] <- cluster_intercepts
    
    # Store this in your data frame
    condition_intercepts[replication_name, ] <- rep_intercepts
    
    
    vcov_cluster <- VarCorr(maidha_model)$cluster
    se_intercept <- sqrt(vcov_cluster[1, 1])
    z_value <- 1.96 # For a 95% confidence interval
    
    # Create a vector to store the full set of CI values
    full_combined_ci_values <- rep(NA, length(all_unique_clusters) * 2)  # Multiply by 2 to account for lower and upper limits
    
    # Compute CI values
    ci_values_LL <- cluster_intercepts - z_value * se_intercept
    ci_values_UL <- cluster_intercepts + z_value * se_intercept
    
    # Combine LL and UL in a single row
    combined_ci_values <- as.numeric(c(ci_values_LL, ci_values_UL))
    
    # Position the computed CI values in the full CI vector
    full_combined_ci_values[c(match(model_cluster_names, all_unique_clusters), match(model_cluster_names, all_unique_clusters) + length(all_unique_clusters))] <- combined_ci_values
    
    # Assign the full set of CI values to the condition_ci_values data frame
    condition_ci_values[replication_name, ] <- full_combined_ci_values
    
    if (is.null(cluster_names)) {
      cluster_names <- rownames(ranef(maidha_model)$cluster)
    }
    
    random_effects <- ranef(maidha_model)$cluster
    postVar <- attr(random_effects, "postVar")
    std_errors <- numeric(nrow(random_effects))
    
    for (i in 1:nrow(random_effects)) {
      std_errors[i] <- sqrt(postVar[, , i])
    }
    
    lower_bound <- cluster_intercepts - 1.96 * std_errors
    upper_bound <- cluster_intercepts + 1.96 * std_errors
    
    condition_intervals[replication_name, seq(1, 84, by = 2)] <- lower_bound
    condition_intervals[replication_name, seq(2, 84, by = 2)] <- upper_bound
    
    # Test whether confidence intervals contain zero
    contains_zero <- ifelse(lower_bound < 0 & upper_bound > 0, 0, 1)
    
    # Store the result in the condition_hyp_test DataFrame
    condition_hyp_test[replication_name, ] <- contains_zero
    
    
    
    
    # Store cluster names if not already stored
    if (is.null(cluster_names)) {
      cluster_names <- rownames(ranef(maidha_model)$cluster)
    }
  }
  
  # Store the condition-specific intercepts and CI values in the overall results lists
  
  lower_cols <- sapply(cluster_mapping[cluster_names], function(x) paste0("LL_", x))
  upper_cols <- sapply(cluster_mapping[cluster_names], function(x) paste0("UL_", x))
  
  colnames(condition_intercepts) <- colnames(condition_hyp_test) <- cluster_mapping[cluster_names]
  colnames(condition_intervals) <- c(rbind(lower_cols, upper_cols))
  new_colnames <- c(paste0(cluster_mapping[cluster_names], "_LL"), paste0(cluster_mapping[cluster_names], "_UL"))
  
  
  
  colnames(condition_intercepts) <- cluster_mapping[cluster_names]
  colnames(condition_ci_values) <- new_colnames
  
  maidha_intercepts_list[[condition_name]] <- condition_intercepts
  maidha_ci_values_list[[condition_name]] <- condition_ci_values  
  maidha_aic_bic_values_list[[condition_name]] <- maidha_condition_aic_bic
  hyp_test_interval[[condition_name]] <- condition_intervals
  hyp_test_maidha[[condition_name]] <- condition_hyp_test
  colnames(condition_ci_values) <- new_colnames
  
  maidha_icc_values_list[[condition_name]] <- maidha_condition_icc_values
  
}




#DROP CONDITOINS FOR MISSING CASES


NA_maidha_intercepts_list <- maidha_intercepts_list
NA_hyp_test_maidha <- hyp_test_maidha 
NA_maidha_aic_bic_values_list <- maidha_aic_bic_values_list 
NA_hyp_test_interval<- hyp_test_interval
NA_interaction_results<-interaction_results
NA_categorical_results<-categorical_results
NA_interaction_p_values_list<-interaction_p_values_list
NA_categorical_p_values_list<-categorical_p_values_list
NA_interaction_aic_bic_list<-interaction_aic_bic_values_list
NA_categorical_aic_bic_values_list<-categorical_aic_bic_values_list
NA_interaction_ci_values_list<-interaction_ci_values_list
NA_categorical_ci_values_list<-categorical_ci_values_list
NA_maidha_ci_values_list<-maidha_ci_values_list

# Initialize a vector to hold condition_names that have NA values
conditions_with_na <- c()

# Loop through each condition_name in nested_categorical_results to find NAs
for (condition_name in names(nested_categorical_results)) {
  # Check if any NA values in the data frame of this condition
  if (any(is.na(nested_categorical_results[[condition_name]]))) {
    # Add the condition_name to the conditions_with_na vector
    conditions_with_na <- c(conditions_with_na, condition_name)
  }
}

# Remove the conditions with NA from each list
list_names <- c(
  
  "maidha_intercepts_list",
  "hyp_test_maidha",
  "maidha_aic_bic_values_list",
  "hyp_test_interval",
  "interaction_results",
  "categorical_results",
  "interaction_p_values_list",
  "categorical_p_values_list",
  "interaction_aic_bic_values_list",
  "categorical_aic_bic_values_list",
  "interaction_ci_values_list",
  "categorical_ci_values_list",
  "maidha_ci_values_list"
)

# Loop through each list name and remove conditions with NA
for (list_name in list_names) {
  assign(list_name, eval(parse(text=list_name))[!names(eval(parse(text=list_name))) %in% conditions_with_na], envir=.GlobalEnv)
}


###Outcome Generation####

#Average of estiamtes across 1000 reps 

# Initialize empty data frames to store average coefficients
categorical_avg_coefficients <- data.frame(matrix(ncol = length(covariate_names_categorical), nrow = 0))
interaction_avg_coefficients <- data.frame(matrix(ncol = length(covariate_names_interaction), nrow = 0))

# Set column names based on your predefined lists
colnames(categorical_avg_coefficients) <- covariate_names_categorical
colnames(interaction_avg_coefficients) <- covariate_names_interaction

# Iterate through the conditions
for (condition_name in names(categorical_results)) {
  # Retrieve the coefficients for this condition for both models
  categorical_condition_coefficients <- categorical_results[[condition_name]]
  interaction_condition_coefficients <- interaction_results[[condition_name]]
  
  # Calculate the average across the replications for both models
  avg_categorical_coefficients <- colMeans(categorical_condition_coefficients)
  avg_interaction_coefficients <- colMeans(interaction_condition_coefficients)
  
  # Add condition name as a row name and bind to the overall results
  avg_categorical_coefficients <- data.frame(t(avg_categorical_coefficients))
  avg_interaction_coefficients <- data.frame(t(avg_interaction_coefficients))
  rownames(avg_categorical_coefficients) <- condition_name
  rownames(avg_interaction_coefficients) <- condition_name
  
  categorical_avg_coefficients <- rbind(categorical_avg_coefficients, avg_categorical_coefficients)
  interaction_avg_coefficients <- rbind(interaction_avg_coefficients, avg_interaction_coefficients)
}

colnames(categorical_avg_coefficients) <- colnames(categorical_results[[1]])
colnames(interaction_avg_coefficients) <- colnames(interaction_results[[1]])


#Then get the average for intersections of the interaction model
intersectional_effects <- interaction_avg_coefficients
intersectional_effects$A1B1C1 <- interaction_avg_coefficients$A1 + interaction_avg_coefficients$B1 + interaction_avg_coefficients$C1 + interaction_avg_coefficients$A1B1C1
intersectional_effects$A2B1C1 <- interaction_avg_coefficients$A2 + interaction_avg_coefficients$B1 +  interaction_avg_coefficients$C1 + interaction_avg_coefficients$A2B1C1
intersectional_effects$A3B1C1 <- interaction_avg_coefficients$A3 + interaction_avg_coefficients$B1 + interaction_avg_coefficients$C1  + interaction_avg_coefficients$A3B1C1
intersectional_effects$A4B1C1 <- interaction_avg_coefficients$A4 + interaction_avg_coefficients$B1 + interaction_avg_coefficients$C1 + interaction_avg_coefficients$A4B1C1
intersectional_effects$A5B1C1 <- interaction_avg_coefficients$A5 + interaction_avg_coefficients$B1 + interaction_avg_coefficients$C1 + interaction_avg_coefficients$A5B1C1
intersectional_effects$A6B1C1 <- interaction_avg_coefficients$A6 + interaction_avg_coefficients$B1 + interaction_avg_coefficients$C1  + interaction_avg_coefficients$A6B1C1

intersectional_effects$A1B1C2 <- interaction_avg_coefficients$A1 + interaction_avg_coefficients$B1 + interaction_avg_coefficients$C2 + interaction_avg_coefficients$A1B1C2
intersectional_effects$A2B1C2 <- interaction_avg_coefficients$A2 + interaction_avg_coefficients$B1 + interaction_avg_coefficients$C2 + interaction_avg_coefficients$A2B1C2
intersectional_effects$A3B1C2 <- interaction_avg_coefficients$A3 + interaction_avg_coefficients$B1 + interaction_avg_coefficients$C2  + interaction_avg_coefficients$A3B1C2
intersectional_effects$A4B1C2 <- interaction_avg_coefficients$A4 + interaction_avg_coefficients$B1 + interaction_avg_coefficients$C2 + interaction_avg_coefficients$A4B1C2
intersectional_effects$A5B1C2 <- interaction_avg_coefficients$A5 + interaction_avg_coefficients$B1 + interaction_avg_coefficients$C2 + interaction_avg_coefficients$A5B1C2
intersectional_effects$A6B1C2 <- interaction_avg_coefficients$A6 + interaction_avg_coefficients$B1 + interaction_avg_coefficients$C2  + interaction_avg_coefficients$A6B1C2
interaction_int_effects<- intersectional_effects 



#Maidha

# Initialize an empty data frame to store the average intercepts for MAIDHA
maidha_avg_intercepts <- data.frame()

# Iterate through the conditions
for (condition_name in names(maidha_intercepts_list)) {
  # Retrieve the intercepts for this condition
  maidha_condition_intercepts <- maidha_intercepts_list[[condition_name]]
  
  # Calculate the average across the replications
  avg_maidha_intercepts <- colMeans(maidha_condition_intercepts)
  
  # Convert to a data frame and set the condition name as a row name
  avg_maidha_intercepts_df <- data.frame(t(avg_maidha_intercepts))
  rownames(avg_maidha_intercepts_df) <- condition_name
  
  # Bind it to the overall results
  maidha_avg_intercepts <- rbind(maidha_avg_intercepts, avg_maidha_intercepts_df)
}

# Set the column names
colnames(maidha_avg_intercepts) <- colnames(maidha_intercepts_list[[1]])





#Average AIC BIC 

#Categorial
# Initialize an empty data frame to store the average AIC and BIC for each condition
average_aic_bic_categorical <- data.frame()

# Iterate through the list to calculate the averages
for (condition_name in names(categorical_aic_bic_values_list)) {
  condition_df <- categorical_aic_bic_values_list[[condition_name]]
  avg_values <- colMeans(condition_df, na.rm = TRUE)  # Calculate the column-wise mean
  avg_values <- as.data.frame(t(avg_values))  # Transpose to row format
  rownames(avg_values) <- condition_name  # Assign condition name as row name
  
  average_aic_bic_categorical <- rbind(average_aic_bic_categorical, avg_values)  # Append to the final dataframe
}


#Interaction

# Initialize an empty data frame to store the average AIC and BIC for each condition
average_aic_bic_interaction <- data.frame()

# Iterate through the list to calculate the averages
for (condition_name in names(interaction_aic_bic_values_list)) {
  condition_df <- interaction_aic_bic_values_list[[condition_name]]
  avg_values <- colMeans(condition_df, na.rm = TRUE)  # Calculate the column-wise mean
  avg_values <- as.data.frame(t(avg_values))  # Transpose to row format
  rownames(avg_values) <- condition_name  # Assign condition name as row name
  
  average_aic_bic_interaction <- rbind(average_aic_bic_interaction, avg_values)  # Append to the final dataframe
}

#Maidha

# Initialize an empty data frame to store the average AIC and BIC for each condition
average_aic_bic_maidha <- data.frame()

# Iterate through the list to calculate the averages
for (condition_name in names(maidha_aic_bic_values_list)) {
  condition_df <- maidha_aic_bic_values_list[[condition_name]]
  avg_values <- colMeans(condition_df, na.rm = TRUE)  # Calculate the column-wise mean
  avg_values <- as.data.frame(t(avg_values))  # Transpose to row format
  rownames(avg_values) <- condition_name  # Assign condition name as row name
  
  average_aic_bic_maidha <- rbind(average_aic_bic_maidha, avg_values)  # Append to the final dataframe
}


### Average CI Values 

# Initialize an empty list to hold the average CI values for each condition
avg_categorical_ci_values <- data.frame()

# Loop over each condition to calculate the average UL and LL for each coefficient
for (condition_name in names(categorical_ci_values_list)) {
  condition_data <- categorical_ci_values_list[[condition_name]]
  
  # Calculate the mean of UL and LL for each coefficient
  avg_condition_data <- colMeans(condition_data, na.rm = TRUE)
  
  # Convert to a data frame and set row names as condition_name
  avg_condition_data_df <- as.data.frame(t(avg_condition_data))
  rownames(avg_condition_data_df) <- condition_name
  
  # rbind this row to the overall data frame
  avg_categorical_ci_values <- rbind(avg_categorical_ci_values, avg_condition_data_df)
}

# Assign the column names based on one of the original data frames inside your list
colnames(avg_categorical_ci_values) <- colnames(categorical_ci_values_list[[1]])




#Interaction

intersectional_ci_values_list <- list()

for (condition_name in names(interaction_ci_values_list)) {
  interaction_condition_coefficients <- interaction_ci_values_list[[condition_name]]
  
  # Directly create the data frame with the calculated values
  intersectional_effects <- interaction_condition_coefficients
  intersectional_effects$A1B1C1_LL <- interaction_condition_coefficients$A1_LL + interaction_condition_coefficients$B1_LL + interaction_condition_coefficients$C1_LL + interaction_condition_coefficients$A1B1C1_LL 
  intersectional_effects$A2B1C1_LL <- interaction_condition_coefficients$A2_LL + interaction_condition_coefficients$B1_LL + interaction_condition_coefficients$C1_LL + interaction_condition_coefficients$A2B1C1_LL
  intersectional_effects$A3B1C1_LL <- interaction_condition_coefficients$A3_LL + interaction_condition_coefficients$B1_LL + interaction_condition_coefficients$C1_LL + interaction_condition_coefficients$A3B1C1_LL  
  intersectional_effects$A4B1C1_LL <- interaction_condition_coefficients$A4_LL + interaction_condition_coefficients$B1_LL + interaction_condition_coefficients$C1_LL + interaction_condition_coefficients$A4B1C1_LL
  intersectional_effects$A5B1C1_LL <- interaction_condition_coefficients$A5_LL + interaction_condition_coefficients$B1_LL + interaction_condition_coefficients$C1_LL + interaction_condition_coefficients$A5B1C1_LL
  intersectional_effects$A6B1C1_LL <- interaction_condition_coefficients$A6_LL + interaction_condition_coefficients$B1_LL + interaction_condition_coefficients$C1_LL + interaction_condition_coefficients$A6B1C1_LL
  
  intersectional_effects$A1B1C2_LL <- interaction_condition_coefficients$A1_LL + interaction_condition_coefficients$B1_LL + interaction_condition_coefficients$C2_LL + interaction_condition_coefficients$A1B1C2_LL
  intersectional_effects$A2B1C2_LL <- interaction_condition_coefficients$A2_LL + interaction_condition_coefficients$B1_LL + interaction_condition_coefficients$C2_LL + interaction_condition_coefficients$A2B1C2_LL
  intersectional_effects$A3B1C2_LL <- interaction_condition_coefficients$A3_LL + interaction_condition_coefficients$B1_LL + interaction_condition_coefficients$C2_LL + interaction_condition_coefficients$A3B1C2_LL  
  intersectional_effects$A4B1C2_LL <- interaction_condition_coefficients$A4_LL + interaction_condition_coefficients$B1_LL + interaction_condition_coefficients$C2_LL + interaction_condition_coefficients$A4B1C2_LL
  intersectional_effects$A5B1C2_LL <- interaction_condition_coefficients$A5_LL + interaction_condition_coefficients$B1_LL + interaction_condition_coefficients$C2_LL + interaction_condition_coefficients$A5B1C2_LL
  intersectional_effects$A6B1C2_LL <- interaction_condition_coefficients$A6_LL + interaction_condition_coefficients$B1_LL + interaction_condition_coefficients$C2_LL + interaction_condition_coefficients$A6B1C2_LL
  
  intersectional_effects$A1B1C1_UL <- interaction_condition_coefficients$A1_UL + interaction_condition_coefficients$B1_UL + interaction_condition_coefficients$C1_UL + interaction_condition_coefficients$A1B1C1_UL 
  intersectional_effects$A2B1C1_UL <- interaction_condition_coefficients$A2_UL + interaction_condition_coefficients$B1_UL + interaction_condition_coefficients$C1_UL + interaction_condition_coefficients$A2B1C1_UL
  intersectional_effects$A3B1C1_UL <- interaction_condition_coefficients$A3_UL + interaction_condition_coefficients$B1_UL + interaction_condition_coefficients$C1_UL + interaction_condition_coefficients$A3B1C1_UL  
  intersectional_effects$A4B1C1_UL <- interaction_condition_coefficients$A4_UL + interaction_condition_coefficients$B1_UL + interaction_condition_coefficients$C1_UL + interaction_condition_coefficients$A4B1C1_UL
  intersectional_effects$A5B1C1_UL <- interaction_condition_coefficients$A5_UL + interaction_condition_coefficients$B1_UL + interaction_condition_coefficients$C1_UL + interaction_condition_coefficients$A5B1C1_UL
  intersectional_effects$A6B1C1_UL <- interaction_condition_coefficients$A6_UL + interaction_condition_coefficients$B1_UL + interaction_condition_coefficients$C1_UL + interaction_condition_coefficients$A6B1C1_UL
  
  intersectional_effects$A1B1C2_UL <- interaction_condition_coefficients$A1_UL + interaction_condition_coefficients$B1_UL + interaction_condition_coefficients$C2_UL + interaction_condition_coefficients$A1B1C2_UL
  intersectional_effects$A2B1C2_UL <- interaction_condition_coefficients$A2_UL + interaction_condition_coefficients$B1_UL + interaction_condition_coefficients$C2_UL + interaction_condition_coefficients$A2B1C2_UL
  intersectional_effects$A3B1C2_UL <- interaction_condition_coefficients$A3_UL + interaction_condition_coefficients$B1_UL + interaction_condition_coefficients$C2_UL + interaction_condition_coefficients$A3B1C2_UL  
  intersectional_effects$A4B1C2_UL <- interaction_condition_coefficients$A4_UL + interaction_condition_coefficients$B1_UL + interaction_condition_coefficients$C2_UL + interaction_condition_coefficients$A4B1C2_UL
  intersectional_effects$A5B1C2_UL <- interaction_condition_coefficients$A5_UL + interaction_condition_coefficients$B1_UL + interaction_condition_coefficients$C2_UL + interaction_condition_coefficients$A5B1C2_UL
  intersectional_effects$A6B1C2_UL <- interaction_condition_coefficients$A6_UL + interaction_condition_coefficients$B1_UL + interaction_condition_coefficients$C2_UL + interaction_condition_coefficients$A6B1C2_UL
  
  
  intersectional_ci_values_list[[condition_name]] <- intersectional_effects
}


avg_interaction_ci_values_list <- list()

# Loop over each condition to calculate the average UL and LL for each coefficient
for (condition_name in names(intersectional_ci_values_list)) {
  condition_data <- intersectional_ci_values_list[[condition_name]]
  
  # Calculate the mean of UL and LL for each coefficient
  avg_condition_data <- sapply(condition_data, mean, na.rm = TRUE)
  
  # Add this to the list
  avg_interaction_ci_values_list[[condition_name]] <- avg_condition_data
}

# Convert the list of averages to a data frame
avg_interaction_ci_values <- data.frame(matrix(unlist(avg_interaction_ci_values_list), nrow=length(avg_interaction_ci_values_list), byrow=T))

# Assign the column names based on one of the original data frames inside your list
colnames(avg_interaction_ci_values) <- colnames(intersectional_ci_values_list[[1]])

# Assign the row names to be the condition names
rownames(avg_interaction_ci_values) <- names(intersectional_ci_values_list)


# Maidha


# Initialize an empty list to hold the average CI values for each condition
avg_maidha_ci_values_list <- list()

# Loop over each condition to calculate the average UL and LL for each coefficient
for (condition_name in names( maidha_ci_values_list)) {
  condition_data <-  maidha_ci_values_list[[condition_name]]
  
  # Calculate the mean of UL and LL for each coefficient
  avg_condition_data <- sapply(condition_data, mean, na.rm = TRUE)
  
  # Add this to the list
  avg_maidha_ci_values_list[[condition_name]] <- avg_condition_data
}

# Convert the list of averages to a data frame
avg_maidha_ci_values <- data.frame(matrix(unlist(avg_maidha_ci_values_list), nrow=length(avg_maidha_ci_values_list), byrow=T))

# Assign the column names based on one of the original data frames inside your list
colnames(avg_maidha_ci_values) <- colnames(maidha_ci_values_list[[1]])

# Assign the row names to be the condition names
rownames(avg_maidha_ci_values) <- names(maidha_ci_values_list)



####Average P values##

# Initialize an empty list to hold the average p-values for each condition
avg_categorical_p_values_list <- list()

# Loop over each condition to calculate the average p-values for each coefficient
for (condition_name in names(categorical_p_values_list)) {
  condition_data_p <- categorical_p_values_list[[condition_name]]
  
  # Calculate the mean of p-values for each coefficient
  avg_condition_data_p <- sapply(condition_data_p, mean, na.rm = TRUE)
  
  # Add this to the list
  avg_categorical_p_values_list[[condition_name]] <- avg_condition_data_p
}

# Convert the list of averages to a data frame
avg_categorical_p_values <- data.frame(matrix(unlist(avg_categorical_p_values_list), nrow=length(avg_categorical_p_values_list), byrow=T))

# Assign the column names based on one of the original data frames inside your list
colnames(avg_categorical_p_values) <- colnames(categorical_p_values_list[[1]])

# Assign the row names to be the condition names
rownames(avg_categorical_p_values) <- names(categorical_p_values_list)

# Remove the '(Intercept)' column from avg_categorical_p_values
avg_categorical_p_values <- avg_categorical_p_values[, colnames(avg_categorical_p_values) != "(Intercept)"]





#Interaction
# Remove specified columns from each data frame in the list
interaction_p_values_list_cleaned <- lapply(interaction_p_values_list, function(df) {
  df[, !colnames(df) %in% c("(Intercept)", "A1", "A2", "A3", "A4", "A5", "A6", "B1", "C1", "C2")]
})


# Initialize an empty list to hold the average p-values for each condition
avg_interaction_p_values_list <- list()

# Loop over each condition to calculate the average p-values for each coefficient
for (condition_name in names(interaction_p_values_list_cleaned)) {
  condition_data_p <- interaction_p_values_list_cleaned[[condition_name]]
  
  # Calculate the mean of p-values for each coefficient
  avg_condition_data_p <- sapply(condition_data_p, mean, na.rm = TRUE)
  
  # Add this to the list
  avg_interaction_p_values_list[[condition_name]] <- avg_condition_data_p
}

# Convert the list of averages to a data frame
avg_interaction_p_values <- data.frame(matrix(unlist(avg_interaction_p_values_list), nrow=length(avg_interaction_p_values_list), byrow=T))

# Assign the column names based on one of the original data frames inside your list
colnames(avg_interaction_p_values) <- colnames(interaction_p_values_list_cleaned[[1]])

# Assign the row names to be the condition names
rownames(avg_interaction_p_values) <- names(interaction_p_values_list_cleaned)

# Remove the '(Intercept)' column from avg_categorical_p_values
avg_interaction_p_values <- avg_interaction_p_values[, colnames(avg_interaction_p_values) != "(Intercept)"]


# Maidha 
# Initialize an empty dataframe to hold the results
interval_means_maidha <- data.frame()

# Loop through each condition
for (condition_name in names(hyp_test_interval)) {
  condition_data <- hyp_test_interval[[condition_name]]
  
  # Extract the lower bounds and upper bounds separately
  lower_bounds <- condition_data[, seq(1, 84, by = 2)]
  upper_bounds <- condition_data[, seq(2, 84, by = 2)]
  
  # Calculate the mean of each interval for each replication
  interval_means <- (lower_bounds + upper_bounds) / 2
  
  # Calculate the average of these interval means across all replications
  avg_interval_means <- colMeans(interval_means, na.rm = TRUE)
  
  # Create a dataframe with the condition name and calculated averages
  condition_result <- data.frame(condition = condition_name, t(avg_interval_means))
  
  # Append this to the overall result dataframe
  interval_means_maidha <- rbind(interval_means_maidha, condition_result)
}

# Making the first column as rownames
rownames(interval_means_maidha) <- interval_means_maidha$condition
interval_means_maidha$condition <- NULL

# Replace the column names
colnames(interval_means_maidha) <- colnames(hyp_test_maidha[[1]])


#######Calculate SEb######### 

seb_categorical <- list()

# Create an empty data frame to store the SEB values for the categorical model
seb_categorical_df <- data.frame(matrix(ncol = ncol(categorical_avg_coefficients), nrow = 0))
colnames(seb_categorical_df) <- colnames(categorical_avg_coefficients)

# Iterate through the conditions for the categorical model
for (condition_name in rownames(categorical_avg_coefficients)) {
  # Retrieve the coefficients for this condition
  categorical_condition_coefficients <- categorical_results[[condition_name]]
  avg_categorical_coefficients <- as.numeric(categorical_avg_coefficients[condition_name,]) # Convert to a numeric vector
  
  
  # Compute SEB for each coefficient
  seb <- sapply(seq_along(avg_categorical_coefficients), function(i) {
    coef <- categorical_condition_coefficients[,i]
    sqrt(sum((coef - avg_categorical_coefficients[i])^2) / length(coef))
  })
  
  # Append the SEB values to the data frame
  
  seb_categorical_df <- rbind(seb_categorical_df, setNames(as.list(seb), colnames(categorical_avg_coefficients)))
  rownames(seb_categorical_df)[nrow(seb_categorical_df)] <- condition_name
}




# Print the final data frame
seb_interaction <- list()

# Iterate through the conditions for interaction model
for (condition_name in names(interaction_results)) {
  # Retrieve the coefficients for this condition
  interaction_condition_coefficients <- interaction_results[[condition_name]]
  avg_interaction_coefficients <- interaction_avg_coefficients[condition_name,]
  
  # Create intersectional effect coefficients
  intersectional_effects <- interaction_condition_coefficients
  intersectional_effects$A1B1C1 <- interaction_condition_coefficients$A1 + interaction_condition_coefficients$B1 + interaction_condition_coefficients$C1 + interaction_condition_coefficients$A1B1C1 
  intersectional_effects$A2B1C1 <- interaction_condition_coefficients$A2 + interaction_condition_coefficients$B1 + interaction_condition_coefficients$C1 + interaction_condition_coefficients$A2B1C1
  intersectional_effects$A3B1C1 <- interaction_condition_coefficients$A3 + interaction_condition_coefficients$B1 + interaction_condition_coefficients$C1 + interaction_condition_coefficients$A3B1C1  
  intersectional_effects$A4B1C1 <- interaction_condition_coefficients$A4 + interaction_condition_coefficients$B1 + interaction_condition_coefficients$C1 + interaction_condition_coefficients$A4B1C1
  intersectional_effects$A5B1C1 <- interaction_condition_coefficients$A5 + interaction_condition_coefficients$B1 + interaction_condition_coefficients$C1 + interaction_condition_coefficients$A5B1C1
  intersectional_effects$A6B1C1 <- interaction_condition_coefficients$A6 + interaction_condition_coefficients$B1 + interaction_condition_coefficients$C1 + interaction_condition_coefficients$A6B1C1
  
  intersectional_effects$A1B1C2 <- interaction_condition_coefficients$A1 + interaction_condition_coefficients$B1 + interaction_condition_coefficients$C2 + interaction_condition_coefficients$A1B1C2 
  intersectional_effects$A2B1C2 <- interaction_condition_coefficients$A2 + interaction_condition_coefficients$B1 + interaction_condition_coefficients$C2 + interaction_condition_coefficients$A2B1C2
  intersectional_effects$A3B1C2 <- interaction_condition_coefficients$A3 + interaction_condition_coefficients$B1 + interaction_condition_coefficients$C2 + interaction_condition_coefficients$A3B1C2  
  intersectional_effects$A4B1C2 <- interaction_condition_coefficients$A4 + interaction_condition_coefficients$B1 + interaction_condition_coefficients$C2 + interaction_condition_coefficients$A4B1C2
  intersectional_effects$A5B1C2 <- interaction_condition_coefficients$A5 + interaction_condition_coefficients$B1 + interaction_condition_coefficients$C2 + interaction_condition_coefficients$A5B1C2
  intersectional_effects$A6B1C2 <- interaction_condition_coefficients$A6 + interaction_condition_coefficients$B1 + interaction_condition_coefficients$C2 + interaction_condition_coefficients$A6B1C2
  
  # Keep only intersectional effects
  intersectional_effects <- intersectional_effects[, !(colnames(intersectional_effects) %in% c('(Intercept)', 'A1', 'A2', 'A3', 'A4', 'A5', 'A6', 'B1', 'C1', 'C2'))]
  
  # Compute SEB for each intersectional effect
  seb <- apply(intersectional_effects, 2, function(coef) {
    sqrt(sum((coef - avg_interaction_coefficients)^2) / length(coef))
  })
  
  # Store the SEB values
  seb_interaction[[condition_name]] <- seb
}


seb_interaction_df <- do.call(rbind, lapply(seb_interaction, function(x) {
  as.data.frame(t(as.matrix(x)))
}))
seb_interaction_df <- as.data.frame(seb_interaction_df)



#Maidha
# Initialize a list to store the SEB values for each condition in the MAIDHA model
seb_maidha <- list()

# Iterate through the conditions for MAIDHA model
for (condition_name in names(maidha_intercepts_list)) {
  # Retrieve the intercepts for this condition
  maidha_condition_intercepts <- maidha_intercepts_list[[condition_name]]
  
  # Retrieve the average intercepts from the previously computed maidha_avg_intercepts
  avg_maidha_intercepts <- maidha_avg_intercepts[condition_name,]
  
  # Compute SEB for each cluster intercept
  seb <- apply(maidha_condition_intercepts, 2, function(intercept) {
    sqrt(sum((intercept - avg_maidha_intercepts)^2) / length(intercept))
  })
  
  # Store the SEB values in the list
  seb_maidha[[condition_name]] <- seb
}

# Convert the SEB list to a data frame
seb_maidha_df <- do.call(rbind, seb_maidha)

# Ensure the columns are named appropriately
colnames(seb_maidha_df) <- colnames(maidha_intercepts_list[[1]])

seb_maidha_df <- as.data.frame(seb_maidha_df)



###Bias######

#categorical

true_values_categorical <- c(bA1B0C0, bA2B0C0, bA3B0C0, bA4B0C0, bA5B0C0, bA6B0C0, bA0B1C0, bA1B1C0,
                             bA2B1C0, bA3B1C0, bA4B1C0, bA5B1C0, bA6B1C0, 
                             bA1B0C1, bA2B0C1, bA3B0C1, bA4B0C1, bA5B0C1, bA6B0C1, bA0B1C1, bA1B1C1,
                             bA2B1C1, bA3B1C1, bA4B1C1, bA5B1C1, bA6B1C1,
                             bA1B0C2, bA2B0C2, bA3B0C2, bA4B0C2, bA5B0C2, bA6B0C2, bA0B1C2, bA1B1C2,
                             bA2B1C2, bA3B1C2, bA4B1C2, bA5B1C2, bA6B1C2, bA0B0C1, bA0B0C2
)

categorical_avg_coefficients <- categorical_avg_coefficients[, !(colnames(categorical_avg_coefficients) %in% c('(Intercept)'))]


bias_categorical <- sweep(categorical_avg_coefficients, 2, unlist(true_values_categorical), "-")

#interaction

true_values_interaction <- c(bA1B1C1, bA2B1C1, bA3B1C1, bA4B1C1, bA5B1C1, bA6B1C1, 
                             bA1B1C2, bA2B1C2, bA3B1C2, bA4B1C2, bA5B1C2, bA6B1C2) 


columns_to_remove <- c('(Intercept)', 'A1', 'A2', 'A3', 'A4', 'A5', 'A6', 'B1', 'C1', 'C2')
interaction_int_effects <- interaction_int_effects[, !(colnames(interaction_int_effects) %in% columns_to_remove)]

# Calculate bias for intersectional effects
bias_interaction <- sweep(interaction_int_effects, 2, unlist(true_values_interaction), "-")


#Maidha


true_values_maidha<- c(mA0B0C0, mA1B0C0, mA2B0C0, mA3B0C0, mA4B0C0, mA5B0C0, mA6B0C0, mA1B1C0,
                       mA2B1C0, mA3B1C0, mA4B1C0, mA5B1C0, mA6B1C0, mA0B0C1, mA1B0C1, mA2B0C1,
                       mA3B0C1, mA4B0C1, mA5B0C1, mA6B0C1, mA0B1C0, mA0B1C1, mA1B1C1, mA2B1C1,
                       mA3B1C1, mA4B1C1, mA5B1C1, mA6B1C1, mA0B0C2, mA1B0C2, mA2B0C2, mA3B0C2,
                       mA4B0C2, mA5B0C2, mA6B0C2, mA0B1C2, mA1B1C2, mA2B1C2, mA3B1C2, mA4B1C2,
                       mA5B1C2, mA6B1C2)

bias_maidha<- sweep(maidha_avg_intercepts, 2, unlist(true_values_maidha), "-")




####Accuracy##############
# Categorical
accuracy_categorical <- bias_categorical^2 + seb_categorical_df^2

# Interaction
accuracy_interaction <- bias_interaction^2 + seb_interaction_df^2

# maidha <-

accuracy_maidha<- bias_maidha^2 + seb_maidha_df^2




###### Coverage ########

coverage_results_categorical <- list()
overall_coverage_percentages_categorical<-list()

# Iterate through the conditions
for (condition_name in names(categorical_ci_values_list)) {
  # Extract the CI values for this condition
  condition_ci_values <- categorical_ci_values_list[[condition_name]]
  
  # Check if the true value for each coefficient falls into the observed interval
  condition_coverage <- data.frame(
    A1B0C0 = ifelse(bA1B0C0 > condition_ci_values$A1B0C0_LL & bA1B0C0 < condition_ci_values$A1B0C0_UL, 1, 0),
    A2B0C0 = ifelse(bA2B0C0 > condition_ci_values$A2B0C0_LL & bA2B0C0 < condition_ci_values$A2B0C0_UL, 1, 0),
    A3B0C0 = ifelse(bA3B0C0 > condition_ci_values$A3B0C0_LL & bA3B0C0 < condition_ci_values$A3B0C0_UL, 1, 0),
    A4B0C0 = ifelse(bA4B0C0 > condition_ci_values$A4B0C0_LL & bA4B0C0 < condition_ci_values$A4B0C0_UL, 1, 0),
    A5B0C0 = ifelse(bA5B0C0 > condition_ci_values$A5B0C0_LL & bA5B0C0 < condition_ci_values$A5B0C0_UL, 1, 0),
    A6B0C0 = ifelse(bA6B0C0 > condition_ci_values$A6B0C0_LL & bA6B0C0 < condition_ci_values$A6B0C0_UL, 1, 0),
    A1B1C0 = ifelse(bA1B1C0 > condition_ci_values$A1B1C0_LL & bA1B1C0 < condition_ci_values$A1B1C0_UL, 1, 0),
    A2B1C0 = ifelse(bA2B1C0 > condition_ci_values$A2B1C0_LL & bA2B1C0 < condition_ci_values$A2B1C0_UL, 1, 0),
    A3B1C0 = ifelse(bA3B1C0 > condition_ci_values$A3B1C0_LL & bA3B1C0 < condition_ci_values$A3B1C0_UL, 1, 0),
    A4B1C0 = ifelse(bA4B1C0 > condition_ci_values$A4B1C0_LL & bA4B1C0 < condition_ci_values$A4B1C0_UL, 1, 0),
    A5B1C0 = ifelse(bA5B1C0 > condition_ci_values$A5B1C0_LL & bA5B1C0 < condition_ci_values$A5B1C0_UL, 1, 0),
    A6B1C0 = ifelse(bA6B1C0 > condition_ci_values$A6B1C0_LL & bA6B1C0 < condition_ci_values$A6B1C0_UL, 1, 0),
    
    
    A1B0C1 = ifelse(bA1B0C1 > condition_ci_values$A1B0C1_LL & bA1B0C1 < condition_ci_values$A1B0C1_UL, 1, 0),
    A2B0C1 = ifelse(bA2B0C1 > condition_ci_values$A2B0C1_LL & bA2B0C1 < condition_ci_values$A2B0C1_UL, 1, 0),
    A3B0C1 = ifelse(bA3B0C1 > condition_ci_values$A3B0C1_LL & bA3B0C1 < condition_ci_values$A3B0C1_UL, 1, 0),
    A4B0C1 = ifelse(bA4B0C1 > condition_ci_values$A4B0C1_LL & bA4B0C1 < condition_ci_values$A4B0C1_UL, 1, 0),
    A5B0C1 = ifelse(bA5B0C1 > condition_ci_values$A5B0C1_LL & bA5B0C1 < condition_ci_values$A5B0C1_UL, 1, 0),
    A6B0C1 = ifelse(bA6B0C1 > condition_ci_values$A6B0C1_LL & bA6B0C1 < condition_ci_values$A6B0C1_UL, 1, 0),
    A1B1C1 = ifelse(bA1B1C1 > condition_ci_values$A1B1C0_LL & bA1B1C1 < condition_ci_values$A1B1C1_UL, 1, 0),
    A2B1C1 = ifelse(bA2B1C1 > condition_ci_values$A2B1C0_LL & bA2B1C1 < condition_ci_values$A2B1C1_UL, 1, 0),
    A3B1C1 = ifelse(bA3B1C1 > condition_ci_values$A3B1C0_LL & bA3B1C1 < condition_ci_values$A3B1C1_UL, 1, 0),
    A4B1C1 = ifelse(bA4B1C1 > condition_ci_values$A4B1C0_LL & bA4B1C1 < condition_ci_values$A4B1C1_UL, 1, 0),
    A5B1C1 = ifelse(bA5B1C1 > condition_ci_values$A5B1C0_LL & bA5B1C1 < condition_ci_values$A5B1C1_UL, 1, 0),
    A6B1C1 = ifelse(bA6B1C1 > condition_ci_values$A6B1C0_LL & bA6B1C1 < condition_ci_values$A6B1C1_UL, 1, 0),
    A0B0C1 = ifelse(bA0B0C1 > condition_ci_values$A0B0C1_LL & bA0B0C1 < condition_ci_values$A0B0C1_UL, 1, 0),
    
    
    A1B0C2 = ifelse(bA1B0C2 > condition_ci_values$A1B0C2_LL & bA1B0C2 < condition_ci_values$A1B0C2_UL, 1, 0),
    A2B0C2 = ifelse(bA2B0C2 > condition_ci_values$A2B0C0_LL & bA2B0C2 < condition_ci_values$A2B0C2_UL, 1, 0),
    A3B0C2 = ifelse(bA3B0C2 > condition_ci_values$A3B0C2_LL & bA3B0C2 < condition_ci_values$A3B0C2_UL, 1, 0),
    A4B0C2 = ifelse(bA4B0C2 > condition_ci_values$A4B0C2_LL & bA4B0C0 < condition_ci_values$A4B0C2_UL, 1, 0),
    A5B0C2 = ifelse(bA5B0C2 > condition_ci_values$A5B0C2_LL & bA5B0C0 < condition_ci_values$A5B0C2_UL, 1, 0),
    A6B0C2 = ifelse(bA6B0C2 > condition_ci_values$A6B0C2_LL & bA6B0C0 < condition_ci_values$A6B0C2_UL, 1, 0),
    A1B1C2 = ifelse(bA1B1C2 > condition_ci_values$A1B1C2_LL & bA1B1C0 < condition_ci_values$A1B1C2_UL, 1, 0),
    A2B1C2 = ifelse(bA2B1C2 > condition_ci_values$A2B1C2_LL & bA2B1C0 < condition_ci_values$A2B1C2_UL, 1, 0),
    A3B1C2 = ifelse(bA3B1C2 > condition_ci_values$A3B1C2_LL & bA3B1C0 < condition_ci_values$A3B1C2_UL, 1, 0),
    A4B1C2 = ifelse(bA4B1C2 > condition_ci_values$A4B1C2_LL & bA4B1C0 < condition_ci_values$A4B1C2_UL, 1, 0),
    A5B1C2 = ifelse(bA5B1C2 > condition_ci_values$A5B1C2_LL & bA5B1C0 < condition_ci_values$A5B1C2_UL, 1, 0),
    A6B1C2 = ifelse(bA6B1C2 > condition_ci_values$A6B1C2_LL & bA6B1C0 < condition_ci_values$A6B1C2_UL, 1, 0),
    A0B0C2 = ifelse(bA0B0C2 > condition_ci_values$A0B0C2_LL & bA0B0C2 < condition_ci_values$A0B0C2_UL, 1, 0)
  )
  
  # Store the coverage results data frame for this condition in the overall results list
  coverage_results_categorical[[condition_name]] <- condition_coverage
  
  
  
  # Calculate the overall % coverage for each coefficient
  coverage_percentages <- sapply(condition_coverage, function(col) sum(col) / length(col) * 100)
  
  # Convert to data frame and set row name to the condition name
  coverage_percentages_df <- as.data.frame(t(coverage_percentages))
  rownames(coverage_percentages_df) <- condition_name
  
  # Combine with the overall data frame
  overall_coverage_percentages_categorical <- rbind(overall_coverage_percentages_categorical, coverage_percentages_df)
}





intersectional_ci_values_list <- list()

for (condition_name in names(interaction_ci_values_list)) {
  condition_ci_values <- interaction_ci_values_list[[condition_name]]
  
  # Directly create the data frame with the calculated values
  intersectional_effects_ci <- data.frame(
    A1B1C1_LL = condition_ci_values$A1_LL + condition_ci_values$B1_LL + condition_ci_values$C1_LL + condition_ci_values$A1B1C1_LL,
    A1B1C1_UL = condition_ci_values$A1_UL + condition_ci_values$B1_UL + condition_ci_values$C1_UL + condition_ci_values$A1B1C1_UL,
    A2B1C1_LL = condition_ci_values$A2_LL + condition_ci_values$B1_LL + condition_ci_values$C1_LL + condition_ci_values$A2B1C1_LL,
    A2B1C1_UL = condition_ci_values$A2_UL + condition_ci_values$B1_UL + condition_ci_values$C1_UL + condition_ci_values$A2B1C1_UL,
    A3B1C1_LL = condition_ci_values$A3_LL + condition_ci_values$B1_LL + condition_ci_values$C1_LL + condition_ci_values$A3B1C1_LL,
    A3B1C1_UL = condition_ci_values$A3_UL + condition_ci_values$B1_UL + condition_ci_values$C1_UL + condition_ci_values$A3B1C1_UL,
    A4B1C1_LL = condition_ci_values$A4_LL + condition_ci_values$B1_LL + condition_ci_values$C1_LL + condition_ci_values$A4B1C1_LL,
    A4B1C1_UL = condition_ci_values$A4_UL + condition_ci_values$B1_UL + condition_ci_values$C1_UL + condition_ci_values$A4B1C1_UL,
    A5B1C1_LL = condition_ci_values$A5_LL + condition_ci_values$B1_LL + condition_ci_values$C1_LL + condition_ci_values$A5B1C1_LL,
    A5B1C1_UL = condition_ci_values$A5_UL + condition_ci_values$B1_UL + condition_ci_values$C1_UL + condition_ci_values$A5B1C1_UL,
    A6B1C1_LL = condition_ci_values$A6_LL + condition_ci_values$B1_LL + condition_ci_values$C1_LL + condition_ci_values$A6B1C1_LL,
    A6B1C1_UL = condition_ci_values$A6_UL + condition_ci_values$B1_UL + condition_ci_values$C1_UL + condition_ci_values$A6B1C1_UL,
    
    A1B1C2_LL = condition_ci_values$A1_LL + condition_ci_values$B1_LL + condition_ci_values$C2_LL + condition_ci_values$A1B1C2_LL,
    A1B1C2_UL = condition_ci_values$A1_UL + condition_ci_values$B1_UL + condition_ci_values$C2_UL + condition_ci_values$A1B1C2_UL,
    A2B1C2_LL = condition_ci_values$A2_LL + condition_ci_values$B1_LL + condition_ci_values$C2_LL + condition_ci_values$A2B1C2_LL,
    A2B1C2_UL = condition_ci_values$A2_UL + condition_ci_values$B1_UL + condition_ci_values$C2_UL + condition_ci_values$A2B1C2_UL,
    A3B1C2_LL = condition_ci_values$A3_LL + condition_ci_values$B1_LL + condition_ci_values$C2_LL + condition_ci_values$A3B1C2_LL,
    A3B1C2_UL = condition_ci_values$A3_UL + condition_ci_values$B1_UL + condition_ci_values$C2_UL + condition_ci_values$A3B1C2_UL,
    A4B1C2_LL = condition_ci_values$A4_LL + condition_ci_values$B1_LL + condition_ci_values$C2_LL + condition_ci_values$A4B1C2_LL,
    A4B1C2_UL = condition_ci_values$A4_UL + condition_ci_values$B1_UL + condition_ci_values$C2_UL + condition_ci_values$A4B1C2_UL,
    A5B1C2_LL = condition_ci_values$A5_LL + condition_ci_values$B1_LL + condition_ci_values$C2_LL + condition_ci_values$A5B1C2_LL,
    A5B1C2_UL = condition_ci_values$A5_UL + condition_ci_values$B1_UL + condition_ci_values$C2_UL + condition_ci_values$A5B1C2_UL,
    A6B1C2_LL = condition_ci_values$A6_LL + condition_ci_values$B1_LL + condition_ci_values$C2_LL + condition_ci_values$A6B1C2_LL,
    A6B1C2_UL = condition_ci_values$A6_UL + condition_ci_values$B1_UL + condition_ci_values$C2_UL + condition_ci_values$A6B1C2_UL
  )
  
  intersectional_ci_values_list[[condition_name]] <- intersectional_effects_ci
}


# Initializing the list to store coverage tables for each condition
coverage_results_interaction <- list()
overall_coverage_percentages_interaction<-list()

# Iterate through the conditions
for (condition_name in names(intersectional_ci_values_list)) {
  # Extract the CI values for this condition
  condition_ci_values <- intersectional_ci_values_list[[condition_name]]
  
  # Initializing the data frame to store the coverage results for this condition
  condition_coverage <- data.frame()
  
  # Check if the true value for each coefficient falls into the observed interval
  
  condition_coverage <- data.frame(
    A1B1C1 = ifelse(bA1B1C1 > condition_ci_values$A1B1C1_LL & bA1B1C1 < condition_ci_values$A1B1C1_UL, 1, 0),
    A2B1C1 = ifelse(bA2B1C1 > condition_ci_values$A2B1C1_LL & bA2B1C1 < condition_ci_values$A2B1C1_UL, 1, 0),
    A3B1C1 = ifelse(bA3B1C1 > condition_ci_values$A3B1C1_LL & bA3B1C1 < condition_ci_values$A3B1C1_UL, 1, 0),
    A4B1C1 = ifelse(bA4B1C1 > condition_ci_values$A4B1C1_LL & bA4B1C1 < condition_ci_values$A4B1C1_UL, 1, 0),
    A5B1C1 = ifelse(bA5B1C1 > condition_ci_values$A5B1C1_LL & bA5B1C1 < condition_ci_values$A5B1C1_UL, 1, 0),
    A6B1C1 = ifelse(bA6B1C1 > condition_ci_values$A6B1C1_LL & bA6B1C1 < condition_ci_values$A6B1C1_UL, 1, 0),
    
    A1B1C2 = ifelse(bA1B1C2 > condition_ci_values$A1B1C2_LL & bA1B1C2 < condition_ci_values$A1B1C2_UL, 1, 0),
    A2B1C2 = ifelse(bA2B1C2 > condition_ci_values$A2B1C2_LL & bA2B1C2 < condition_ci_values$A2B1C2_UL, 1, 0),
    A3B1C2 = ifelse(bA3B1C2 > condition_ci_values$A3B1C2_LL & bA3B1C2 < condition_ci_values$A3B1C2_UL, 1, 0),
    A4B1C2 = ifelse(bA4B1C2 > condition_ci_values$A4B1C2_LL & bA4B1C2 < condition_ci_values$A4B1C2_UL, 1, 0),
    A5B1C2 = ifelse(bA5B1C2 > condition_ci_values$A5B1C2_LL & bA5B1C2 < condition_ci_values$A5B1C2_UL, 1, 0),
    A6B1C2 = ifelse(bA6B1C2 > condition_ci_values$A6B1C2_LL & bA6B1C2 < condition_ci_values$A6B1C2_UL, 1, 0)
  )
  
  
  # Store the coverage results data frame for this condition in the overall results list
  coverage_results_interaction[[condition_name]] <- condition_coverage
  
  # Calculate the overall % coverage for each coefficient
  coverage_percentages <- sapply(condition_coverage, function(col) sum(col) / length(col) * 100)
  
  # Convert to data frame and set row name to the condition name
  coverage_percentages_df <- as.data.frame(t(coverage_percentages))
  rownames(coverage_percentages_df) <- condition_name
  
  # Combine with the overall data frame
  overall_coverage_percentages_interaction <- rbind(overall_coverage_percentages_interaction, coverage_percentages_df)
}



#Maidha


# Initialize an empty data frame to store overall coverage percentages for all conditions and coefficients
overall_coverage_percentages_maidha <- data.frame()
coverage_results_maidha<-list()
# Iterate through the conditions
for (condition_name in names(maidha_ci_values_list)) {
  
  # Extract the CI values for this condition
  condition_ci_values <- maidha_ci_values_list[[condition_name]]
  
  # Initialize the data frame to store the coverage results for this condition
  condition_coverage <- data.frame(
    # Add the coefficients for the categorical model
    A0B0C0 = ifelse(mA0B0C0 > condition_ci_values$A0B0C0_LL & mA1B0C0 < condition_ci_values$A0B0C0_UL, 1, 0),
    A1B0C0 = ifelse(mA1B0C0 > condition_ci_values$A1B0C0_LL & mA1B0C0 < condition_ci_values$A1B0C0_UL, 1, 0),
    A2B0C0 = ifelse(mA2B0C0 > condition_ci_values$A2B0C0_LL & mA2B0C0 < condition_ci_values$A2B0C0_UL, 1, 0),
    A3B0C0 = ifelse(mA3B0C0 > condition_ci_values$A3B0C0_LL & mA3B0C0 < condition_ci_values$A3B0C0_UL, 1, 0),
    A4B0C0 = ifelse(mA4B0C0 > condition_ci_values$A4B0C0_LL & mA4B0C0 < condition_ci_values$A4B0C0_UL, 1, 0),
    A5B0C0 = ifelse(mA5B0C0 > condition_ci_values$A5B0C0_LL & mA5B0C0 < condition_ci_values$A5B0C0_UL, 1, 0),
    A6B0C0 = ifelse(mA6B0C0 > condition_ci_values$A6B0C0_LL & mA6B0C0 < condition_ci_values$A6B0C0_UL, 1, 0),
    A1B1C0 = ifelse(mA1B1C0 > condition_ci_values$A1B1C0_LL & mA1B1C0 < condition_ci_values$A1B1C0_UL, 1, 0),
    A2B1C0 = ifelse(mA2B1C0 > condition_ci_values$A2B1C0_LL & mA2B1C0 < condition_ci_values$A2B1C0_UL, 1, 0),
    A3B1C0 = ifelse(mA3B1C0 > condition_ci_values$A3B1C0_LL & mA3B1C0 < condition_ci_values$A3B1C0_UL, 1, 0),
    A4B1C0 = ifelse(mA4B1C0 > condition_ci_values$A4B1C0_LL & mA4B1C0 < condition_ci_values$A4B1C0_UL, 1, 0),
    A5B1C0 = ifelse(mA5B1C0 > condition_ci_values$A5B1C0_LL & mA5B1C0 < condition_ci_values$A5B1C0_UL, 1, 0),
    A6B1C0 = ifelse(mA6B1C0 > condition_ci_values$A6B1C0_LL & mA6B1C0 < condition_ci_values$A6B1C0_UL, 1, 0),
    
    
    A1B0C1 = ifelse(mA1B0C1 > condition_ci_values$A1B0C1_LL & mA1B0C1 < condition_ci_values$A1B0C1_UL, 1, 0),
    A2B0C1 = ifelse(mA2B0C1 > condition_ci_values$A2B0C1_LL & mA2B0C1 < condition_ci_values$A2B0C1_UL, 1, 0),
    A3B0C1 = ifelse(mA3B0C1 > condition_ci_values$A3B0C1_LL & mA3B0C1 < condition_ci_values$A3B0C1_UL, 1, 0),
    A4B0C1 = ifelse(mA4B0C1 > condition_ci_values$A4B0C1_LL & mA4B0C1 < condition_ci_values$A4B0C1_UL, 1, 0),
    A5B0C1 = ifelse(mA5B0C1 > condition_ci_values$A5B0C1_LL & mA5B0C1 < condition_ci_values$A5B0C1_UL, 1, 0),
    A6B0C1 = ifelse(mA6B0C1 > condition_ci_values$A6B0C1_LL & mA6B0C1 < condition_ci_values$A6B0C1_UL, 1, 0),
    A1B1C1 = ifelse(mA1B1C1 > condition_ci_values$A1B1C0_LL & mA1B1C1 < condition_ci_values$A1B1C1_UL, 1, 0),
    A2B1C1 = ifelse(mA2B1C1 > condition_ci_values$A2B1C0_LL & mA2B1C1 < condition_ci_values$A2B1C1_UL, 1, 0),
    A3B1C1 = ifelse(mA3B1C1 > condition_ci_values$A3B1C0_LL & mA3B1C1 < condition_ci_values$A3B1C1_UL, 1, 0),
    A4B1C1 = ifelse(mA4B1C1 > condition_ci_values$A4B1C0_LL & mA4B1C1 < condition_ci_values$A4B1C1_UL, 1, 0),
    A5B1C1 = ifelse(mA5B1C1 > condition_ci_values$A5B1C0_LL & mA5B1C1 < condition_ci_values$A5B1C1_UL, 1, 0),
    A6B1C1 = ifelse(mA6B1C1 > condition_ci_values$A6B1C0_LL & mA6B1C1 < condition_ci_values$A6B1C1_UL, 1, 0),
    A0B0C1 = ifelse(mA0B0C1 > condition_ci_values$A0B0C1_LL & mA0B0C1 < condition_ci_values$A0B0C1_UL, 1, 0),
    
    
    A1B0C2 = ifelse(mA1B0C2 > condition_ci_values$A1B0C2_LL & mA1B0C2 < condition_ci_values$A1B0C2_UL, 1, 0),
    A2B0C2 = ifelse(mA2B0C2 > condition_ci_values$A2B0C0_LL & mA2B0C2 < condition_ci_values$A2B0C2_UL, 1, 0),
    A3B0C2 = ifelse(mA3B0C2 > condition_ci_values$A3B0C2_LL & mA3B0C2 < condition_ci_values$A3B0C2_UL, 1, 0),
    A4B0C2 = ifelse(mA4B0C2 > condition_ci_values$A4B0C2_LL & mA4B0C0 < condition_ci_values$A4B0C2_UL, 1, 0),
    A5B0C2 = ifelse(mA5B0C2 > condition_ci_values$A5B0C2_LL & mA5B0C0 < condition_ci_values$A5B0C2_UL, 1, 0),
    A6B0C2 = ifelse(mA6B0C2 > condition_ci_values$A6B0C2_LL & mA6B0C0 < condition_ci_values$A6B0C2_UL, 1, 0),
    A1B1C2 = ifelse(mA1B1C2 > condition_ci_values$A1B1C2_LL & mA1B1C0 < condition_ci_values$A1B1C2_UL, 1, 0),
    A2B1C2 = ifelse(mA2B1C2 > condition_ci_values$A2B1C2_LL & mA2B1C0 < condition_ci_values$A2B1C2_UL, 1, 0),
    A3B1C2 = ifelse(mA3B1C2 > condition_ci_values$A3B1C2_LL & mA3B1C0 < condition_ci_values$A3B1C2_UL, 1, 0),
    A4B1C2 = ifelse(mA4B1C2 > condition_ci_values$A4B1C2_LL & mA4B1C0 < condition_ci_values$A4B1C2_UL, 1, 0),
    A5B1C2 = ifelse(mA5B1C2 > condition_ci_values$A5B1C2_LL & mA5B1C0 < condition_ci_values$A5B1C2_UL, 1, 0),
    A6B1C2 = ifelse(mA6B1C2 > condition_ci_values$A6B1C2_LL & mA6B1C0 < condition_ci_values$A6B1C2_UL, 1, 0),
    A0B0C2 = ifelse(mA0B0C2 > condition_ci_values$A0B0C2_LL & mA0B0C2 < condition_ci_values$A0B0C2_UL, 1, 0)
  )
  
  # Store the coverage results data frame for this condition in the overall results list
  coverage_results_maidha[[condition_name]] <- condition_coverage
  
  # Calculate the overall % coverage for each coefficient
  coverage_percentages <- sapply(condition_coverage, function(col) sum(col) / length(col) * 100)
  
  # Convert to data frame and set row name to the condition name
  coverage_percentages_df <- as.data.frame(t(coverage_percentages))
  rownames(coverage_percentages_df) <- condition_name
  
  # Combine with the overall data frame
  overall_coverage_percentages_maidha <- rbind(overall_coverage_percentages_maidha, coverage_percentages_df)
}



###Power & Type 1 Error##### 

#Categorical

# Initialize an empty data frame to store the final percentages
overall_percentages_df <- data.frame()

# Extract condition names
condition_names <- names(categorical_p_values_list)

# Loop through each condition
for (condition_name in condition_names) {
  
  # Get the data frame corresponding to the current condition
  condition_df <- categorical_p_values_list[[condition_name]]
  
  # Calculate the percentage of p-values less than 0.05 for each coefficient
  percentage_vector <- colMeans(condition_df < 0.05, na.rm = TRUE) * 100
  
  # Append this to the overall data frame
  overall_percentages_df <- rbind(overall_percentages_df, as.data.frame(t(percentage_vector)))
}

# Assign the row names and column names to the overall data frame
rownames(overall_percentages_df) <- condition_names
colnames(overall_percentages_df) <- colnames(categorical_p_values_list[[1]])

p_value_percent_categorical<-overall_percentages_df





# Interaction



# Initialize another empty data frame to store the final percentages for interactions
interaction_overall_percentages_df <- data.frame()

# Remove unwanted columns from all dataframes in the list
interaction_p_values_list_cleaned <- lapply(interaction_p_values_list, function(df) {
  df[, !colnames(df) %in% c("A1","A2","A3","A4","A5","A6","B1", "C1", "C2")]
})

# Extract condition names for interactions
interaction_condition_names <- names(interaction_p_values_list_cleaned)

# Loop through each condition for interactions
for (interaction_condition_name in interaction_condition_names) {
  
  # Get the data frame corresponding to the current interaction condition
  interaction_condition_df <- interaction_p_values_list_cleaned[[interaction_condition_name]]
  
  # Calculate the percentage of p-values less than 0.05 for each coefficient
  interaction_percentage_vector <- colMeans(interaction_condition_df < 0.05, na.rm = TRUE) * 100
  
  # Append this to the overall data frame for interactions
  interaction_overall_percentages_df <- rbind(interaction_overall_percentages_df, as.data.frame(t(interaction_percentage_vector)))
}

# Assign the row names and column names to the overall data frame for interactions
rownames(interaction_overall_percentages_df) <- interaction_condition_names
colnames(interaction_overall_percentages_df) <- colnames(interaction_p_values_list_cleaned[[1]])
p_value_percent_interaction <- interaction_overall_percentages_df[, !(colnames(interaction_overall_percentages_df) == "(Intercept)")]




# Maidha



# Initialize an empty dataframe to hold the results
sig_percent_maidha <- data.frame()

# Loop through each condition
for (condition_name in names(hyp_test_maidha)) {
  condition_data <- hyp_test_maidha[[condition_name]]
  
  # Calculate the percentage of times each coefficient has a "1"
  percentage_ones <- colMeans(condition_data, na.rm = TRUE) * 100  # Assuming that NAs should be removed
  
  # Create a dataframe with the condition name and calculated percentages
  condition_result <- data.frame(condition = condition_name, t(percentage_ones))
  
  # Append this to the overall result dataframe
  sig_percent_maidha <- rbind(sig_percent_maidha, condition_result)
}

# Making the first column as rownames
rownames(sig_percent_maidha) <- sig_percent_maidha$condition
sig_percent_maidha$condition <- NULL




###############################################################################






####Bias#####

#Categorical

# Initialize flagged_bias_categorical with the same dimensions as bias_categorical
flagged_extremebias_categorical <- as.data.frame(matrix(0, nrow=nrow(bias_categorical), ncol=ncol(bias_categorical)))
colnames(flagged_extremebias_categorical) <- colnames(bias_categorical)
rownames(flagged_extremebias_categorical) <- rownames(bias_categorical)  # Retain the row names

for (i in 1:nrow(bias_categorical)) {
  for (j in 1:ncol(bias_categorical)) {
    if (is.na(bias_categorical[i, j]) || is.na(seb_categorical_df[i, j])) {
      flagged_bias_categorical[i, j] <- NA
    } else {
      if (abs(bias_categorical[i, j]) >= 2 * seb_categorical_df[i, j]) {
        flagged_extremebias_categorical[i, j] <- 1
      }
    }
  }
}

flagged_modbias_categorical <- as.data.frame(matrix(0, nrow=nrow(bias_categorical), ncol=ncol(bias_categorical)))
colnames(flagged_modbias_categorical) <- colnames(bias_categorical)
rownames(flagged_modbias_categorical) <- rownames(bias_categorical)  # Retain the row names

for (i in 1:nrow(bias_categorical)) {
  for (j in 1:ncol(bias_categorical)) {
    if (is.na(bias_categorical[i, j]) || is.na(seb_categorical_df[i, j])) {
      flagged_modbias_categorical[i, j] <- NA
    } else {
      if (abs(bias_categorical[i, j]) >= .5 * seb_categorical_df[i, j] &&
          abs(bias_categorical[i, j]) <2 * seb_categorical_df[i, j]) {
        flagged_modbias_categorical[i, j] <- 1 
      }
    }
  }
}

# Assuming flagged_extremebias_categorical and flagged_modbias_categorical have the same dimensions
flagged_bias_categorical <- flagged_extremebias_categorical + flagged_modbias_categorical

# Replace all values greater than 1 with 1 (since we only care if it's flagged or not)
flagged_bias_categorical[flagged_bias_categorical > 1] <- 1




# Interaction
flagged_extremebias_interaction <- as.data.frame(matrix(0, nrow=nrow(bias_interaction), ncol=ncol(bias_interaction)))
colnames(flagged_extremebias_interaction) <- colnames(bias_interaction)
rownames(flagged_extremebias_interaction) <- rownames(bias_interaction)  


for (i in 1:nrow(bias_interaction)) {
  for (j in 1:ncol(bias_interaction)) {
    if (is.na(bias_interaction[i, j]) || is.na(seb_interaction_df[i, j])) {
      flagged_bias_categorical[i, j] <- NA
    } else {
      if (abs(bias_interaction[i, j]) >= 2 * seb_interaction_df[i, j]) {
        flagged_extremebias_interaction[i, j] <- 1
      }
    }
  }
}



flagged_modbias_interaction <- as.data.frame(matrix(0, nrow=nrow(bias_interaction), ncol=ncol(bias_interaction)))
colnames(flagged_modbias_interaction) <- colnames(bias_interaction)
rownames(flagged_modbias_interaction) <- rownames(bias_interaction)  # Retain the row names

for (i in 1:nrow(bias_interaction)) {
  for (j in 1:ncol(bias_interaction)) {
    if (is.na(bias_interaction[i, j]) || is.na(seb_interaction_df[i, j])) {
      flagged_modbias_interaction[i, j] <- NA
    } else {
      if (abs(bias_interaction[i, j]) >= .5 * seb_interaction_df[i, j] &&
          abs(bias_interaction[i, j]) <2 * seb_interaction_df[i, j]) {
        flagged_modbias_interaction[i, j] <- 1 
      }
    }
  }
}


flagged_bias_interaction <- flagged_extremebias_interaction + flagged_modbias_interaction

flagged_bias_interaction[flagged_bias_interaction > 1] <- 1


#Maidha
flagged_extremebias_maidha <- as.data.frame(matrix(0, nrow=nrow(bias_maidha), ncol=ncol(bias_maidha)))
colnames(flagged_extremebias_maidha) <- colnames(bias_maidha)
rownames(flagged_extremebias_maidha) <- rownames(bias_maidha)  # Retain the row names


for (i in 1:nrow(bias_maidha)) {
  for (j in 1:ncol(bias_maidha)) {
    if (is.na(bias_maidha[i, j]) || is.na(seb_maidha_df[i, j])) {
      flagged_bias_maidha[i, j] <- NA
    } else {
      if (abs(bias_maidha[i, j]) >= 2 * seb_maidha_df[i, j]) {
        flagged_extremebias_maidha[i, j] <- 1
      }
    }
  }
}

flagged_modbias_maidha <- as.data.frame(matrix(0, nrow=nrow(bias_maidha), ncol=ncol(bias_maidha)))
colnames(flagged_modbias_maidha) <- colnames(bias_maidha)
rownames(flagged_modbias_maidha) <- rownames(bias_maidha)  # Retain the row names

for (i in 1:nrow(bias_maidha)) {
  for (j in 1:ncol(bias_maidha)) {
    if (is.na(bias_maidha[i, j]) || is.na(seb_maidha_df[i, j])) {
      flagged_modbias_maidha[i, j] <- NA
    } else {
      if (abs(bias_maidha[i, j]) >= .5 * seb_maidha_df[i, j] &&
          abs(bias_maidha[i, j]) <2 * seb_maidha_df[i, j]) {
        flagged_modbias_maidha[i, j] <- 1 
      }
    }
  }
}


flagged_bias_maidha<- flagged_extremebias_maidha + flagged_modbias_maidha

flagged_bias_maidha[flagged_bias_maidha > 1] <- 1






#####Acccuracy#### 

#Categorical
flagged_accuracy_categorical <- as.data.frame(matrix(0, nrow=nrow(accuracy_categorical), ncol=ncol(accuracy_categorical)))
colnames(flagged_accuracy_categorical) <- colnames(accuracy_categorical)
rownames(flagged_accuracy_categorical) <- rownames(accuracy_categorical)

# Flag cells where the condition is met
for (i in 1:nrow(accuracy_categorical)) {
  for (j in 1:ncol(accuracy_categorical)) {
    if (is.na(accuracy_categorical[i, j]) || is.na(seb_categorical_df[i, j])) {
      flagged_accuracy_categorical[i, j] <- NA
    } else{
      if (accuracy_categorical[i, j] > 0.5) {
        flagged_accuracy_categorical[i, j] <- 1
      }
    }
  }
}

flagged_modbias_categorical <- as.data.frame(matrix(0, nrow=nrow(bias_categorical), ncol=ncol(bias_categorical)))
colnames(flagged_modbias_categorical) <- colnames(bias_categorical)
rownames(flagged_modbias_categorical) <- rownames(bias_categorical)  # Retain the row names

for (i in 1:nrow(bias_categorical)) {
  for (j in 1:ncol(bias_categorical)) {
    if (is.na(bias_categorical[i, j]) || is.na(seb_categorical_df[i, j])) {
      flagged_modbias_categorical[i, j] <- NA
    } else {
      if (abs(bias_categorical[i, j]) >= .5 * seb_categorical_df[i, j] &&
          abs(bias_categorical[i, j]) <2 * seb_categorical_df[i, j]) {
        flagged_modbias_categorical[i, j] <- 1 
      }
    }
  }
}

#Interaction


flagged_accuracy_interaction <- as.data.frame(matrix(0, nrow=nrow(accuracy_interaction), ncol=ncol(accuracy_interaction)))
colnames(flagged_accuracy_interaction) <- colnames(accuracy_interaction)
rownames(flagged_accuracy_interaction) <- rownames(accuracy_interaction)  # Retain the row names

# Flag cells where the condition is met
for (i in 1:nrow(accuracy_interaction)) {
  for (j in 1:ncol(accuracy_interaction)) {
    if (is.na(accuracy_interaction[i, j]) || is.na(seb_interaction_df[i, j])) {
      flagged_accuracy_interaction[i, j] <- NA
    } else{
      if (accuracy_interaction[i, j] > 0.5) {
        flagged_accuracy_interaction[i, j] <- 1
      }
    }
  }
}
#Maidha

flagged_accuracy_maidha <- as.data.frame(matrix(0, nrow=nrow(accuracy_maidha), ncol=ncol(accuracy_maidha)))
colnames(flagged_accuracy_maidha) <- colnames(accuracy_maidha)
rownames(flagged_accuracy_maidha) <- rownames(accuracy_maidha)  # Retain the row names

# Flag cells where the condition is met
for (i in 1:nrow(accuracy_maidha)) {
  for (j in 1:ncol(accuracy_maidha)) {
    if (is.na(accuracy_maidha[i, j]) || is.na(seb_maidha_df[i, j])) {
      flagged_accuracy_maidha[i, j] <- NA
    } else{
      if (accuracy_maidha[i, j] > 0.5) {
        flagged_accuracy_maidha[i, j] <- 1
      }
    }
  }
}

######Power and Type 1 Error ##############
#Categorical

# Initialize flagged_power_error_categorical table
flagged_power_error_categorical <- as.data.frame(matrix(0, nrow = nrow(p_value_percent_categorical), ncol = ncol(p_value_percent_categorical)))
colnames(flagged_power_error_categorical) <- colnames(p_value_percent_categorical)
rownames(flagged_power_error_categorical) <- rownames(p_value_percent_categorical)  # Retain the row names

# Coefficients that should have a true effect
true_effect_cols_categorical <- c("A1B0C0", "A2B0C0", "A3B0C0", "A2B1C0", 
                                  "A2B0C1", "A3B0C1", "A2B1C1", "A6B1C1", 
                                  "A5B0C2", "A1B1C2",  "A3B1C2", "A6B1C2")

# Coefficients that should not have a true effect
no_effect_cols_categorical <- c("A4B0C0", "A5B0C0", "A6B0C0", "A0B1C0", "A1B1C0", 
                                "A3B1C0", "A4B1C0", "A5B1C0", "A6B1C0", "A0B0C1", 
                                "A1B0C1", "A4B0C1", "A5B0C1", "A6B0C1", "A0B1C1", 
                                "A1B1C1", "A3B1C1", "A4B1C1", "A5B1C1", "A0B0C2", 
                                "A1B0C2", "A2B0C2", "A3B0C2", "A4B0C2", "A6B0C2", 
                                "A0B1C2", "A2B1C2", "A4B1C2", "A5B1C2")

# Flag values for insufficient power (< 80%)
for (col in true_effect_cols_categorical) {
  flagged_power_error_categorical[, col] <- ifelse(p_value_percent_categorical[, col] < 80, 1, 0)
}

# Flag values for too much type 1 error (> 5%)
for (col in no_effect_cols_categorical) {
  flagged_power_error_categorical[, col] <- ifelse(p_value_percent_categorical[, col] > 5, 1, 0)
}



#Interaction


# Initialize flagged_power_error_categorical table
flagged_power_error_interaction <- as.data.frame(matrix(0, nrow = nrow(interaction_overall_percentages_df), ncol = ncol(interaction_overall_percentages_df)))
colnames(flagged_power_error_interaction) <- colnames(interaction_overall_percentages_df)
rownames(flagged_power_error_interaction) <- rownames(interaction_overall_percentages_df)  # Retain the row names


# Coefficients that should have a true effect
true_effect_cols_interaction<- c( "A2B1C1", "A6B1C1", 
                                  "A1B1C2",  "A3B1C2", "A6B1C2")

# Coefficients that should not have a true effect
no_effect_cols_interaction <- c( "A1B1C1", "A3B1C1", "A4B1C1", "A5B1C1", 
                                 "A2B1C2", "A4B1C2", "A5B1C2")

# Flag values for insufficient power (< 80%)
for (col in true_effect_cols_interaction) {
  flagged_power_error_interaction[, col] <- ifelse(interaction_overall_percentages_df[, col] < 80, 1, 0)
}

# Flag values for too much type 1 error (> 5%)
for (col in no_effect_cols_interaction) {
  flagged_power_error_interaction[, col] <- ifelse(interaction_overall_percentages_df[, col] > 5, 1, 0)
}



#Maidha

# Initialize flagged_power_error_categorical table
flagged_power_error_maidha<- as.data.frame(matrix(0, nrow = nrow(sig_percent_maidha), ncol = ncol(sig_percent_maidha)))
colnames(flagged_power_error_maidha) <- colnames(sig_percent_maidha)
rownames(flagged_power_error_maidha) <- rownames(sig_percent_maidha)  # Retain the row names

# Coefficients that should have a true effect
true_effect_cols_maidha <- c("A1B0C0", "A2B0C0", "A3B0C0", "A2B1C0", 
                             "A2B0C1", "A3B0C1", "A2B1C1", "A6B1C1", 
                             "A5B0C2", "A1B1C2",  "A3B1C2", "A6B1C2")

# Coefficients that should not have a true effect
no_effect_cols_maidha <- c("A0B0C0","A4B0C0", "A5B0C0", "A6B0C0", "A0B1C0", "A1B1C0", 
                           "A3B1C0", "A4B1C0", "A5B1C0", "A6B1C0", "A0B0C1", 
                           "A1B0C1", "A4B0C1", "A5B0C1", "A6B0C1", "A0B1C1", 
                           "A1B1C1", "A3B1C1", "A4B1C1", "A5B1C1", "A0B0C2", 
                           "A1B0C2", "A2B0C2", "A3B0C2", "A4B0C2", "A6B0C2", 
                           "A0B1C2", "A2B1C2", "A4B1C2", "A5B1C2")


# Flag values for insufficient power (< 80%)
for (col in true_effect_cols_maidha) {
  flagged_power_error_maidha[, col] <- ifelse(sig_percent_maidha[, col] < 80, 1, 0)
}

# Flag values for too much type 1 error (> 5%)
for (col in no_effect_cols_maidha) {
  flagged_power_error_maidha[, col] <- ifelse(sig_percent_maidha[, col] > 5, 1, 0)
}






############Coverage##############

# Categorical 


flagged_coverage_categorical <- data.frame(
  Flag = ifelse(overall_coverage_percentages_categorical < 92.5, 1, 0)
)

# To keep row names
rownames(flagged_coverage_categorical) <- rownames(overall_coverage_percentages_categorical)


flagged_uprcoverage_categorical <- data.frame(
  Flag = ifelse(overall_coverage_percentages_categorical > 97.5, 1, 0)
)

rownames(flagged_uprcoverage_categorical) <- rownames(overall_coverage_percentages_categorical)

flagged_fullcoverage_categorical<- flagged_coverage_categorical + flagged_uprcoverage_categorical

flagged_fullcoverage_categorical[flagged_fullcoverage_categorical > 1] <- 1


#Interaction


flagged_coverage_interaction <- data.frame(
  Flag = ifelse(overall_coverage_percentages_interaction < 92.5, 1, 0)
)

rownames(flagged_coverage_interaction) <- rownames(overall_coverage_percentages_interaction)

flagged_uprcoverage_interaction <- data.frame(
  Flag = ifelse(overall_coverage_percentages_interaction > 97.5, 1, 0)
)

rownames(flagged_uprcoverage_interaction) <- rownames(overall_coverage_percentages_interaction)

flagged_fullcoverage_interaction<- flagged_coverage_interaction + flagged_uprcoverage_interaction

flagged_fullcoverage_interaction[flagged_fullcoverage_interaction > 1] <- 1


#Maidha

flagged_coverage_maidha <- data.frame(
  Flag = ifelse(overall_coverage_percentages_maidha < 92.5, 1, 0)
)

# To keep row names
rownames(flagged_coverage_maidha) <- rownames(overall_coverage_percentages_maidha)


flagged_uprcoverage_maidha<- data.frame(
  Flag = ifelse(overall_coverage_percentages_maidha > 97.5, 1, 0)
)

rownames(flagged_uprcoverage_maidha) <- rownames(overall_coverage_percentages_maidha)

flagged_fullcoverage_maidha<- flagged_coverage_maidha + flagged_uprcoverage_maidha

flagged_fullcoverage_maidha[flagged_fullcoverage_maidha> 1] <- 1



####### Summaries of Flags #######


# Sum across the rows to get the number of flags for each condition in each table

sum_flagged_modbias <- rowSums(flagged_modbias_categorical, na.rm = TRUE)
sum_flagged_extremebias <- rowSums(flagged_extremebias_categorical, na.rm = TRUE)
sum_flagged_bias <- rowSums(flagged_bias_categorical, na.rm = TRUE)
sum_flagged_accuracy <- rowSums(flagged_accuracy_categorical, na.rm = TRUE)
sum_flagged_coverage <- rowSums(flagged_coverage_categorical, na.rm = TRUE)
sum_flagged_uprcoverage <- rowSums(flagged_uprcoverage_categorical, na.rm = TRUE)
sum_flagged_fullcoverage <- rowSums(flagged_fullcoverage_categorical, na.rm = TRUE)
sum_flagged_power <- rowSums(flagged_power_error_categorical[, true_effect_cols_categorical], na.rm = TRUE)
sum_flagged_error <- rowSums(flagged_power_error_categorical[, no_effect_cols_categorical], na.rm = TRUE)

flag_sum_all_categorical <- data.frame(
  ModBias= sum_flagged_modbias,
  ExtremeBias= sum_flagged_extremebias,
  Bias = sum_flagged_bias,
  Accuracy = sum_flagged_accuracy,
  LwrCoverage = sum_flagged_coverage,
  UprCoverage = sum_flagged_uprcoverage,
  FullCoverage = sum_flagged_fullcoverage,
  Power = sum_flagged_power,
  Type1Error = sum_flagged_error
)


flag_sum_categorical <- data.frame(
  Bias = sum_flagged_bias,
  Accuracy = sum_flagged_accuracy,
  Coverage = sum_flagged_coverage,
  Power = sum_flagged_power,
  Type1Error = sum_flagged_error
)



# Count the total number of coefficients for each outcome type
total_coefficients <- ncol(flagged_bias_categorical)
total_power_coefficients <- length(true_effect_cols_categorical)
total_error_coefficients <- length(no_effect_cols_categorical)

# Calculate the percentages
flag_percent_categorical <- data.frame(
  Bias = (flag_sum_categorical$Bias / total_coefficients) * 100,
  Accuracy = (flag_sum_categorical$Accuracy / total_coefficients) * 100,
  Coverage = (flag_sum_categorical$Coverage / total_coefficients) * 100,
  Power = (flag_sum_categorical$Power / total_power_coefficients) * 100,
  Type1Error = (flag_sum_categorical$Type1Error / total_error_coefficients) * 100
)
row.names(flag_percent_categorical) <- row.names(flag_sum_categorical)


# Calculate the percentages
flag_percent_all_categorical <- data.frame(
  ModBias = (flag_sum_all_categorical$ModBias / total_coefficients)* 100,
  ExtremeBias= (flag_sum_all_categorical$ExtremeBias/total_coefficients) * 100 ,
  Bias = (flag_sum_all_categorical$Bias / total_coefficients) *100,
  Accuracy = (flag_sum_all_categorical$Accuracy / total_coefficients) * 100,
  LwrCoverage = (flag_sum_all_categorical$LwrCoverage/ total_coefficients) * 100,
  UprCoverage = (flag_sum_all_categorical$UprCoverage / total_coefficients) * 100,
  FullCoverage = (flag_sum_all_categorical$FullCoverage / total_coefficients) * 100,
  Power = (flag_sum_all_categorical$Power / total_power_coefficients) * 100,
  Type1Error = (flag_sum_all_categorical$Type1Error / total_error_coefficients) * 100
)
row.names(flag_percent_all_categorical) <- row.names(flag_sum_all_categorical)



# Interaction

sum_flagged_bias <- rowSums(flagged_bias_interaction, na.rm = TRUE)
sum_flagged_modbias <- rowSums(flagged_modbias_interaction, na.rm = TRUE)
sum_flagged_extremebias <- rowSums(flagged_extremebias_interaction, na.rm = TRUE)
sum_flagged_accuracy <- rowSums(flagged_accuracy_interaction, na.rm = TRUE)
sum_flagged_coverage <- rowSums(flagged_coverage_interaction, na.rm = TRUE)
sum_flagged_power <- rowSums(flagged_power_error_interaction[, true_effect_cols_interaction], na.rm = TRUE)
sum_flagged_error <- rowSums(flagged_power_error_interaction[, no_effect_cols_interaction], na.rm = TRUE)
sum_flagged_uprcoverage <- rowSums(flagged_uprcoverage_interaction, na.rm = TRUE)
sum_flagged_fullcoverage <- rowSums(flagged_fullcoverage_interaction, na.rm = TRUE)



flag_sum_all_interaction <- data.frame(
  ModBias= sum_flagged_modbias,
  ExtremeBias= sum_flagged_extremebias,
  Bias = sum_flagged_bias,
  Accuracy = sum_flagged_accuracy,
  LwrCoverage = sum_flagged_coverage,
  UprCoverage = sum_flagged_uprcoverage,
  FullCoverage = sum_flagged_fullcoverage,
  Power = sum_flagged_power,
  Type1Error = sum_flagged_error
)


# Create a new dataframe to hold these sums
flag_sum_interaction <- data.frame(
  Bias = sum_flagged_bias,
  Accuracy = sum_flagged_accuracy,
  Coverage = sum_flagged_coverage,
  Power = sum_flagged_power,
  Type1Error = sum_flagged_error
)

# Count the total number of coefficients for each outcome type
total_coefficients <- ncol(flagged_bias_interaction)
total_power_coefficients <- length(true_effect_cols_interaction)
total_error_coefficients <- length(no_effect_cols_interaction)

# Calculate the percentages
flag_percent_interaction <- data.frame(
  Bias = (flag_sum_interaction$Bias / total_coefficients) * 100,
  Accuracy = (flag_sum_interaction$Accuracy / total_coefficients) * 100,
  Coverage = (flag_sum_interaction$Coverage / total_coefficients) * 100,
  Power = (flag_sum_interaction$Power / total_power_coefficients) * 100,
  Type1Error = (flag_sum_interaction$Type1Error / total_error_coefficients) * 100
)
row.names(flag_percent_interaction) <- row.names(flag_sum_interaction)

# Calculate the percentages
flag_percent_all_interaction <- data.frame(
  ModBias = (flag_sum_all_interaction$ModBias/total_coefficients)*100,
  ExtremeBias= (flag_sum_all_interaction$ExtremeBias/total_coefficients)*100,
  Bias = (flag_sum_all_interaction$Bias / total_coefficients) * 100,
  Accuracy = (flag_sum_all_interaction$Accuracy / total_coefficients) * 100,
  LwrCoverage = (flag_sum_all_interaction$LwrCoverage/ total_coefficients) * 100,
  UprCoverage = (flag_sum_all_interaction$UprCoverage / total_coefficients) * 100,
  FullCoverage = (flag_sum_all_interaction$FullCoverage / total_coefficients) * 100,
  Power = (flag_sum_all_interaction$Power / total_power_coefficients) * 100,
  Type1Error = (flag_sum_all_interaction$Type1Error / total_error_coefficients) * 100
)
row.names(flag_percent_all_interaction) <- row.names(flag_sum_all_interaction)



#Maida

sum_flagged_bias <- rowSums(flagged_bias_maidha, na.rm = TRUE)
sum_flagged_modbias <- rowSums(flagged_modbias_maidha, na.rm = TRUE)
sum_flagged_extremebias <- rowSums(flagged_extremebias_maidha, na.rm = TRUE)
sum_flagged_accuracy <- rowSums(flagged_accuracy_maidha, na.rm = TRUE)
sum_flagged_coverage <- rowSums(flagged_coverage_maidha, na.rm = TRUE)
sum_flagged_power <- rowSums(flagged_power_error_maidha[, true_effect_cols_maidha], na.rm = TRUE)
sum_flagged_error <- rowSums(flagged_power_error_maidha[, no_effect_cols_maidha], na.rm = TRUE)
sum_flagged_uprcoverage <- rowSums(flagged_uprcoverage_maidha, na.rm = TRUE)
sum_flagged_fullcoverage <- rowSums(flagged_fullcoverage_maidha, na.rm = TRUE)


flag_sum_all_maidha<- data.frame(
  ModBias= sum_flagged_modbias,
  ExtremeBias= sum_flagged_extremebias,
  Bias = sum_flagged_bias,
  Accuracy = sum_flagged_accuracy,
  LwrCoverage = sum_flagged_coverage,
  UprCoverage = sum_flagged_uprcoverage,
  FullCoverage = sum_flagged_fullcoverage,
  Power = sum_flagged_power,
  Type1Error = sum_flagged_error
)




# Create a new dataframe to hold these sums
flag_sum_maidha<- data.frame(
  Bias = sum_flagged_bias,
  Accuracy = sum_flagged_accuracy,
  Coverage = sum_flagged_coverage,
  Power = sum_flagged_power,
  Type1Error = sum_flagged_error
)

# Count the total number of coefficients for each outcome type
total_coefficients <- ncol(flagged_bias_maidha)
total_power_coefficients <- length(true_effect_cols_maidha)
total_error_coefficients <- length(no_effect_cols_maidha)

# Calculate the percentages
flag_percent_maidha <- data.frame(
  Bias = (flag_sum_maidha$Bias / total_coefficients) * 100,
  Accuracy = (flag_sum_maidha$Accuracy / total_coefficients) * 100,
  Coverage = (flag_sum_maidha$Coverage / total_coefficients) * 100,
  Power = (flag_sum_maidha$Power / total_power_coefficients) * 100,
  Type1Error = (flag_sum_maidha$Type1Error / total_error_coefficients) * 100
)
row.names(flag_percent_maidha) <- row.names(flag_sum_maidha)

# Calculate the percentages
flag_percent_all_maidha <- data.frame(
  ModBias = (flag_sum_all_maidha$ModBias/total_coefficients) *100,
  ExtremeBias= (flag_sum_all_maidha$ExtremeBias/total_coefficients) *100, 
  Bias = (flag_sum_all_maidha$Bias / total_coefficients) * 100,
  Accuracy = (flag_sum_all_maidha$Accuracy / total_coefficients) * 100,
  LwrCoverage = (flag_sum_all_maidha$LwrCoverage/ total_coefficients) * 100,
  UprCoverage = (flag_sum_all_maidha$UprCoverage / total_coefficients) * 100,
  FullCoverage = (flag_sum_all_maidha$FullCoverage / total_coefficients) * 100,
  Power = (flag_sum_all_maidha$Power / total_power_coefficients) * 100,
  Type1Error = (flag_sum_all_maidha$Type1Error / total_error_coefficients) * 100
)
row.names(flag_percent_all_maidha) <- row.names(flag_sum_all_maidha)




###Average for each outcome


#Categorical
# Initialize the new data frame
categorical_outcomes_avg <- data.frame(matrix(ncol = 8, nrow = length(rownames(bias_categorical))))
colnames(categorical_outcomes_avg) <- c("Bias", "Accuracy", "P-Value True Effect", "P-Value No Effect", "CI True Effect LL", "CI True Effect UL", "CI No Effect LL", "CI No Effect UL")
rownames(categorical_outcomes_avg) <- rownames(bias_categorical)

# Average Bias and Accuracy across all coefficients for each condition
categorical_outcomes_avg$Bias <- rowMeans(bias_categorical, na.rm = TRUE)
categorical_outcomes_avg$Accuracy <- rowMeans(accuracy_categorical, na.rm = TRUE)

# Average P-values for true effect and no effect
categorical_outcomes_avg$`P-Value True Effect` <- rowMeans(avg_categorical_p_values[, true_effect_cols_categorical], na.rm = TRUE)
categorical_outcomes_avg$`P-Value No Effect` <- rowMeans(avg_categorical_p_values[, no_effect_cols_categorical], na.rm = TRUE)

# Calculate average CI True Effect and No Effect (LL and UL separately)
true_effect_ci_LL_cols <- paste0(true_effect_cols_categorical, "_LL")
true_effect_ci_UL_cols <- paste0(true_effect_cols_categorical, "_UL")
no_effect_ci_LL_cols <- paste0(no_effect_cols_categorical, "_LL")
no_effect_ci_UL_cols <- paste0(no_effect_cols_categorical, "_UL")

categorical_outcomes_avg$`CI True Effect LL` <- rowMeans(avg_categorical_ci_values[, true_effect_ci_LL_cols], na.rm = TRUE)
categorical_outcomes_avg$`CI True Effect UL` <- rowMeans(avg_categorical_ci_values[, true_effect_ci_UL_cols], na.rm = TRUE)
categorical_outcomes_avg$`CI No Effect LL` <- rowMeans(avg_categorical_ci_values[, no_effect_ci_LL_cols], na.rm = TRUE)
categorical_outcomes_avg$`CI No Effect UL` <- rowMeans(avg_categorical_ci_values[, no_effect_ci_UL_cols], na.rm = TRUE)







#Interaction

# Initialize the new data frame
interaction_outcomes_avg <- data.frame(matrix(ncol = 8, nrow = length(rownames(bias_interaction))))
colnames(interaction_outcomes_avg) <- c("Bias", "Accuracy", "P-Value True Effect", "P-Value No Effect", "CI True Effect LL", "CI True Effect UL", "CI No Effect LL", "CI No Effect UL")
rownames(interaction_outcomes_avg) <- rownames(bias_interaction)

# Average Bias and Accuracy across all coefficients for each condition
interaction_outcomes_avg$Bias <- rowMeans(bias_interaction, na.rm = TRUE)
interaction_outcomes_avg$Accuracy <- rowMeans(accuracy_interaction, na.rm = TRUE)

# Average P-values for true effect and no effect
interaction_outcomes_avg$`P-Value True Effect` <- rowMeans(avg_interaction_p_values[, true_effect_cols_interaction], na.rm = TRUE)
interaction_outcomes_avg$`P-Value No Effect` <- rowMeans(avg_interaction_p_values[, no_effect_cols_interaction], na.rm = TRUE)

# Calculate average CI True Effect and No Effect (LL and UL separately)
true_effect_ci_LL_cols <- paste0(true_effect_cols_interaction, "_LL")
true_effect_ci_UL_cols <- paste0(true_effect_cols_interaction, "_UL")
no_effect_ci_LL_cols <- paste0(no_effect_cols_interaction, "_LL")
no_effect_ci_UL_cols <- paste0(no_effect_cols_interaction, "_UL")

interaction_outcomes_avg$`CI True Effect LL` <- rowMeans(avg_interaction_ci_values[, true_effect_ci_LL_cols], na.rm = TRUE)
interaction_outcomes_avg$`CI True Effect UL` <- rowMeans(avg_interaction_ci_values[, true_effect_ci_UL_cols], na.rm = TRUE)
interaction_outcomes_avg$`CI No Effect LL` <- rowMeans(avg_interaction_ci_values[, no_effect_ci_LL_cols], na.rm = TRUE)
interaction_outcomes_avg$`CI No Effect UL` <- rowMeans(avg_interaction_ci_values[, no_effect_ci_UL_cols], na.rm = TRUE)


#Maidha

# Initialize the new data frame
maidha_outcomes_avg <- data.frame(matrix(ncol = 8, nrow = length(rownames(bias_maidha))))
colnames(maidha_outcomes_avg) <- c("Bias", "Accuracy", "P-Value True Effect", "P-Value No Effect", "CI True Effect LL", "CI True Effect UL", "CI No Effect LL", "CI No Effect UL")
rownames(maidha_outcomes_avg) <- rownames(bias_maidha)

# Average Bias and Accuracy across all coefficients for each condition
maidha_outcomes_avg$Bias <- rowMeans(bias_maidha, na.rm = TRUE)
maidha_outcomes_avg$Accuracy <- rowMeans(accuracy_maidha, na.rm = TRUE)

# Average P-values for true effect and no effect
maidha_outcomes_avg$`P-Value True Effect` <- rowMeans(interval_means_maidha[, true_effect_cols_maidha], na.rm = TRUE)
maidha_outcomes_avg$`P-Value No Effect` <- rowMeans(interval_means_maidha[, no_effect_cols_maidha], na.rm = TRUE)

# Calculate average CI True Effect and No Effect (LL and UL separately)
true_effect_ci_LL_cols <- paste0(true_effect_cols_maidha, "_LL")
true_effect_ci_UL_cols <- paste0(true_effect_cols_maidha, "_UL")
no_effect_ci_LL_cols <- paste0(no_effect_cols_maidha, "_LL")
no_effect_ci_UL_cols <- paste0(no_effect_cols_maidha, "_UL")

maidha_outcomes_avg$`CI True Effect LL` <- rowMeans(avg_maidha_ci_values[, true_effect_ci_LL_cols], na.rm = TRUE)
maidha_outcomes_avg$`CI True Effect UL` <- rowMeans(avg_maidha_ci_values[, true_effect_ci_UL_cols], na.rm = TRUE)
maidha_outcomes_avg$`CI No Effect LL` <- rowMeans(avg_maidha_ci_values[, no_effect_ci_LL_cols], na.rm = TRUE)
maidha_outcomes_avg$`CI No Effect UL` <- rowMeans(avg_maidha_ci_values[, no_effect_ci_UL_cols], na.rm = TRUE)






##Summary Tables

# Initialize an empty data frame for total flags



total_flags_table_3 <- data.frame(matrix(ncol = 3, nrow = 5))
colnames(total_flags_table_3) <- c("Categorical", "Interaction", "Maidha")
rownames(total_flags_table_3) <- c("Bias", "Accuracy", "Coverage", "Power", "Type1Error")

# Fill in the data frame with total flag counts
total_flags_table_3$Categorical <- colSums(flag_sum_categorical)
total_flags_table_3$Interaction <- colSums(flag_sum_interaction)
total_flags_table_3$Maidha <- colSums(flag_sum_maidha)




#Total sums (assuming 24 dropping all n_5000_p3 conditions) 

#Interaction-- Bias: 288; Accuracy: 288; Coverage: 288; Power: 120; Type 1 Error: 168
#Categorical-- Bias: 984; Accuracy: 984; Coverage: 984; Power: 288; Type 1 Error: 696
#Maidha-- Bias: 1008; Accuracy: 1008; Coverage: 1008; Power: 288; Type 1 Error: 720


# Define the outcome names
outcomes <- c("Bias", "Accuracy", "Coverage", "Power", "Type1Error")

# Define the total possible flags for each model and each outcome
total_flags_interaction <- c(288, 288, 288, 120, 168)
total_flags_categorical <- c(984, 984, 984, 288, 696)
total_flags_maidha <- c(1008, 1008, 1008, 288, 720)

# Initialize an empty data frame
summary_table_3 <- data.frame(matrix(ncol = 2, nrow = length(outcomes)))
colnames(summary_table_3) <- c("Categorical", "Interaction")
row.names(summary_table_3) <- outcomes

# Fill in the data frame with percentages
summary_table_3$Categorical <- (colSums(flag_sum_categorical) / total_flags_categorical) * 100
summary_table_3$Interaction <- (colSums(flag_sum_interaction) / total_flags_interaction) * 100
summary_table_3$Maidha <- (colSums(flag_sum_maidha) / total_flags_maidha) * 100




# Define the outcomes
outcomes <- c("Bias", "Accuracy", "P-Value True Effect", "P-Value No Effect", "CI True Effect LL", "CI True Effect UL", "CI No Effect LL", "CI No Effect UL")

# Initialize the final data frame to hold averages
average_outcomes_3 <- data.frame(matrix(ncol = 3, nrow = length(outcomes)))
colnames(average_outcomes_3) <- c("Interaction", "Categorical", "Maidha")
rownames(average_outcomes_3) <- outcomes

# Calculate the average for each outcome across conditions for each model
average_outcomes_3$Interaction <- colMeans(interaction_outcomes_avg, na.rm = TRUE)
average_outcomes_3$Categorical <- colMeans(categorical_outcomes_avg, na.rm = TRUE)
average_outcomes_3$Maidha <- colMeans(maidha_outcomes_avg, na.rm = TRUE)





# Create an empty data frame with model names as column names and conditions as row names
final_aic_bic_table <- data.frame(matrix(ncol = 6, nrow = nrow(average_aic_bic_categorical)))
colnames(final_aic_bic_table) <- c("Interaction_AIC", "Interaction_BIC", "Categorical_AIC", "Categorical_BIC", "Maidha_AIC", "Maidha_BIC")
rownames(final_aic_bic_table) <- rownames(average_aic_bic_categorical)

# Fill in the AIC and BIC values for each model
final_aic_bic_table$Interaction_AIC <- average_aic_bic_interaction$AIC
final_aic_bic_table$Interaction_BIC <- average_aic_bic_interaction$BIC

final_aic_bic_table$Categorical_AIC <- average_aic_bic_categorical$AIC
final_aic_bic_table$Categorical_BIC <- average_aic_bic_categorical$BIC

final_aic_bic_table$Maidha_AIC <- average_aic_bic_maidha$AIC
final_aic_bic_table$Maidha_BIC <- average_aic_bic_maidha$BIC









###############################################################################
###########################Save Tables#########################################

#####Overall Results#######
####lists



save(interaction_results,categorical_results, maidha_intercepts_list, file =
       "Documents/Dissertation_Results/3cat_intersection_results.RData")

save(interaction_p_values_list_cleaned, categorical_p_values_list, hyp_test_maidha,
     file= "Documents/Dissertation_Results/3cat_p_values.RData")

save(interaction_aic_bic_values_list, categorical_aic_bic_values_list, maidha_aic_bic_values_list,
     file= "Documents/Dissertation_Results/3cat_aic_bic.RData")

save(intersectional_ci_values_list, categorical_ci_values_list, maidha_ci_values_list,
     file = "Documents/Dissertation_Results/3cat_ci_values.RData")

save(coverage_results_interaction, coverage_results_categorical, coverage_results_maidha, 
     file = "Documents/Dissertation_Results/3cat_coverage_results.RData")

save(categorical_icc_values_list, interaction_icc_values_list, maidha_icc_values_list, 
     file = "Documents/Dissertation_Results/3cat_ICC_results.RData")



###Data Frames
average_ci_values <- list(
  "Interaction" = avg_interaction_ci_values,
  "Categorical" = avg_categorical_ci_values,
  "Maidha" = avg_maidha_ci_values
)


average_p_values <- list(
  "Interaction" = avg_interaction_p_values,
  "Categorical" = avg_categorical_p_values,
  "Maidha" = interval_means_maidha
)


average_intersectional_values <- list(
  "Interaction" = interaction_int_effects,
  "Categorical" = categorical_avg_coefficients,
  "Maidha" = maidha_avg_intercepts
)


average_aic_bic_values <- list(
  "Interaction" = average_aic_bic_interaction,
  "Categorical" = average_aic_bic_categorical,
  "Maidha" = average_aic_bic_maidha
)


average_seb_values <- list(
  "Interaction" = seb_interaction_df,
  "Categorical" = seb_categorical_df,
  "Maidha" = seb_maidha_df
)


average_bias_values <- list(
  "Interaction" = bias_interaction,
  "Categorical" = bias_categorical,
  "Maidha" = bias_maidha
)


average_accuracy_values <- list(
  "Interaction" = accuracy_interaction,
  "Categorical" = accuracy_categorical,
  "Maidha" = accuracy_maidha
)



coverage_percent <- list(
  "Interaction" = overall_coverage_percentages_interaction,
  "Categorical" = overall_coverage_percentages_categorical,
  "Maidha" = overall_coverage_percentages_maidha
)



p_value_percent <- list(
  "Interaction" = p_value_percent_interaction,
  "Categorical" = p_value_percent_categorical,
  "Maidha" = sig_percent_maidha
)



outcomeavg_model <- list(
  "Interaction" = interaction_outcomes_avg,
  "Categorical" = categorical_outcomes_avg,
  "Maidha" = maidha_outcomes_avg
)




####Flagging########

flagged_bias_tables <- list(
  "Interaction" = flagged_bias_interaction,
  "Categorical" = flagged_bias_categorical,
  "Maidha" = flagged_bias_maidha
)



flagged_accuracy_tables <- list(
  "Interaction" = flagged_accuracy_interaction,
  "Categorical" = flagged_accuracy_categorical,
  "Maidha" = flagged_accuracy_maidha
)



flagged_power_error_tables <- list(
  "Interaction" = flagged_power_error_interaction,
  "Categorical" = flagged_power_error_categorical,
  "Maidha" = flagged_power_error_maidha
)



flagged_coverage_tables <- list(
  "Interaction" = flagged_coverage_interaction,
  "Categorical" = flagged_coverage_categorical,
  "Maidha" = flagged_coverage_maidha
)


flagged_percent_tables <- list(
  "Interaction" = flag_percent_interaction,
  "Categorical" = flag_percent_categorical,
  "Maidha" = flag_percent_maidha
)



flag_sum_tables <- list(
  "Interaction" = flag_sum_interaction,
  "Categorical" = flag_sum_categorical,
  "Maidha" = flag_sum_maidha
)




flagged_percent_all_tables <- list(
  "Interaction" = flag_percent_all_interaction,
  "Categorical" = flag_percent_all_categorical,
  "Maidha" = flag_percent_all_maidha
)


flag_sum_all_tables <- list(
  "Interaction" = flag_sum_all_interaction,
  "Categorical" = flag_sum_all_categorical,
  "Maidha" = flag_sum_all_maidha
)



# Save each of your data frame lists into Excel, and include the row names
write.xlsx(average_ci_values, "Documents/Dissertation_Results/3cat_average_ci_values.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(average_p_values, "Documents/Dissertation_Results/3cat_average_p_values.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(average_intersectional_values, "Documents/Dissertation_Results/3cat_average_intersectioanl_values.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(average_aic_bic_values, "Documents/Dissertation_Results/3cat_average_aic_bic_values.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(average_seb_values, "Documents/Dissertation_Results/3cat_average_seb_values.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(average_bias_values, "Documents/Dissertation_Results/3cat_average_bias_values.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(average_accuracy_values, "Documents/Dissertation_Results/3cat_average_accuracy_values.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(coverage_percent, "Documents/Dissertation_Results/3cat_coverage_percent.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(p_value_percent, "Documents/Dissertation_Results/3cat_p_value_percent.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(outcomeavg_model, "Documents/Dissertation_Results/3cat_outcomeavg_model.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(flagged_bias_tables, "Documents/Dissertation_Results/3cat_flagged_bias.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(flagged_accuracy_tables, "Documents/Dissertation_Results/3cat_flagged_accuracy.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(flagged_power_error_tables, "Documents/Dissertation_Results/3cat_flagged_power_error.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(flagged_coverage_tables, "Documents/Dissertation_Results/3cat_flagged_coverage.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(flagged_percent_tables, "Documents/Dissertation_Results/3cat_flagged_percent.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(flag_sum_tables, "Documents/Dissertation_Results/3cat_flag_sums.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(flagged_percent_all_tables, "Documents/Dissertation_Results/3cat_flagged_percent_all.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(flag_sum_all_tables, "Documents/Dissertation_Results/3cat_flag_sums_all.xlsx", asTable = TRUE, rowNames = TRUE)


####Summarizing#####

write.xlsx(total_flags_table_3, "Documents/Dissertation_Results/3cat_total_flags.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(summary_table_3, "Documents/Dissertation_Results/3cat_percent_flags.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(average_outcomes_3, "Documents/Dissertation_Results/3cat_average_outcomes.xlsx", asTable = TRUE, rowNames = TRUE)
write.xlsx(final_aic_bic_table, "Documents/Dissertation_Results/3cat_AIC_BIC.xlsx", asTable = TRUE, rowNames = TRUE)




###Distributions of Outcomes

#Bias

#Interaction

# Extract the "interaction" dataframe from the list "average_bias_outcomes"
interaction_df <- average_bias_values$Interaction

# Flatten the dataframe to get a single vector of all data points
all_data_points <- as.vector(as.matrix(interaction_df))

# Create a new dataframe to store all data points
flattened_df <- data.frame(Value = all_data_points)


# Extract the "Interaction" dataframe from the list "average_seb_values"
interaction_seb_df <- average_seb_values$Interaction

# Calculate the mean of all values in the dataframe
mean_value <- mean(as.vector(as.matrix(interaction_seb_df)))

# Calculate 2x the mean value
double_mean_value <- 2 * mean_value
negative_double_mean_value <- -double_mean_value
moderate_bias<-.5*mean_value
negative_moderate_bias<- -moderate_bias



# Plot the histogram
interaction_extremebias<-ggplot(flattened_df, aes(x=Value)) +
  geom_histogram(binwidth=0.05, fill="blue", alpha=0.7, color="black") +
  ggtitle("Interaction Extreme Bias") +
  geom_vline(aes(xintercept=double_mean_value), color="red", linetype="dashed", linewidth=1) +
  geom_vline(aes(xintercept=negative_double_mean_value), color="red", linetype="dashed", linewidth=1) +
  xlab("Value") +
  ylab("Frequency") 


# Plot the histogram
interaction_modbias<-ggplot(flattened_df, aes(x=Value)) +
  geom_histogram(binwidth=0.05, fill="blue", alpha=0.7, color="black") +
  ggtitle("Interaction Moderate Bias") +
  geom_vline(aes(xintercept=moderate_bias), color="red", linetype="dashed", linewidth=1) +
  geom_vline(aes(xintercept=negative_moderate_bias), color="red", linetype="dashed", linewidth=1) +
  xlab("Value") +
  ylab("Frequency") 



#Categorical

# Extract the "interaction" dataframe from the list "average_bias_outcomes"
categorical_df <- average_bias_values$Categorical

# Flatten the dataframe to get a single vector of all data points
all_data_points <- as.vector(as.matrix(categorical_df))

# Create a new dataframe to store all data points
flattened_df <- data.frame(Value = all_data_points)

# Extract the "Interaction" dataframe from the list "average_seb_values"
categorical_seb_df <- average_seb_values$Categorical

# Calculate the mean of all values in the dataframe
mean_value <- mean(as.vector(as.matrix(categorical_seb_df)))

# Calculate 2x the mean value
double_mean_value <- 2 * mean_value
negative_double_mean_value <- -double_mean_value
moderate_bias<-.5*mean_value
negative_moderate_bias<- -moderate_bias

# Plot the histogram
categorical_extremebias<-ggplot(flattened_df, aes(x=Value)) +
  geom_histogram(binwidth=0.05, fill="blue", alpha=0.7, color="black") +
  ggtitle("Categorical Extreme Bias") +
  geom_vline(aes(xintercept=double_mean_value), color="red", linetype="dashed", linewidth=1) +
  geom_vline(aes(xintercept=negative_double_mean_value), color="red", linetype="dashed", linewidth=1) +
  xlab("Value") +
  ylab("Frequency")

# Plot the histogram
categorical_modbias<-ggplot(flattened_df, aes(x=Value)) +
  geom_histogram(binwidth=0.05, fill="blue", alpha=0.7, color="black") +
  ggtitle("Categorical Moderate Bias") +
  geom_vline(aes(xintercept=moderate_bias), color="red", linetype="dashed", linewidth=1) +
  geom_vline(aes(xintercept=negative_moderate_bias), color="red", linetype="dashed", linewidth=1) +
  xlab("Value") +
  ylab("Frequency")



#Maidha
# Extract the "interaction" dataframe from the list "average_bias_outcomes"
maidha_df <- average_bias_values$Maidha

# Flatten the dataframe to get a single vector of all data points
all_data_points <- as.vector(as.matrix(maidha_df))

# Create a new dataframe to store all data points
flattened_df <- data.frame(Value = all_data_points)


# Extract the "Interaction" dataframe from the list "average_seb_values"
maidha_seb_df <- average_seb_values$Maidha
# Calculate the mean of all values in the dataframe
mean_value <- mean(as.vector(as.matrix(maidha_seb_df)))



# Calculate 2x the mean value
double_mean_value <- 2 * mean_value
negative_double_mean_value <- -double_mean_value


moderate_bias<-.5*mean_value
negative_moderate_bias<- -moderate_bias


# Plot the histogram
maidha_extremebias<-ggplot(flattened_df, aes(x=Value)) +
  geom_histogram(binwidth=0.05, fill="blue", alpha=0.7, color="black") +
  ggtitle("MAIDHA Extreme Bias") +
  geom_vline(aes(xintercept=double_mean_value), color="red", linetype="dashed", linewidth=1) +
  geom_vline(aes(xintercept=negative_double_mean_value), color="red", linetype="dashed", linewidth=1) +
  xlab("Value") +
  ylab("Frequency")

maidha_modbias<-ggplot(flattened_df, aes(x=Value)) +
  geom_histogram(binwidth=0.05, fill="blue", alpha=0.7, color="black") +
  ggtitle("MAIDHA Moderate Bias") +
  geom_vline(aes(xintercept=moderate_bias), color="red", linetype="dashed", linewidth=1) +
  geom_vline(aes(xintercept=negative_moderate_bias), color="red", linetype="dashed", linewidth=1) +
  xlab("Value") +
  ylab("Frequency")



#Accuracy

#Interaction

# Extract the "interaction" dataframe from the list "average_bias_outcomes"
interaction_df <- average_accuracy_values$Interaction

# Flatten the dataframe to get a single vector of all data points
all_data_points <- as.vector(as.matrix(interaction_df))

# Create a new dataframe to store all data points
flattened_df <- data.frame(Value = all_data_points)


# Plot the histogram
interaction_accuracy<-ggplot(flattened_df, aes(x=Value)) +
  geom_histogram(binwidth=0.05, fill="blue", alpha=0.7, color="black") +
  ggtitle("Interaction Accuracy") +
  geom_vline(aes(xintercept=.5), color="red", linetype="dashed", linewidth=1) +
  xlab("Value") +
  ylab("Frequency") 




#Categorical

# Extract the "interaction" dataframe from the list "average_bias_outcomes"
categorical_df <- average_accuracy_values$Categorical

# Flatten the dataframe to get a single vector of all data points
all_data_points <- as.vector(as.matrix(categorical_df))

# Create a new dataframe to store all data points
flattened_df <- data.frame(Value = all_data_points)



# Plot the histogram
categorical_accuracy<-ggplot(flattened_df, aes(x=Value)) +
  geom_histogram(binwidth=0.05, fill="blue", alpha=0.7, color="black") +
  ggtitle("Categorical Accuracy") +
  geom_vline(aes(xintercept=.5), color="red", linetype="dashed", linewidth=1) +
  xlab("Value") +
  ylab("Frequency")


#Maidha
# Extract the "interaction" dataframe from the list "average_bias_outcomes"
maidha_df <- average_accuracy_values$Maidha

# Flatten the dataframe to get a single vector of all data points
all_data_points <- as.vector(as.matrix(maidha_df))

# Create a new dataframe to store all data points
flattened_df <- data.frame(Value = all_data_points)


# Plot the histogram
maidha_accuracy<-ggplot(flattened_df, aes(x=Value)) +
  geom_histogram(binwidth=0.05, fill="blue", alpha=0.7, color="black") +
  ggtitle("MAIDHA Accuracy") +
  geom_vline(aes(xintercept=.5), color="red", linetype="dashed", linewidth=1) +
  xlab("Value") +
  ylab("Frequency")




###True Effect P-Values

#Interaction

true_effect_data <- avg_interaction_p_values[, true_effect_cols_interaction]
true_effect_data_melted <- as.data.frame(as.table(as.matrix(true_effect_data)))

# Create a long format data frame for no effect columns
no_effect_data <- avg_interaction_p_values[, no_effect_cols_interaction]
no_effect_data_melted <- as.data.frame(as.table(as.matrix(no_effect_data)))


# Create histograms for True Effect Coefficients
interaction_true <- ggplot(true_effect_data_melted, aes(x=Freq)) +
  geom_histogram(binwidth=0.05, fill="blue", alpha=0.7, color="black") +
  geom_vline(aes(xintercept=0.05), color="red", linetype="dashed", linewidth=1) +
  ggtitle("True Effect - Interaction") +
  xlab("Value") +
  ylab("Frequency")


# Create histograms for No Effect Coefficients
interaction_none <- ggplot(no_effect_data_melted, aes(x=Freq)) +
  geom_histogram(binwidth=0.05, fill="blue", alpha=0.7, color="black") +
  geom_vline(aes(xintercept=0.05), color="red", linetype="dashed", linewidth=1) +
  ggtitle("No Effect - Interaction") +
  xlab("Value") +
  ylab("Frequency")




#Categorical


# Create a long format data frame for true effect columns
true_effect_data <- avg_categorical_p_values[, true_effect_cols_categorical]
true_effect_data_melted <- as.data.frame(as.table(as.matrix(true_effect_data)))

# Create a long format data frame for no effect columns
no_effect_data <- avg_categorical_p_values[, no_effect_cols_categorical]
no_effect_data_melted <- as.data.frame(as.table(as.matrix(no_effect_data)))


# Create histograms for True Effect Coefficients
categorical_true <- ggplot(true_effect_data_melted, aes(x=Freq)) +
  geom_histogram(binwidth=0.05, fill="blue", alpha=0.7, color="black") +
  geom_vline(aes(xintercept=0.05), color="red", linetype="dashed", linewidth=1) +
  ggtitle("True Effect - Categorical") +
  xlab("Value") +
  ylab("Frequency")


# Create histograms for No Effect Coefficients
categorical_none <- ggplot(no_effect_data_melted, aes(x=Freq)) +
  geom_histogram(binwidth=0.05, fill="blue", alpha=0.7, color="black") +
  geom_vline(aes(xintercept=0.05), color="red", linetype="dashed", linewidth=1) +
  ggtitle("No Effect - Categorical") +
  xlab("Value") +
  ylab("Frequency")



##Percent of P-values 

#Interaction

# Extract the specific 'categorical_data' data frame from the list
# Replace 'categorical_data' with the correct name if different
interaction_data <- p_value_percent$Interaction

# Data for true effect columns
true_effect_data <- interaction_data[, true_effect_cols_interaction, drop = FALSE]
true_effect_data_melted <- as.data.frame(as.table(as.matrix(true_effect_data)))

# Data for no effect columns
no_effect_data <- interaction_data[, no_effect_cols_interaction, drop = FALSE]
no_effect_data_melted <- as.data.frame(as.table(as.matrix(no_effect_data)))

# Plot for true effect columns
true_pct_pval_interaction <- ggplot(true_effect_data_melted, aes(x=Freq)) +
  geom_histogram(binwidth=5, fill="blue", alpha=0.7, color="black") +
  geom_vline(aes(xintercept=95), color="red", linetype="dashed", linewidth=1) +
  ggtitle("True Effect - Interaction") +
  xlab("Value") +
  ylab("Frequency")

# Plot for no effect columns
none_pct_pval_interaction <- ggplot(no_effect_data_melted, aes(x=Freq)) +
  geom_histogram(binwidth=5, fill="blue", alpha=0.7, color="black") +
  geom_vline(aes(xintercept=5), color="red", linetype="dashed", linewidth=1) +
  ggtitle("No Effect - Interaction") +
  xlab("Value") +
  ylab("Frequency")





#Categorical


categorical_data <- p_value_percent$Categorical

# Data for true effect columns
true_effect_data <- categorical_data[, true_effect_cols_categorical, drop = FALSE]
true_effect_data_melted <- as.data.frame(as.table(as.matrix(true_effect_data)))

# Data for no effect columns
no_effect_data <- categorical_data[, no_effect_cols_categorical, drop = FALSE]
no_effect_data_melted <- as.data.frame(as.table(as.matrix(no_effect_data)))

# Plot for true effect columns
true_pct_pval_categorical <- ggplot(true_effect_data_melted, aes(x=Freq)) +
  geom_histogram(binwidth=5, fill="blue", alpha=0.7, color="black") +
  geom_vline(aes(xintercept=95), color="red", linetype="dashed", linewidth=1) +
  ggtitle("True Effect - Categorical ") +
  xlab("Value") +
  ylab("Frequency")


# Plot for no effect columns
none_pct_pval_categorical <- ggplot(no_effect_data_melted, aes(x=Freq)) +
  geom_histogram(binwidth=5, fill="blue", alpha=0.7, color="black") +
  geom_vline(aes(xintercept=5), color="red", linetype="dashed", linewidth=1) +
  ggtitle("No Effect - Categorical ") +
  xlab("Value") +
  ylab("Frequency")


#Maidha

# Extract the specific 'categorical_data' data frame from the list
# Replace 'categorical_data' with the correct name if different
maidha_data <- p_value_percent$Maidha

# Data for true effect columns
true_effect_data <- maidha_data[, true_effect_cols_maidha, drop = FALSE]
true_effect_data_melted <- as.data.frame(as.table(as.matrix(true_effect_data)))

# Data for no effect columns
no_effect_data <- maidha_data[, no_effect_cols_maidha, drop = FALSE]
no_effect_data_melted <- as.data.frame(as.table(as.matrix(no_effect_data)))

# Plot for true effect columns
true_pct_pval_maidha <- ggplot(true_effect_data_melted, aes(x=Freq)) +
  geom_histogram(binwidth=5, fill="blue", alpha=0.7, color="black") +
  geom_vline(aes(xintercept=95), color="red", linetype="dashed", linewidth=1) +
  ggtitle("True Effect - MAIDHA") +
  xlab("Value") +
  ylab("Frequency")


# Plot for no effect columns
none_pct_pval_maidha <- ggplot(no_effect_data_melted, aes(x=Freq)) +
  geom_histogram(binwidth=5, fill="blue", alpha=0.7, color="black") +
  geom_vline(aes(xintercept=5), color="red", linetype="dashed", linewidth=1) +
  ggtitle("No Effect - MAIDHA") +
  xlab("Value") +
  ylab("Frequency")



#Coverage

#Interaction
melted_data <- as.data.frame(as.table(as.matrix(overall_coverage_percentages_interaction)))

# Create the histogram
coverage_interaction <- ggplot(melted_data, aes(x=Freq)) +
  geom_histogram(binwidth=1, fill="blue", alpha=0.7, color="black") +
  geom_vline(aes(xintercept=92.5), color="red", linetype="dashed", linewidth=1) +
  ggtitle("Overall Coverage Percentages Interaction") +
  xlab("Value") +
  ylab("Frequency")



#Categorical
melted_data <- as.data.frame(as.table(as.matrix(overall_coverage_percentages_categorical)))

# Create the histogram
coverage_categorical <- ggplot(melted_data, aes(x=Freq)) +
  geom_histogram(binwidth=1, fill="blue", alpha=0.7, color="black") +
  geom_vline(aes(xintercept=92.5), color="red", linetype="dashed", linewidth=1) +
  ggtitle("Overall Coverage Percentages Categorical") +
  xlab("Value") +
  ylab("Frequency")


#MAIDHA
melted_data <- as.data.frame(as.table(as.matrix(overall_coverage_percentages_maidha)))

# Create the histogram
coverage_maidha <- ggplot(melted_data, aes(x=Freq)) +
  geom_histogram(binwidth=1, fill="blue", alpha=0.7, color="black") +
  geom_vline(aes(xintercept=92.5), color="red", linetype="dashed", linewidth=1) +
  ggtitle("Overall Coverage Percentages MAIDHA") +
  xlab("Value") +
  ylab("Frequency")


# Function to save grid to file
save_grid <- function(grid_obj, filename) {
  pdf(filename)
  grid.draw(grid_obj)
  dev.off()
}

# Create and save the grid arrangements
viz_extreme_bias <- arrangeGrob(interaction_extremebias, categorical_extremebias, maidha_extremebias)
save_grid(viz_extreme_bias, "Dissertation_Results/Plots/3cat_viz_extreme_bias.pdf")

viz_mod_bias <- arrangeGrob(interaction_modbias, categorical_modbias, maidha_modbias)
save_grid(viz_mod_bias, "Dissertation_Results/Plots/3cat_viz_mod_bias.pdf")

viz_accuracy <- arrangeGrob(interaction_accuracy, categorical_accuracy, maidha_accuracy)
save_grid(viz_accuracy, "Dissertation_Results/Plots/3cat_viz_accuracy.pdf")

viz_p_values <- arrangeGrob(interaction_true, categorical_true, interaction_none, categorical_none)
save_grid(viz_p_values, "Dissertation_Results/Plots/3cat_viz_p_values.pdf")

viz_pct_pvals_true <- arrangeGrob(true_pct_pval_interaction, true_pct_pval_categorical, true_pct_pval_maidha)
save_grid(viz_pct_pvals_true, "Dissertation_Results/Plots/3cat_viz_pct_pvals_true.pdf")

viz_pct_pvals_none <- arrangeGrob(none_pct_pval_interaction, none_pct_pval_categorical, none_pct_pval_maidha)
save_grid(viz_pct_pvals_none, "Dissertation_Results/Plots/3cat_viz_pct_pvals_none.pdf")

viz_coverage <- arrangeGrob(coverage_interaction, coverage_categorical, coverage_maidha)
save_grid(viz_coverage, "Dissertation_Results/Plots/3cat_viz_coverage.pdf")




# Save the entire environment to a .RData file
save.image("Documents/Dissertation_Results/3cat_envi.RData")



