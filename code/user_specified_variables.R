###------------------------------------###
###---USER-MADE VARIABLE NAMES---------###
###------------------------------------###

library(tidyverse)

# Edit the following variables to tailor
# code to specific redcap project

# Project ID in REDCap
PID <- "7019"

# Column name indicating study arm
# We know that a participant is randomized if
#     study_arm %in% already_randomized_val
study_arm = "arm" 
already_randomized_value = c(1, 2)


# Column name indicating randomization is ready
# We know that a record is ready to be randomized when 
#     randomization_ready == randomization_ready_val
randomization_ready = "randomize"
randomization_ready_val = 1


# Randomization results fields
rand_prob_field = "rand_prob"
rand_vote_fiedl = "rand_vote"


# For repeated measures at different timelines,
# we need to collect baseline data for MSB algorithm
baseline_event = "Baseline"


# List of covariates on which to maintina balance
bal_covariates = c(
  # List covariate column names used to
  # compute balance
  "age", "rand_race", "gender", "disease_status"
)
study_center = "center" # Used for multi-center trials

# List of variable types (numeric, factor) 
# that correspond to the bal_covariates above
bal_covariate_type <- list(
  "numeric", "factor", "factor", "factor"
)
bal_covariate_type <- lapply(bal_covariate_type,
                             FUN = function(x) paste0("as.", x)
)
# names(bal_covariate_type) <- bal_covariates


# REDCap API token and relevant URL
token = ""
URL <- "https://redcap.nubic.northwestern.edu/redcap/api/"


# Sample size required before starting adaptive randomization
min_n_algorithm <- 100 

# Minimum sample size per center before allowing center balance to influence MSB algorithm
min_n_center <- 4

# P-value cutoff for balance tests 
p_cutoff <- 0.3

# Notes: Change to TRUE if you want to document notes about MSB randomization in REDCap
#   If TRUE, specify the column name for the randomization notes.
notes <- FALSE
note_name <- "randomization_notes"

###----------------------------------------------------###
### Sanitize/Standardize Balance Covariates
###
### This function is used to standardize variables 
### relevant to the MSB algorithm according to the 
### types listed above and any rules specified for 
### collapsing or labelling categories.
###----------------------------------------------------###

standardize_msb_variables <- function(data, bal_covariates, bal_covariate_type){
  
  out <- data %>% 
    mutate_at(
      .vars = bal_covariates,
      .funs = bal_covariate_type
    )
  
  return(out)
  
}




