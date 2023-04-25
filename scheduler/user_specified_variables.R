###------------------------------------###
###---USER-MADE VARIABLE NAMES---------###
###------------------------------------###

# library(tidyr); library(dplyr)

# Edit the following variables to tailor
# code to specific redcap project

# Project ID in REDCap
PID <- "7091"

###---------------------------------------------###
### Info on randomization variable and levels
###---------------------------------------------###

# Column name indicating study arm
# We know that a participant is randomized if
#     study_arm %in% already_randomized_val
study_arm = "arm" 

# List indicating the integer value associated with treatment and control arms in REDCap
# Important, the list order should be control first, then treatment.
already_randomized_value = list(
  control = 0, 
  treatment = 1
)

# Column name indicating randomization is ready
# We know that a record is ready to be randomized when 
#     randomization_ready == randomization_ready_val
randomization_ready = "randomize"
randomization_ready_val = "Yes"


###---------------------------------------------###
### Info on covariates on which we want balance
###---------------------------------------------###

# Column name for stratification variables
# Change to c("variable_name") to indicate REDCap variable name
# If no stratification is used prior to MSB, then leave the value as NULL
stratification_variables <- NULL
# Note that stratification is only recommended if strata N>75-100 minimum

# If you are not stratifying on center, but want within-center balance, then specify
# the column name in REDCap here.
study_center = "center" 

# List of covariates on which to maintin balance
bal_covariates = c(
  # List covariate column names used to
  # compute balance
  "age", "rand_race", "gender", "disease_status"
)

# List of variable types (numeric, factor) 
# that correspond to the bal_covariates above
bal_covariate_type <- list(
  "numeric", "factor", "factor", "factor"
)



###---------------------------------------------###
### Info on REDCap project
###---------------------------------------------###

# For repeated measures at different timelines,
# we need to collect baseline data for MSB algorithm
baseline_event = "baseline_arm_1"

# REDCap API token and relevant URL
token = ""
URL <- "https://redcap.nubic.northwestern.edu/redcap/api/"



###---------------------------------------------###
### Info on MSB algorithm
###---------------------------------------------###

# Sample size required before starting adaptive randomization
min_n_algorithm <- 20 

# Minimum sample size per center before allowing center balance to influence MSB algorithm
min_n_center <- 4

# P-value cutoff for balance tests 
p_cutoff <- 0.3

# Cohen's cutoff for balance on continuous covariates
use_p_cutoff <- TRUE # Change to FALSE to use the cutoff on Cohen's d.
d_cutoff <- 0.1

# Notes: Change to TRUE if you want to document notes about MSB randomization in REDCap
#   If TRUE, specify the column name for the randomization notes.
notes <- TRUE

# Randomization results fields
vote_name <- "randomization_notes"
prob_name <- "rand_prob"




###----------------------------------------------------###
### Sanitize/Standardize Balance Covariates
###
### This function is used to standardize variables 
### relevant to the MSB algorithm according to the 
### types listed above and any rules specified for 
### collapsing or labelling categories.
###----------------------------------------------------###

# Check for clash between center in MSB vs. center stratification
if(study_center %in% stratification_variables){
  study_center <- NULL
  print("STUDY CENTER WILL BE STRATIFIED ON AND NOT INCLUDED IN MSB CALCULATIONS.")
}


bal_covariate_type <- lapply(bal_covariate_type,
                             FUN = function(x) paste0("as.", x)
)
# names(bal_covariate_type) <- bal_covariates


standardize_msb_variables <- function(data, bal_covariates, bal_covariate_type){
  
  out <- data %>% 
    mutate_at(
      .vars = bal_covariates,
      .funs = bal_covariate_type
    )
  
  return(out)
  
}




