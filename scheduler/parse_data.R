###---------------------------###
### Libraries
###---------------------------###
library(dplyr)
library(tidyr)
library(redcapAPI)


###---------------------------###
### Get arguments specified by
###   user based on REDCap project
###---------------------------###
source("user_specified_variables.R")



###---------------------------###
### Read in REDCap data
###---------------------------###

# Establish REDCap connection
rc_con <- redcapConnection(
  url = URL,
  token = token, # Set API token as global variable
  conn = con,
  project = PID,
  config = httr::config()
)

# Read in data
data <- exportRecords(
  rc_con
)


# Stash a version of the REDCap database on the server just in case.
saveRDS(data, paste0("data/redcap_db.RDS"))


###---------------------------###
### Get baseline data
###---------------------------###

# Some MSB data may need to be pre-processed. This can be done below.
# If addtional processing is required, you may either 
#     1. add to the lines below
#     2. modify the standardize_msb_variables function in the user_specified_variables.R script
baseline_data <- data[which(data$redcap_event_name == baseline_event), ]
# standardize_msb_variables(
#   data[which(data$redcap_event_name == baseline_event), ],
#   bal_covariates,
#   bal_covariate_type
# )



###---------------------------###
### Separate by new/old units
### New units are marked as ready
###   but haven't been randomized
### Old units have already been
###   randomized.
###---------------------------###

new_to_randomize <- baseline_data[
  which(is.na(baseline_data[[study_arm]]) & 
          baseline_data[[randomization_ready]] == randomization_ready_val), 
]

already_randomized <- baseline_data[
  which(baseline_data[[study_arm]] %in% already_randomized_value),
]


### If stratification occurs first, we must select only the already-randomized participants
### who are in the same stratum as the new participant.
if(!is.null(stratification_variables)){
  
  svs <- sapply(
    stratification_variables, 
    FUN = function(x) new_to_randomize[[x]])
  
  stratification_levels <- as.data.frame(t(as.matrix(svs)))
  
  arand <- already_randomized 
  for(vv in stratification_variables){
    arand <- arand[arand[[vv]] == stratification_levels[[vv]], ]
  }
  
  already_randomized <- arand
}
# write.csv(new_to_randomize, "ntr.csv")
# write.csv(already_randomized, "ar.csv")


# If we need to randomize a new participant, run the randomization.R script.
if(nrow(new_to_randomize) > 0){
  write(
    paste(
      "-----------------------------------------------------\n",
      Sys.time(), "\n",
      "There are",
      nrow(new_to_randomize),
      "units to randomize. Running randomization script.\n\n"
    ),
    "out.txt", 
    append = TRUE
  )

  source("randomization.R")
  
} else {
  
  write(
    paste(
      "-----------------------------------------------------\n",
      Sys.time(), "\n",
      "There are no eligible units to randomize:\n",
      sum(is.na(new_obs[[study_arm]])),
      "recently modified record has yet to be randomized.\n",
      sum(is.na(new_obs[[randomization_ready]])),
      "recently modified records are indicated as 'Ready to randomize'.\n\n"
    ),
    "out.txt", 
    append = TRUE
  )
  
}
