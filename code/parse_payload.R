source("user_specified_variables.R")

###---------------------------###
### Libraries
###---------------------------###
library(dplyr)
library(tidyr)
library(redcapAPI)
library(RCurl)


###---------------------------###
### Get arguments from payload
###---------------------------###
# args <- commandArgs(trailingOnly = TRUE)
pl <- data.frame(pid = args[1], record = args[2], instrument = args[3])
# write.csv(pl, "args.csv")


###---------------------------###
### Read in REDCap data
###---------------------------###
rc_db <- postForm(
  uri=URL,
  token=token,
  content='record',
  format='csv',
  type='flat',
  rawOrLabel='label',
  #rawOrLabel='raw',
  rawOrLabelHeaders='raw',
  exportCheckboxLabel='false',
  exportSurveyFields='false',
  exportDataAccessGroups='false',
  returnFormat='json'
)

con <- textConnection(rc_db)
data <- read.csv(con, stringsAsFactors = F)
data[data == ""] <- NA

# Stash a version of the REDCap database on the server just in case.
saveRDS(data, paste0("data/redcap_db.RDS"))


###---------------------------###
### Get baseline data
###---------------------------###

# Some MSB data may need to be pre-processed. This can be done below.
# If pre-processing is not needed, delete the following line.
# If addtional processing is required, you may either 
#     1. add to the lines below
#     2. modify the standardize_msb_variables function in the user_specified_variables.R script
baseline_data <- standardize_msb_variables(
  data[which(data$redcap_event_name == baseline_event), ], 
  bal_covariates, 
  bal_covariate_type
)



###---------------------------###
### Separate by new/old units
###---------------------------###

###---New units are those that trigger the DET
new_obs <- baseline_data[which(baseline_data$record_id %in% pl$record), ]

old_obs <- baseline_data[which(!(baseline_data$record_id %in% pl$record)), ]

# write.csv(new_obs, paste0("new_obs_", Sys.Date(), ".csv"))
# write.csv(old_obs, paste0("old_obs_", Sys.Date(), ".csv"))

###---------------------------###
### Check if randomization
### needs to occur
###---------------------------###
new_to_randomize <- new_obs[
  which(is.na(new_obs[[study_arm]]) & 
          new_obs[[randomization_ready]] == randomization_ready_val), 
]

already_randomized <- old_obs[
  which(old_obs[[study_arm]] %in% already_randomized_value),
]

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
  # write(getwd(), "wd.txt")
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
