###------------------------------------###
###---USER-MADE VARIABLE NAMES---------###
###------------------------------------###
# Edit the following variables to tailor
# code to specific redcap project

# Column name indicating study arm
# We know that a participant is randomized if
#     study_arm %in% already_randomized_val
study_arm = "arm" 
already_randomized_value = c()


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
  "age", "race", "gender", "disease_status"
)
study_center = "center" # Used for multi-center trials


# REDCap API token and relevant URL
token = ""
URL <- "https://redcap.nubic.northwestern.edu/redcap/api/"


# Sample size required before starting adaptive randomization
min_n_algorithm <- 100 


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
args <- commandArgs(trailingOnly = TRUE)
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

saveRDS(data, paste0("data/redcap_db_", Sys.Date(), ".RDS"))


###---------------------------###
### Get baseline data
###---------------------------###
baseline_data <- data %>%
  filter(
    redcap_event_name == baseline_event,
    # consent == "I consent to be in this study." # NEED TO EDIT FOR DIFFERENT PROJECTS
  ) %>%
  # USER UPDATE: Some MSB data may need to be pre-processed. This can be done below.
  # If pre-processing is not needed, delete the following lines.
  mutate( 
    race = factor(
      ifelse(race___2 == "Checked",
             "Asian",
             ifelse(race___3 == "Checked",
                    "Black",
                    ifelse(race___5 == "Checked",
                           "White",
                           "Other")))
    ),
    age = difftime(Sys.Date(), as.Date(dob), units = "weeks")/52.14,
    gender = factor(gender),
    disease_status = factor(disease_status)
  )



###---------------------------###
### Separate by new/old units
###---------------------------###

###---New units are those that trigger the DET
new_obs <- baseline_data %>%
  filter(record_id %in% pl$record)

old_obs <- baseline_data %>%
  filter(!(record_id %in% pl$record))

# write.csv(new_obs, paste0("new_obs_", Sys.Date(), ".csv"))
# write.csv(old_obs, paste0("old_obs_", Sys.Date(), ".csv"))

###---------------------------###
### Check if randomization
### needs to occur
###---------------------------###
new_to_randomize <- new_obs %>%
  filter(
    is.na(get(study_arm)),
    !!randomization_ready == randomization_ready_val
  )

already_randomized <- data %>%
  filter(!!study_arm  %in% already_randomized_value)

# write.csv(new_to_randomize, "ntr.csv")
# write.csv(already_randomized, "ar.csv")


# If we need to randomize a new participant, run the randomization.R script.
if(nrow(new_to_randomize) > 0){
  write(
    paste(
      "There are",
      nrow(new_to_randomize),
      "units to randomize. Running randomization script."
    ),
    "out.txt"
  )
  # write(getwd(), "wd.txt")
  source("randomization.R")
  
} else {
  
  write(
    paste(
      "There are no eligible units to randomize:\n",
      new_obs %>% pull(get(study_arm)) %>% is.na() %>% sum(),
      "recently modified record has yet to be randomized.\n",
      new_obs %>% pull(get(randomization_ready)) %>% is.na() %>% sum(),
      "recently modified records are indicated as 'Ready to randomize'."
    ),
    "out.txt")
}
