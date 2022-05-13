###------------------------------------###
###---USER-MADE VARIABLE NAMES---------###
###------------------------------------###
# Edit the following variables to tailor
# code to specific redcap project

study_arm = "arm" # column name indicating study arm
randomization_ready = "randomize"
baseline_event = "Baseline"
bal_covariates = c(
  # List covariate column names used to
  # compute balance
  "age", "race", "gender", "disease_status"
)
study_center = "center"

min_n_algorithm <- 100 # Sample size required before starting adaptive randomization

###------------------------------------###
###------------------------------------###


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

write.csv(pl, "args.csv")



###---------------------------###
### Read in REDCap data
###---------------------------###
token = TOKEN
URL <- "https://redcap.nubic.northwestern.edu/redcap/api/"

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
    consent == "I consent to be in this study." # NEED TO EDIT FOR DIFFERENT PROJECTS
  ) %>%
  mutate( # NEED TO EDIT FOR DIFFERENT PROJECTS
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

write.csv(new_obs, paste0("new_obs_", Sys.Date(), ".csv"))
# write.csv(old_obs, paste0("old_obs_", Sys.Date(), ".csv"))

###---------------------------###
### Check if randomization
### needs to occur
###---------------------------###
new_to_randomize <- new_obs %>%
  filter(
    is.na(get(study_arm)),
    randomize == "Yes"
  )

already_randomized <- data %>%
  filter(!is.na(get(study_arm)))

write.csv(new_to_randomize, "ntr.csv")
write.csv(already_randomized, "ar.csv")

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
