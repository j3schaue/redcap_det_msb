source("apply_msb.R")
# write("sourced main code", "srcmvall.txt")

## Notes
# This script is triggered by parse_payload.R
# parse_payload.R passes the following arguments:
#   new_to_randomize: Tibble of baseline data on units to randomize
#   already_randomized: Tibble of baseline data on units already randomized
#   bal_covariates: list of strings indicating variable names for balance
#   study_center: string indicating variable name for center.
#   study_arm: string indicating variable name for study arm.
#   min_n_algorithm: interger Sample size required before starting adaptive randomization


###---------------------------###
### Randomize
###---------------------------###
if(nrow(already_randomized) == 0){ already_randomized = data.frame() }

# Get the randomization probability based on MSB algorithm
prob <- get_votes(
  data = already_randomized,
  new_data = new_to_randomize,
  center = study_center,
  covariates = bal_covariates,
  treatment = study_arm,
  min_n_adapt = min_n_algorithm
)

# Generate new random assignment
new_arm <- generate_assignment(prob)


###---------------------------###
### Update REDCap
###---------------------------###

# Document assignment arm for new participants
ntr <- new_to_randomize 
ntr$arm = new_arm

if(notes){
  
  # Generate a note to include in REDCap (if using)
  rand_note <- paste("Record", pl$record, "assignment = ", new_arm, "probability =", prob, Sys.Date())
  if(length(rand_note) > 1){
    rand_note <- paste(rand_note, collapse = "\n")
  }
  
  ntr[[note_name]] = rand_note # include randomization note
}

# Document new randomization on remote server
write.csv(ntr, "randomized_patient.csv", append = TRUE)

# Document randomization note on text file on remote server
write(rand_note, "new_randomization.txt", append = TRUE)


# Format new observations for import into REDCap via API
ntr_import <- ntr 
ntr_import$redcap_event_name = baseline_event

# Establish REDCap connection
rc_con <- redcapConnection(
  url = URL,
  token = TOKEN, # Set API token as global variable
  conn = con,
  project = PID,
  config = httr::config()
)

# Import new randomizations
importRecords(
  rc_con,
  data = ntr_import,
  overwriteBehavior = "normal",
  returnContent = "count"
)

# Write success message to server to track randomizations
write(
  paste(
    "-----------------------------------------------------\n",
    Sys.time(), "\n",
    " Updated RCDB with a new randomization.\n",
    "Record(s):", ntr$record_id, "\n\n"
  ),
  "success.txt", 
  append = TRUE
)
