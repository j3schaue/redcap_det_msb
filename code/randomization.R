###------------------------------------------------------------------------------------###
###------------------------------------------------------------------------------------###
###------------------------------------------------------------------------------------###
### Notes
### This script is triggered by parse_payload.R
###    parse_payload.R passes the following arguments:
###    new_to_randomize: Tibble of baseline data on units to randomize
###    already_randomized: Tibble of baseline data on units already randomized
###    bal_covariates: list of strings indicating variable names for balance
###    study_center: string indicating variable name for center.
###    study_arm: string indicating variable name for study arm.
###    min_n_algorithm: interger Sample size required before starting adaptive randomization
###------------------------------------------------------------------------------------###
###------------------------------------------------------------------------------------###
###------------------------------------------------------------------------------------###

source("apply_msb.R")
# write("sourced main code", "srcmvall.txt")

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
  min_n_adapt = min_n_algorithm, 
  diff_cutoff = d_cutoff,
  p_criterion = use_p_cutoff, 
  show_votes = T
)

# Generate new random assignment
new_arm <- generate_assignment(prob$prob, already_randomized_value)[[1]]


###---------------------------###
### Update REDCap
###---------------------------###

# Document assignment arm for new participants
ntr <- new_to_randomize 
ntr$arm = new_arm

# If documenting notes about randomization process (recommended)
#   format the randomization probability and votes
if(notes){
  
  # Generate a note documenting votes to include in REDCap
  rand_vote <- paste(
    "Record", pl$record, "assignment = ", new_arm, "\n", Sys.Date(), "\n",
    paste(row.names(prob$votes), prob$votes[,1], sep = ": ", collapse = "\n"), 
    "\n",
    "Majority: ", prob$majority
  )
  
  # Extract the randomization probability
  rand_prob <- prob$prob
    
  # If for some reason the vote or probability are a list, collapse them
  if(length(rand_vote) > 1){
    rand_vote <- paste(rand_vote, collapse = "\n")
  }
  
  if(length(rand_prob) > 1){
    rand_prob <- paste(rand_prob, collapse = ", ")
  }
  
  
  # Add the votes and probability to the new record
  ntr[[vote_name]] <- rand_vote 
  ntr[[prob_name]] <- rand_prob
}

# Document new randomization on remote server (helps to debug issues that pop up)
write.csv(ntr, "data/randomized_patient.csv", append = TRUE)

# Document randomization note on text file on remote server
write(rand_vote, "data/new_randomization.txt", append = TRUE)


# Format new observations for import into REDCap via API
ntr_import <- ntr 
ntr_import$redcap_event_name = baseline_event


# Import new randomization
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
    "Updated RCDB with a new randomization.\n",
    "Record(s):", ntr$record_id, 
    "\n\n"
  ),
  "success.txt", 
  append = TRUE
)
