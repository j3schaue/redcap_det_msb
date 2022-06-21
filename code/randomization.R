ource("source_msb.R")
write("sourced main code", "srcmvall.txt")

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
if(nrow(already_randomized) == 0){already_randomized = tibble()}

prob <- get_votes(
  data = already_randomized,
  new_data = new_to_randomize,
  center = study_center,
  covariates = bal_covariates,
  treatment = study_arm,
  min_n_adapt = min_n_algorithm
)

new_arm <- generate_assignment(prob)
rand_note <- paste("Record", pl$record, "assignment = ", new_arm, "probability =", prob, Sys.Date())
write(rand_note, "new_randomization.txt", append = TRUE)



###---------------------------###
### Update REDCap
###---------------------------###

ntr <- new_to_randomize %>% mutate(arm = new_arm, randomization_notes = rand_note)
# write.csv(ntr, "randomized_patient.csv")


rc_con <- redcapConnection(
  url = "https://redcap.nubic.northwestern.edu/redcap/api/",
  token = TOKEN, # Set API token as global variable
  conn = con,
  project = "7091",
  config = httr::config()
)

importRecords(
  rc_con,
  data = ntr %>% mutate(redcap_event_name = "baseline_arm_1") %>% select(-race, -age),
  overwriteBehavior = "normal",
  returnContent = "count"
)

write(
  paste(
    "updated RCDB with a new randomization.",
    "\nRecord:", ntr$record_id
  ),
  "success.txt"
)
