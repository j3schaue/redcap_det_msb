###--------------------------------------###
### Simulation support code:
###
### This script contains code for generating
### synthetic data (patients) to simulate
### the impact of MSB vs. individual
### randomization.
###
### Patients are generating according to
### a fake study in REDCap.
###--------------------------------------###


generate_pt <- function(
    record_id, 
    center = NULL,
    elig_prob = 1, 
    correlation = 0, 
    covariates, 
    covariate_values, 
    probs = NULL){
  
  if(is.null(center)){
    center <- sample(1:10, 1)
  } else if (center > 10 | center < 1) {
    center <- sample(1:10, 1)
  }
  
  n_covariates <- length(covariates)
  Sigma <- diag(1, n_covariates)
  Sigma[outer(1:n_covariates, 1:n_covariates, function(i,j) i!=j)] <- correlation
  cov_draw <- lapply(MASS::mvrnorm(1, mu = rep(0, n_covariates), Sigma), FUN = function(x) x)
  names(cov_draw) <- covariates
  
  covs <- lapply(covariates, FUN = function(vv){
    if(length(covariate_values[[vv]]) == 3 & covariate_values[[vv]][1] == "continuous"){
      
        return(
          as.numeric(covariate_values[[vv]][3]) * cov_draw[[vv]] + 
            as.numeric(covariate_values[[vv]][2])
        )
      
      } else {
        
          if(is.null(probs)){
            
            n_levs <- length(covariate_values[[vv]])
            ind <- findInterval(cov_draw[[vv]], qnorm(cumsum(rep(1/n_levs, n_levs)))) + 1
        
          } else {
            
            ind <- findInterval(cov_draw[[vv]], qnorm(cumsum(probs[[vv]]))) + 1
          }
        
        return(covariate_values[[vv]][ind])
        
      }
      
    }
    )
  
  names(covs) <- covariates
  
  
  out <- tibble(
    record_id = record_id,
    date = Sys.Date(),
    initiation_complete = 2,
    ##--------------------------------------##
    over18 = rbinom(1, 1, elig_prob),
    english = rbinom(1, 1, elig_prob),
    make_fup = rbinom(1, 1, elig_prob),
    mood = sample(c(1, 0, 999), 1, prob = c(elig_prob, (1 - elig_prob)/2, (1 - elig_prob)/2)),
    memory = sample(c(1, 0, 999), 1, prob = c(elig_prob, (1 - elig_prob)/2, (1 - elig_prob)/2)),
    clothes = sample(c(1, 0, 999), 1, prob = c(elig_prob, (1 - elig_prob)/2, (1 - elig_prob)/2)),
    villagers = sample(c(1, 0, 999), 1, prob = c(elig_prob, (1 - elig_prob)/2, (1 - elig_prob)/2)),
    meat = sample(c(1, 0, 999), 1, prob = c(elig_prob, (1 - elig_prob)/2, (1 - elig_prob)/2)),
    eyes = sample(c(1, 0, 999), 1, prob = c(elig_prob, (1 - elig_prob)/2, (1 - elig_prob)/2)),
    eligibility_complete = 2,
    ##--------------------------------------##
    consent = rbinom(1, 1, elig_prob),
    consent_date = Sys.Date(),
    consent_complete = 2
    ##--------------------------------------##
  ) %>%
    bind_cols(as.data.frame(covs)) %>%
    mutate(
      dob = Sys.Date() - 365 * age, 
      guardian = ifelse(age < 18, rbinom(1, 1, elig_prob), NA),
      intake_complete = 2
    )
  
  return(out)
}


format_for_redcap <- function(data){
  
  gnd <- sapply(data$gender, FUN = function(x) switch(x, `1` = "Male", `2` = "Female", `3` = NULL, `666` = NULL))
  names <- randomNames::randomNames(n = nrow(data), gender = gnd)
  splts <- lapply(names, FUN = function(x) strsplit(x, ", "))
  fnames <- sapply(splts, FUN = function(x) x[[1]][2])
  lnames <- sapply(splts, FUN = function(x) x[[1]][1])
  
  out <- data %>%
    mutate(
      age = NA,
      race = ifelse(rand_race %in% c(2, 3, 5), 
                    rand_race,
                    sample(c(1, 4, 666, 777), 1)
      ), 
      rand_race = NA, 
      rev_elg = 1,
      rev_con = 1, 
      rev_base = 1, 
      rev_calc_correct = 1
    )
  
  data$fname <- fnames
  data$lname <- lnames
  
  return(out)
}

format_for_sim <- function(data){
  out <- data %>%
    mutate(randomize = 1)
  
  return(out)
}