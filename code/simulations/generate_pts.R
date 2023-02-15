names(tmp)

generate_new_pt <- function(record_id, elig_prob = .9, corr = 0){
  edate <- format(Sys.Date(), "%m-%d-%Y")
  center <- sample(1:10, 1)
  consent <- 1
  consent_date <- format(Sys.Date(), "%m-%d-%Y")
  pname <- strsplit(randomNames::randomNames(1), ", ")[[1]]
  fname <- pname[2]; lname = pname[1]
  # consent_complete <- 2
  # initiation_complete <- 2
  
  over18 <- rbinom(1, 1, elig_prob)
  if(over18 == 0){
    guardian = rbinom(1, 1, elig_prob)
  } else {
    guardian = NA
  }
  english <- rbinom(1, 1, elig_prob)
  make_fup <- rbinom(1, 1, elig_prob)
  moon <- rbinom(1, 1, elig_prob)
  memory <- rbinom(1, 1, elig_prob)
  clothes <- rbinom(1, 1, elig_prob)
  villagers <- rbinom(1, 1, elig_prob)
  meat <- rbinom(1, 1, elig_prob)
  eyes <- rbinom(1, 1, elig_prob)
  # eligibility_complete <- 2
  
  gender <- sample(1:3, 1, prob = c(.495, .495, .1))
  dob <- as.Date(paste0(sample(1:12, 1), "-", sample(1:28, 1), "-", sample(1800:2002, 1)), format = "%m-%d-%Y")
  disease_status <- sample(1:3, 1)
  race_m <- rbinom(1, 1, .1)
  for(x in c(1:5, 666, 777)){
    assign(paste0("race___", x), 0)
  }
  if(race_m == 1){
    racecats <- sample(c(1:5, 666, 777), sample(2:3, 1), prob = c(.05, .13, .2, .02, .5, .05, .05))
    for(j in racecats){
      assign(paste0("race", "___", j), 1)
    }
  } else {
    racecat <- sample(c(1:5, 666, 777), 1, prob = c(.05, .13, .2, .02, .5, .05, .05))
    assign(paste0("race", "___", racecat), 1)
  }
  # intake_complete <- 2

  new_pt <- tibble(
    record_id = record_id,
    date = edate,
    center = center,
    initiation_complete = 2,
    consent = consent,
    consent_date = consent_date,
    fname = fname,
    lname = lname, 
    consent_complete = 2,
    over18 = over18, 
    english = english,
    make_fup = make_fup,
    guardian = guardian,
    moon = moon,
    memory = memory,
    clothes = clothes,
    villagers = villagers,
    meat = meat,
    eyes = eyes,
    eligibility_complete = 2,
    gender = gender,
    dob = dob,
    disease_status = disease_status,
    race___1 = race___1,
    race___2 = race___2,
    race___3 = race___3,
    race___4 = race___4,
    race___5 = race___5,
    race___666 = race___666,
    race___777 = race___777,
    intake_complete = 2
  )
  
  return(new_pt)
}


complete_randomization_form <- function(data){
  out <- data %>%
    mutate(
      rev_elg = 1,
      rev_con = 1, 
      rev_base = 1,
      age = (Sys.Date() - dob) / 7,
      rand_race = ifelse(race___3 == 1, 3, 
                         ifelse(race___2 == 1, 2, 
                                ifelse(race___5 == 1, 5, 666))),
      rev_calc_correct = 1,
      randomize = 1
    )
  
  return(out)
}