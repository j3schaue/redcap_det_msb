source("./code/simulations/sim_helper.R")
source("./code/user_specified_variables.R")
N <- 200 # total sample size
nsims <- 500 # no. of simulations

covariates <- c("age", "rand_race", "gender", "disease_status")
covariate_values <- list(
  age = c("continuous", 50, 20), 
  rand_race = c(1, 2, 5, 666, 999), 
  gender = c(1, 2, 3, 666),
  disease_status = c(0, 1, 2)
)
probs <- list(
  rand_race = c(.15, .25, .4, .2, 0),
  gender = c(.485, .485, .03, 0), 
  disease_status = c(.2, .4, .4)
)

dfs <- list(); bal <- list()
for(i in 1:nsims){
  
  tmp <- lapply(1:N, FUN = function(x){
    generate_pt(
      x, 
      covariates = covariates, 
      covariate_values = covariate_values, 
      correlation = .3, 
      probs = probs
    )
  }) %>% 
    bind_rows() %>%
    mutate(
      age = abs(age),
      rand_race = factor(rand_race),
      gender = factor(gender),
      disease_status = factor(disease_status)
      )
  
  tmp$arm <- 0
  arm1 <- sample(1:nrow(tmp), nrow(tmp)/2)
  tmp$arm[arm1] <- 1
  dfs[[i]] <- tmp
  
  comps <- list(
    race = chisq.test(tmp$arm, tmp$rand_race),
    gender = chisq.test(tmp$arm, tmp$gender),
    disease = chisq.test(tmp$arm, tmp$disease_status),
    age = lm(age ~ arm, tmp)
  )  
  
  bal[[i]] <- comps
  
}


res <- lapply(1:nsims, 
       FUN = function(i){
         tibble(
           p_race = bal[[i]]$race$p.value,
           p_gender = bal[[i]]$gender$p.value,
           p_disease = bal[[i]]$disease$p.value,
           d_age = bal[[i]]$age$coefficients[["arm"]],
           p_age = summary(bal[[i]]$age)$coefficients["arm", "Pr(>|t|)"]
         )
       }
       ) %>% 
  bind_rows()


summary(res)
res %>%
  filter(
    p_race < 0.05 | p_gender < 0.05 | p_disease < 0.05 | p_age < .05
  ) %>% 
  nrow() / nrow(res)

ggplot(res, aes(d_age)) + geom_histogram() + geom_vline(xintercept = c(-5, 5))
