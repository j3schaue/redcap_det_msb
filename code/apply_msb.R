###---------------------------------------------------###
###---------------------------------------------------###
###---------------------------------------------------###
### Minimal Sufficient Balance
### Adaptive Randomization
###---------------------------------------------------###
###---------------------------------------------------###
###---------------------------------------------------###

###---------------------------------------------------###
### Libraries
###---------------------------------------------------###
#library(tidyverse)

###---------------------------------------------------###
### Functions to compute balance
###---------------------------------------------------###

#' @name get_balance_cts
#' @param data data.frame containing existing randomized participants
#' @param new_data data.frame containing data on a new participant who is ready to be randomized
#' @param treatment string indicating which column in 'data' tracks the treatment assignment
#' @param variable string indicating on which column to compute balance
#' @param p_cutoff numeric (positive (0, 1)) indicates the p-value of the test for covariate balance required for MSB intervention 
#' @value list with two entries: 'results', a DF containing balance metrics for 'variable' in 'data' and 'vote', which indicates vote for MSB allocation.
get_balance_cts <- function(
    data,
    new_data,
    treatment, 
    variable,
    p_cutoff = 0.3,
    diff_cutoff = 0.1,
    p_criterion = TRUE
){
  
  # Value for new participant
  new_val <- new_data[[variable]]
  
  # Compute balance using linear model (computes t-test)
  # Assumes Control/Placebo is reference group
  mod <- lm(data[[variable]] ~ data[[treatment]])
  res <- summary(mod)$coefficients
  
  # Formulate output of balance metric as tibble
  out <- data.frame(
    var = variable, # variable name
    diff = res[2, "Estimate"], # Difference
    smd = res[2, "Estimate"]/summary(mod)$sigma,
    ctrlmn = res[1, "Estimate"], 
    trtmn = sum(res[1:2, "Estimate"]),
    se = res[2, "Std. Error"], # SE of difference
    stat = res[2, "t value"], # test statistic
    p = res[2, "Pr(>|t|)"] # p-value
  )
  
  # Compute whether new patient gets voted to arm 0 or arm 1
  if(p_criterion){
    v1 <- ((out$p < p_cutoff) & (out$diff < 0) & (new_val > out$ctrlmn)) |  # Trt < Ctrl & new_val > ctrl mean
      ((out$p < p_cutoff) & (out$diff > 0) & (new_val < out$ctrlmn)) # Trt > Ctrl & new_val < ctrl mean
    v0 <- ((out$p < p_cutoff) & (out$diff > 0) & (new_val > out$trtmn)) | # Trt > Ctrl & new_val > trt mean
      ((out$p < p_cutoff) & (out$diff < 0) & (new_val < out$trtmn)) # Trt < Ctrl & new_val < trt mean
  } else {
    v1 <- ((abs(out$smd) > diff_cutoff) & (out$diff < 0) & (new_val > out$ctrlmn)) |  # Trt < Ctrl & new_val > ctrl mean
      ((abs(out$smd) > diff_cutoff) & (out$diff > 0) & (new_val < out$ctrlmn)) # Trt > Ctrl & new_val < ctrl mean
    v0 <- ((abs(out$smd) > diff_cutoff) & (out$diff > 0) & (new_val > out$trtmn)) | # Trt > Ctrl & new_val > trt mean
      ((abs(out$smd) > diff_cutoff) & (out$diff < 0) & (new_val < out$trtmn)) # Trt < Ctrl & new_val < trt mean
  }
  
  # Compute final vote
  if(v1 & v0){
    vote <- "Neutral"
  } else if(v1) {
    vote <- "Arm 1"
  } else if(v0){
    vote <- "Arm 0" 
  } else { vote <- "Neutral" }
  
  return(
    list(
      vote = vote,
      results = out
    )
  )
}


#' @name get_balance_cat a function for computing balance on categorical variables
#' @param data data.frame containing existing randomized participants
#' @param new_data data.frame containing data on a new participant who is ready to be randomized
#' @param treatment string indicating which column in 'data' tracks the treatment assignment
#' @param variable string indicating on which column to compute balance
#' @param p_cutoff numeric (positive (0, 1)) indicates the p-value of the test for covariate balance required for MSB intervention 
#' @value list with two entries: 'results', a DF containing balance metrics for 'variable' in 'data' and 'vote', which indicates vote for MSB allocation.
get_balance_cat <- function(
    data,
    new_data,
    treatment, 
    variable,
    p_cutoff = 0.3
){
  
  # New participant value
  new_val <- new_data[[variable]]
  
  if(sum(data[[variable]] == new_val) == 0){
    
    vote = "Neutral"; out = "New level detected"
    
  } else {
    
    # Compute balance stats
    mod <- chisq.test(data[[variable]], data[[treatment]])
    
    # Format output as table
    out <- list(
      var = variable,
      observed = list(mod$observed),
      expected = list(mod$expected),
      diff = list(mod$observed - mod$expected),
      stat = mod$statistic,
      p = mod$p.value
    )
    
    nv <- paste(new_val)
    # Compute and return vote
    v0 <- (out$p < p_cutoff & out$expected[[1]][nv, 1] > out$observed[[1]][nv, 1])
    v1 <- (out$p < p_cutoff & out$expected[[1]][nv, 2] > out$observed[[1]][nv, 2])
    
    # Compute final vote
    if(v1 & v0){
      vote <- "Neutral"
    } else if(v1) {
      vote <- "Arm 1"
    } else if(v0){
      vote <- "Arm 0" 
    } else { vote <- "Neutral" }
    
  }
  return(
    list(
      vote = vote,
      results = out
    )
  )
}


#' @name get_balance_center a function to compute within-center balance of random assignment in a multicenter (block randomized) trial
#' @param data data.frame containing existing randomized participants
#' @param new_data data.frame containing data on a new participant who is ready to be randomized
#' @param treatment string indicating which column in 'data' tracks the treatment assignment
#' @param center string indicating which variable in 'data' and 'new_data' indicates the center 
#' @param p_cutoff numeric (positive (0, 1)) indicates the p-value of the test for covariate balance required for MSB intervention 
#' @value list with two entries: 'results', a DF containing balance metrics for 'variable' in 'data' and 'vote', which indicates vote for MSB allocation.
get_balance_center <- function(
    data, 
    new_data,
    treatment,
    center,
    p_cutoff = 0.3
){
  
  new_center <- new_data[[center]]
  dat <- data[which(data[[center]] == new_center), ]
  
  if(nrow(dat) < 2){
    
    bt = NULL
    vote = "Neutral"
    
  } else {
    
    dc_tmp <- table(dat[[treatment]])
    dc <- data.frame(
      trt = as.numeric(names(dc_tmp)), 
      n = as.vector(dc_tmp)
    )
    
    # Check that there are at least 3 participants 
    # in each arm in the new center
    if(sum(dc$n > 2) == nrow(dc)){
      
      # Get p-value
      bt <- binom.test(dc$n, dc$trt)
      
      # Compute and return vote for arm
      v0 <- as.logical((bt$p.value < p_cutoff) & (dc$trt[1]/dc$n[1] < 0.5))
      v1 <- ((bt$p.value < p_cutoff) & (dc$trt[2]/dc$n[2] > 0.5))
      vote <- ifelse(v0, "Arm 0",
                     ifelse(v1, "Arm 1", "Neutral"))
      
    } else {
      
      bt = NULL
      vote = "Neutral"
      
    }
  }
  
  return(
    list(
      results = bt,
      vote = vote
    )
  )
}


#' @name get_votes
#' @param center string indicating variable name for center
#' @param covariates list of strings indicating variables on which to conduct balance
#' @param treamtent string indicating treatment assignment variable
#' @param min_n_adapt integer indicating # of already randomized individuals required to run adaptive algorithm
#' @param prob_vote probability of assigning to treatment if it would improve balance
#' @return treatment assignment probability
#' @note 1 = treatment
get_votes <- function(data, new_data, # data
                      center = NULL, covariates, treatment, # variable names
                      min_n_adapt = 10,
                      prob_vote = 0.7, # probability split for majority arm
                      p_cutoff = 0.3, 
                      diff_cutoff = 0.1,
                      p_criterion = TRUE,
                      show_votes = F){ # output options
  
  
  if(nrow(data) < min_n_adapt){
    
    prob = .5
    total_vote = NULL
    majority = NULL
    
  } else {
    
    overall_vote <- list()
    center_vote <- list()
    covariate_vote <- list()
    
    dtrt <- mean(data[[treatment]], na.rm = T)
    
    vote <- ifelse(dtrt > 0.5, "Arm 0",
                   ifelse(dtrt < 0.5, "Arm 1", "Neutral"))
    
    overall_vote[["overall"]] <- vote
    
    if(!is.null(center)){
      
      center_info <- get_balance_center(data, new_data, treatment, center, p_cutoff)
      
    } else {
      
      center_info <- list(results = NULL, vote = "Neutral")
      
    }
    
    for(j in covariates){
      
      if(nrow(data) == 1){
        
        vote <- "Neutral"
        
      } else if(is.numeric(data[[j]])){
        
        if(nrow(data) <= 4 | length(unique(data[[treatment]])) == 1){
          
          vote <- "Neutral"
          bal_results <- NULL
          
        } else {
          
          var_info <- get_balance_cts(
            data, 
            new_data, 
            treatment, 
            j, 
            p_cutoff,
            diff_cutoff, 
            p_criterion
          )
          vote <- var_info$vote
          bal_results <- var_info$results
          
        }
        
      } else if(is.factor(data[[j]])){
        
        if((length(unique(data[[j]])) == 1) |
           (length(unique(data[[treatment]])) == 1)){
          
          vote <- "Neutral"
          bal_results <- NULL
          
        } else {
          
          var_info <- get_balance_cat(data, new_data, treatment, j, p_cutoff)
          vote <- var_info$vote
          bal_results <- var_info$results
          
        }
        
      }
      
      covariate_vote[[paste(j)]] <- vote
      
    }
    
    total_vote <- t(as.data.frame(c(overall_vote, center_vote, covariate_vote)))
    #return(t(as.data.frame(total_vote)))
    
    vote_tab <- as.data.frame(table(total_vote))
    
    vt <- vote_tab[which(vote_tab[["total_vote"]] != "Neutral"), ]
    
    if(nrow(vt) == 0){
      
      majority = NULL
      
    } else {
      
      majority <- as.character(vt$total_vote[vote_tab$Freq == max(vt$Freq)])
      majority <- majority[!is.na(majority)]
      
    }
    
    
    if(is.null(majority) | length(majority) > 1){
      prob <- 0.5
    } else if(majority == "Neutral"){
      prob <- 0.5
    } else if(majority == "Arm 1"){
      prob <- prob_vote
    } else {
      prob <- 1 - prob_vote
    }
    
  }
  
  if(show_votes){
    
    return(
      list(
        prob = prob,
        votes = total_vote,
        majority = majority
      )
    )
    
  } else {
    
    return(prob)
    
  }
  
}



#' @name generate_assignment
#' @param prob numeric [0,1]: Probability of assignment to arm 1 (treatment)
#' @return treatment assigment as integer
#' @note 1 = treatment
generate_assignment <- function(prob){
  return(
    rbinom(1, 1, prob)
  )
}



##------------------------------------------------##
## Old/deprecated/rewritten functions
##------------------------------------------------##


# #' @name get_votes_old deprecated
# #' @param center string indicating variable name for center
# #' @param covariates list of strings indicating variables on which to conduct balance
# #' @param treamtent string indicating treatment assignment variable
# #' @param min_n_adapt integer indicating # of already randomized individuals required to run adaptive algorithm
# #' @param prob_vote probability of assigning to treatment if it would improve balance
# #' @return treatment assignment probability
# #' @note 1 = treatment
# get_votes_old <- function(data, new_data, # data
#                       center = NULL, covariates, treatment, # variable names
#                       min_n_adapt = 10,
#                       prob_vote = 0.7, # probability split for majority arm
#                       show_votes = F){ # output options


#   if(nrow(data) < min_n_adapt){

#     prob = .5

#     if(show_votes){

#       return(
#         list(
#           prob = prob,
#           votes = NULL,
#           majority = NULL
#         )
#       )

#     } else {

#       return(prob)

#     }
#   } else {
#     overall_vote <- list()
#     center_vote <- list()
#     mean_vote <- list()

#     dtrt <- data %>%
#       summarize(pct_trt = mean(get(treatment)))

#     vote <- ifelse(dtrt > 0.5, "Arm 0",
#                    ifelse(dtrt < 0.5, "Arm 1", "Neutral"))

#     overall_vote[["overall"]] <- vote

#     new_center <- new_data %>% select(get(center)) %>% pull()

#     # Summarize assignment within center of interest
#     dc <- data %>%
#       filter(.data[[center]] == new_center) %>%
#       summarize(x = sum(get(treatment)), n = n())

#     # Check if this is a brand new center
#     # or if patients have already been
#     # randomized in this center.
#     if(dc$n == 0){

#       vote <- "Neutral"
#       center_vote[[paste("center")]] <- vote


#     } else {

#       # Get p-value
#       bt <- binom.test(dc$x, dc$n)

#       # Compute and return vote for arm
#       v0 <- (bt$p.value < 0.3 & dc$x/dc$n < 0.5)
#       v1 <- (bt$p.value < 0.3 & dc$x/dc$n > 0.5)
#       vote <- ifelse(v0, "Arm 0",
#                      ifelse(v1, "Arm 1", "Neutral"))
#       center_vote[["center"]] <- vote
#     }

#     for(j in covariates){

#       if(nrow(data) == 1){

#         vote <- "Neutral"

#       } else if(is.numeric(data[[j]])){

#         if(nrow(data) <= 4 | length(unique(data[,paste(treatment)])) == 1){

#           vote <- "Neutral"

#         } else {

#           # Compute balance using linear model
#           mod <- lm(data[[j]] ~ data[[treatment]])
#           res <- summary(mod)$coefficients

#           # Formulate output as tibble
#           out <- tibble(
#             var = j, # variable name
#             diff = res[2, "Estimate"], # Difference
#             se = res[2, "Std. Error"], # SE of difference
#             stat = res[2, "t value"], # test statistic
#             p = res[2, "Pr(>|t|)"] # p-value
#           )

#           x_sum <- data %>%
#             group_by(get(treatment)) %>%
#             summarize(x = mean(get(j), na.rm = T)) %>%
#             rename(treatment = `get(treatment)`)

#           x1 <- x_sum %>%
#             filter(treatment == 1) %>%
#             pull(x)

#           x0 <- x_sum %>%
#             filter(treatment == 0) %>%
#             pull(x)

#           new_col_name <- new_data %>% pull(get(j))

#           # Compute whether new patient gets voted to arm 0 or arm 1
#           v1 <- ((out$p < 0.3) & (out$diff > 0) & (new_col_name < x0)) |
#             ((out$p < 0.3) & (out$diff < 0) & (new_col_name > x0))
#           v0 <- ((out$p < 0.3) & (out$diff > 0) & (new_col_name < x1)) |
#             ((out$p < 0.3) & (out$diff < 0) & (new_col_name > x1))

#           # Return vote
#           vote <- ifelse(v1, "Arm 1", ifelse(v0, "Arm 0", "Neutral"))

#         }

#       } else if(is.factor(data[[j]])){

#         if((length(unique(data[[j]])) == 1) |
#            (length(unique(data[[treatment]])) == 1)){

#           vote <- "Neutral"

#         } else {

#           mod <- chisq.test(data[[treatment]], data[[j]])

#           # Format output as table
#           out <- tibble(
#             var = j,
#             observed = list(mod$observed),
#             expected = list(mod$expected),
#             diff = list(mod$observed - mod$expected),
#             stat = mod$statistic,
#             p = mod$p.value)

#           new_col_name <- new_data %>% select(get(j)) %>% pull()

#           # Compute and return vote
#           v0 <- (out$p < 0.3 & out$expected[[1]][new_col_name, 1] > out$observed[[1]][new_col_name, 1])
#           v1 <- (out$p < 0.3 & out$expected[[1]][new_col_name, 2] > out$observed[[1]][new_col_name, 2])
#           vote <- ifelse(v1, "Arm 1", ifelse(v0, "Arm 0", "Neutral"))

#         }

#       }

#       mean_vote[[paste(j)]] <- vote

#     }

#     total_vote <- t(as.data.frame(c(overall_vote, center_vote, mean_vote)))
#     #return(t(as.data.frame(total_vote)))

#     vote_tab <- as.data.frame(table(total_vote))

#     vt <- vote_tab %>% filter(total_vote != "Neutral")

#     if(nrow(vt) == 0){

#       majority = NULL

#     } else {

#       majority <- as.character(vt$total_vote[vote_tab$Freq == max(vt$Freq)])
#       majority <- majority[!is.na(majority)]

#     }


#     if(is.null(majority) | length(majority) > 1){
#       prob <- 0.5
#     } else if(majority == "Neutral"){
#       prob <- 0.5
#     } else if(majority == "Arm 1"){
#       prob <- prob_vote
#     } else {
#       prob <- 1 - prob_vote
#     }

#     if(show_votes){

#       return(
#         list(
#           prob = prob,
#           votes = total_vote,
#           majority = majority
#         )
#       )

#     } else {

#       return(prob)

#     }
#   }

# }

# prob <- get_votes(center, covariates, treatment, data, new_data)


