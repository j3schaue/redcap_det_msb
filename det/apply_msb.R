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
#' @param rd_cutoff numeric (positive (0, 1)) indicates the imbalance on the scale of the absolute risk difference in a given category for MSB intervention.
#' @param p_criterion boolean indicates whether to use p-value or risk difference for MSB intervention criteria
#' @value list with two entries: 'results', a DF containing balance metrics for 'variable' in 'data' and 'vote', which indicates vote for MSB allocation.
get_balance_cat <- function(
    data,
    new_data,
    treatment, 
    variable,
    p_cutoff = 0.3,
    rd_cutoff = 0.1,
    p_criterion = TRUE
){
  
  # New participant value
  new_val <- new_data[[variable]]
  
  if(sum(data[[variable]] == new_val) == 0){
    
    vote = "Neutral"; out = "New level detected"
    
  } else {
    
    
    if(p_criterion){
      
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
      
    } else {
      
      # Imbalance computed by risk difference on category of new participant.
      mod <- lm(data[[variable]] == new_val ~ data[[treatment]])
      res <- summary(mod)$coefficients
      
      out <- list(
        var = variable, 
        diff = res["data[[treatment]]", "Estimate"],
        ctrl = res["(Intercept)", "Estimate"]
      )
      out$trt <- out$ctrl + out$diff
      
      
      # Vote for control if there is a higher proportion in Trt vs. Ctrl (to increase ctrl proportion)
      v0 <- ((abs(out$diff) > rd_cutoff) & (out$diff > 0))
      v1 <- ((abs(out$diff) > rd_cutoff) & (out$diff < 0))
      
    }
    
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
      bt <- binom.test(dc$n)
      
      # Compute and return vote for arm
      v0 <- as.logical((bt$p.value < p_cutoff) & (dc$n[dc$trt==0]/sum(dc$n) < 0.5))
      v1 <- ((bt$p.value < p_cutoff) & (dc$n[dc$trt==1]/sum(dc$n) < 0.5))
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
#' @param p_cutoff numeric (0, 1) indicating level of tests for MSB
#' @param diff_cutoff numeric (positive) indicating the largest magnitude difference in continous variables deemed negligible; on the scale of Cohen's d
#' @param rd_cutoff numeric (0, 1) indicating the largest magnitude difference in binary/categorical variables deemed negligible; on the scale of risk difference
#' @param p_criterion boolean indicating if the p-value of a test or an absolute difference (diff_cutoff/rd_cutoff) should be used
#' @return treatment assignment probability
#' @note 1 = treatment
get_votes <- function(data, new_data, # data
                      center = NULL, covariates, treatment, # variable names
                      min_n_adapt = 10,
                      prob_vote = 0.7, # probability split for majority arm
                      p_cutoff = 0.3, 
                      diff_cutoff = 0.1,
                      rd_cutoff = 0.1,
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
          
          var_info <- get_balance_cat(
            data, 
            new_data, 
            treatment, 
            j, 
            p_cutoff, 
            rd_cutoff, 
            p_criterion
          )
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
generate_assignment <- function(prob, study_arms){
  return(
    sample(study_arms, 1, prob = c(1 - prob, prob))[[1]]
  )
}



