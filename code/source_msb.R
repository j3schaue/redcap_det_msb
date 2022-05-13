Last login: Mon May  2 13:54:55 on ttys000
jms192@FSMC02D91WMML85 ~ % ssh deploy@vtfsmprevmedredcap1.fsm.northwestern.edu
Enter passphrase for key '/Users/jms192/.ssh/id_rsa': 
  Last login: Thu Apr 21 16:35:01 2022 from pa-dhcp-10-120-14-30.gp-vpn-fsm-it-dev.northwestern.private
deploy@vtfsmprevmedredcap1 staging ~ $ cd ../../var/www/apps/detcollect/
  deploy@vtfsmprevmedredcap1 staging /var/www/apps/detcollect $ ls -l
total 264
-rw-r--r-- 1 deploy nginx   3650 Oct 18  2021 404.html
-rw-r--r-- 1 deploy nginx   3693 Oct 18  2021 50x.html
-rw-r--r-- 1 deploy nginx  87605 Apr 28 16:44 ar.csv
-rw-r--r-- 1 deploy nginx     64 Apr 28 16:44 args.csv
-rw-r--r-- 1 deploy nginx    574 Mar 25 15:50 index.php
-rw-r--r-- 1 deploy nginx    960 Mar 31 17:27 new_obs_2022-03-31.csv
-rw-r--r-- 1 deploy nginx   1024 Apr  4 18:32 new_obs_2022-04-04.csv
-rw-r--r-- 1 deploy nginx   1022 Apr  5 11:30 new_obs_2022-04-05.csv
-rw-r--r-- 1 deploy nginx   1013 Apr  6 16:32 new_obs_2022-04-06.csv
-rw-r--r-- 1 deploy nginx    599 Apr  8 09:21 new_obs_2022-04-08.csv
-rw-r--r-- 1 deploy nginx   1013 Apr 13 14:13 new_obs_2022-04-13.csv
-rw-r--r-- 1 deploy nginx   1017 Apr 14 11:34 new_obs_2022-04-14.csv
-rw-r--r-- 1 deploy nginx   1022 Apr 19 11:42 new_obs_2022-04-19.csv
-rw-r--r-- 1 deploy nginx   1018 Apr 21 16:33 new_obs_2022-04-21.csv
-rw-r--r-- 1 deploy nginx    996 Apr 28 16:44 new_obs_2022-04-28.csv
-rw-r--r-- 1 deploy nginx  10544 Apr 28 16:44 new_randomization.txt
-rw-r--r-- 1 deploy nginx    996 Apr 28 16:44 ntr.csv
-rw-r--r-- 1 deploy nginx     62 Apr 28 16:44 out.txt
-rw-rw-r-- 1 deploy deploy  3815 Apr  4 16:30 parse_payload.R
-rw-rw-r-- 1 deploy deploy  1856 Apr  4 18:32 randomization.R
-rw-r--r-- 1 deploy nginx   1050 Apr 28 16:44 randomized_patient.csv
-rw-r--r-- 1 deploy nginx    922 Mar 31 17:27 redcap_db_2022-03-31.RDS
-rw-r--r-- 1 deploy nginx   3646 Apr  4 18:32 redcap_db_2022-04-04.RDS
-rw-r--r-- 1 deploy nginx   3739 Apr  5 11:30 redcap_db_2022-04-05.RDS
-rw-r--r-- 1 deploy nginx   3826 Apr  6 16:32 redcap_db_2022-04-06.RDS
-rw-r--r-- 1 deploy nginx   3839 Apr  8 09:21 redcap_db_2022-04-08.RDS
-rw-r--r-- 1 deploy nginx   3856 Apr 13 14:13 redcap_db_2022-04-13.RDS
-rw-r--r-- 1 deploy nginx   3866 Apr 14 11:34 redcap_db_2022-04-14.RDS
-rw-r--r-- 1 deploy nginx   4143 Apr 19 11:42 redcap_db_2022-04-19.RDS
-rw-r--r-- 1 deploy nginx   4153 Apr 21 16:33 redcap_db_2022-04-21.RDS
-rw-r--r-- 1 deploy nginx   8316 Apr 28 16:44 redcap_db_2022-04-28.RDS
-rw-rw-r-- 1 deploy deploy  6641 Apr  4 16:37 source_mv.R
-rw-r--r-- 1 deploy nginx     12 Mar 31 16:44 srccond.txt
-rw-rw-r-- 1 deploy deploy    18 Apr 28 16:44 srcmvall.txt
-rw-r--r-- 1 deploy nginx     52 Apr 28 16:44 success.txt
-rw-r--r-- 1 deploy nginx    238 Mar 25 14:03 testscript.R
-rw-r--r-- 1 deploy nginx     25 Apr 28 16:44 wd.txt
-rw-r--r-- 1 deploy nginx     54 Apr 28 15:52 wrong_pids.txt
deploy@vtfsmprevmedredcap1 staging /var/www/apps/detcollect $ vi parse_payload.R 



















deploy@vtfsmprevmedredcap1 staging /var/www/apps/detcollect $ mkdir data
deploy@vtfsmprevmedredcap1 staging /var/www/apps/detcollect $ mv redcap_db_2022-0* data
deploy@vtfsmprevmedredcap1 staging /var/www/apps/detcollect $ ls -l
total 208
-rw-r--r-- 1 deploy nginx   3650 Oct 18  2021 404.html
-rw-r--r-- 1 deploy nginx   3693 Oct 18  2021 50x.html
-rw-r--r-- 1 deploy nginx  87605 Apr 28 16:44 ar.csv
-rw-r--r-- 1 deploy nginx     64 Apr 28 16:44 args.csv
drwxrwxr-x 2 deploy deploy   326 May 12 13:11 data
-rw-r--r-- 1 deploy nginx    574 Mar 25 15:50 index.php
-rw-r--r-- 1 deploy nginx    960 Mar 31 17:27 new_obs_2022-03-31.csv
-rw-r--r-- 1 deploy nginx   1024 Apr  4 18:32 new_obs_2022-04-04.csv
-rw-r--r-- 1 deploy nginx   1022 Apr  5 11:30 new_obs_2022-04-05.csv
-rw-r--r-- 1 deploy nginx   1013 Apr  6 16:32 new_obs_2022-04-06.csv
-rw-r--r-- 1 deploy nginx    599 Apr  8 09:21 new_obs_2022-04-08.csv
-rw-r--r-- 1 deploy nginx   1013 Apr 13 14:13 new_obs_2022-04-13.csv
-rw-r--r-- 1 deploy nginx   1017 Apr 14 11:34 new_obs_2022-04-14.csv
-rw-r--r-- 1 deploy nginx   1022 Apr 19 11:42 new_obs_2022-04-19.csv
-rw-r--r-- 1 deploy nginx   1018 Apr 21 16:33 new_obs_2022-04-21.csv
-rw-r--r-- 1 deploy nginx    996 Apr 28 16:44 new_obs_2022-04-28.csv
-rw-r--r-- 1 deploy nginx  10544 Apr 28 16:44 new_randomization.txt
-rw-r--r-- 1 deploy nginx    996 Apr 28 16:44 ntr.csv
-rw-r--r-- 1 deploy nginx     62 Apr 28 16:44 out.txt
-rw-rw-r-- 1 deploy deploy  3826 May 12 13:10 parse_payload.R
-rw-rw-r-- 1 deploy deploy  1856 Apr  4 18:32 randomization.R
-rw-r--r-- 1 deploy nginx   1050 Apr 28 16:44 randomized_patient.csv
-rw-rw-r-- 1 deploy deploy  6641 Apr  4 16:37 source_mv.R
-rw-r--r-- 1 deploy nginx     12 Mar 31 16:44 srccond.txt
-rw-rw-r-- 1 deploy deploy    18 Apr 28 16:44 srcmvall.txt
-rw-r--r-- 1 deploy nginx     52 Apr 28 16:44 success.txt
-rw-r--r-- 1 deploy nginx    238 Mar 25 14:03 testscript.R
-rw-r--r-- 1 deploy nginx     25 Apr 28 16:44 wd.txt
-rw-r--r-- 1 deploy nginx     54 Apr 28 15:52 wrong_pids.txt
deploy@vtfsmprevmedredcap1 staging /var/www/apps/detcollect $ mv new_obs_2022-0* data
deploy@vtfsmprevmedredcap1 staging /var/www/apps/detcollect $ ls -l
total 172
-rw-r--r-- 1 deploy nginx   3650 Oct 18  2021 404.html
-rw-r--r-- 1 deploy nginx   3693 Oct 18  2021 50x.html
-rw-r--r-- 1 deploy nginx  87605 Apr 28 16:44 ar.csv
-rw-r--r-- 1 deploy nginx     64 Apr 28 16:44 args.csv
drwxrwxr-x 2 deploy deploy  4096 May 12 13:11 data
-rw-r--r-- 1 deploy nginx    574 Mar 25 15:50 index.php
-rw-r--r-- 1 deploy nginx  10544 Apr 28 16:44 new_randomization.txt
-rw-r--r-- 1 deploy nginx    996 Apr 28 16:44 ntr.csv
-rw-r--r-- 1 deploy nginx     62 Apr 28 16:44 out.txt
-rw-rw-r-- 1 deploy deploy  3826 May 12 13:10 parse_payload.R
-rw-rw-r-- 1 deploy deploy  1856 Apr  4 18:32 randomization.R
-rw-r--r-- 1 deploy nginx   1050 Apr 28 16:44 randomized_patient.csv
-rw-rw-r-- 1 deploy deploy  6641 Apr  4 16:37 source_mv.R
-rw-r--r-- 1 deploy nginx     12 Mar 31 16:44 srccond.txt
-rw-rw-r-- 1 deploy deploy    18 Apr 28 16:44 srcmvall.txt
-rw-r--r-- 1 deploy nginx     52 Apr 28 16:44 success.txt
-rw-r--r-- 1 deploy nginx    238 Mar 25 14:03 testscript.R
-rw-r--r-- 1 deploy nginx     25 Apr 28 16:44 wd.txt
-rw-r--r-- 1 deploy nginx     54 Apr 28 15:52 wrong_pids.txt
deploy@vtfsmprevmedredcap1 staging /var/www/apps/detcollect $ vi randomization.R
deploy@vtfsmprevmedredcap1 staging /var/www/apps/detcollect $ cp source_mv.R source_msb.R
deploy@vtfsmprevmedredcap1 staging /var/www/apps/detcollect $ ls -l
total 180
-rw-r--r-- 1 deploy nginx   3650 Oct 18  2021 404.html
-rw-r--r-- 1 deploy nginx   3693 Oct 18  2021 50x.html
-rw-r--r-- 1 deploy nginx  87605 Apr 28 16:44 ar.csv
-rw-r--r-- 1 deploy nginx     64 Apr 28 16:44 args.csv
drwxrwxr-x 2 deploy deploy  4096 May 12 13:11 data
-rw-r--r-- 1 deploy nginx    574 Mar 25 15:50 index.php
-rw-r--r-- 1 deploy nginx  10544 Apr 28 16:44 new_randomization.txt
-rw-r--r-- 1 deploy nginx    996 Apr 28 16:44 ntr.csv
-rw-r--r-- 1 deploy nginx     62 Apr 28 16:44 out.txt
-rw-rw-r-- 1 deploy deploy  3826 May 12 13:10 parse_payload.R
-rw-rw-r-- 1 deploy deploy  1857 May 12 13:12 randomization.R
-rw-r--r-- 1 deploy nginx   1050 Apr 28 16:44 randomized_patient.csv
-rw-rw-r-- 1 deploy deploy  6641 May 12 13:12 source_msb.R
-rw-rw-r-- 1 deploy deploy  6641 Apr  4 16:37 source_mv.R
-rw-r--r-- 1 deploy nginx     12 Mar 31 16:44 srccond.txt
-rw-rw-r-- 1 deploy deploy    18 Apr 28 16:44 srcmvall.txt
-rw-r--r-- 1 deploy nginx     52 Apr 28 16:44 success.txt
-rw-r--r-- 1 deploy nginx    238 Mar 25 14:03 testscript.R
-rw-r--r-- 1 deploy nginx     25 Apr 28 16:44 wd.txt
-rw-r--r-- 1 deploy nginx     54 Apr 28 15:52 wrong_pids.txt
deploy@vtfsmprevmedredcap1 staging /var/www/apps/detcollect $ vi randomization.R 
deploy@vtfsmprevmedredcap1 staging /var/www/apps/detcollect $ vi source_msb.R

###------------------------------###
###------------------------------###
### Minimal Sufficient Balance
### Adaptive Randomization
###------------------------------###
###------------------------------###


###--------------------###
### Libraries
###--------------------###
#library(tidyverse)

###--------------------###
### Functions to compute
### balance
###--------------------###



#' @name get_votes
#' @param center string indicating variable name for center
#' @param covariates list of strings indicating variables on which to conduct balance
#' @param treamtent string indicating treatment assignment variable
#' @param min_n_adapt integer indicating # of already randomized individuals required to run adaptive algorithm
#' @param prob_vote probability of assigning to treatment if it would improve balance
#' @return treatment assignment probability
#' @note 1 = treatment
get_votes <- function(data, new_data, # data
                      center, covariates, treatment, # variable names
                      min_n_adapt = 10,
                      prob_vote = 0.7, # probability split for majority arm
                      show_votes = F){ # output options
  
  
  if(nrow(data) < min_n_adapt){
    
    prob = .5
    
    if(show_votes){
      
      return(
        list(
          prob = prob,
          votes = NULL,
          majority = NULL
        )
      )
      
    } else {
      
      return(prob)
      
    }
  } else {
    overall_vote <- list()
    center_vote <- list()
    mean_vote <- list()
    
    dtrt <- data %>%
      summarize(pct_trt = mean(get(treatment)))
    
    vote <- ifelse(dtrt > 0.5, "Arm 0",
                   ifelse(dtrt < 0.5, "Arm 1", "Neutral"))
    
    overall_vote[["overall"]] <- vote
    
    new_center <- new_data %>% select(get(center)) %>% pull()
    
    # Summarize assignment within center of interest
    dc <- data %>%
      filter(.data[[center]] == new_center) %>%
      summarize(x = sum(get(treatment)), n = n())
    
    # Check if this is a brand new center
    # or if patients have already been
    # randomized in this center.
    if(dc$n == 0){
      
      vote <- "Neutral"
      center_vote[[paste("center")]] <- vote
      
      
    } else {
      
      # Get p-value
      bt <- binom.test(dc$x, dc$n)
      
      # Compute and return vote for arm
      v0 <- (bt$p.value < 0.3 & dc$x/dc$n < 0.5)
      v1 <- (bt$p.value < 0.3 & dc$x/dc$n > 0.5)
      vote <- ifelse(v0, "Arm 0",
                     ifelse(v1, "Arm 1", "Neutral"))
      center_vote[["center"]] <- vote
    }
    
    for(j in covariates){
      
      if(nrow(data) == 1){
        
        vote <- "Neutral"
        
      } else if(is.numeric(data[[j]])){
        
        if(nrow(data) <= 4 | length(unique(data[,paste(treatment)])) == 1){
          
          vote <- "Neutral"
          
        } else {
          
          # Compute balance using linear model
          mod <- lm(data[[j]] ~ data[[treatment]])
          res <- summary(mod)$coefficients
          
          # Formulate output as tibble
          out <- tibble(
            var = j, # variable name
            diff = res[2, "Estimate"], # Difference
            se = res[2, "Std. Error"], # SE of difference
            stat = res[2, "t value"], # test statistic
            p = res[2, "Pr(>|t|)"] # p-value
          )
          
          x_sum <- data %>%
            group_by(get(treatment)) %>%
            summarize(x = mean(get(j), na.rm = T)) %>%
            rename(treatment = `get(treatment)`)
          
          x1 <- x_sum %>%
            filter(treatment == 1) %>%
            pull(x)
          
          x0 <- x_sum %>%
            filter(treatment == 0) %>%
            pull(x)
          
          new_col_name <- new_data %>% pull(get(j))
          
          # Compute whether new patient gets voted to arm 0 or arm 1
          v1 <- ((out$p < 0.3) & (out$diff > 0) & (new_col_name < x0)) |
            ((out$p < 0.3) & (out$diff < 0) & (new_col_name > x0))
          v0 <- ((out$p < 0.3) & (out$diff > 0) & (new_col_name < x1)) |
            ((out$p < 0.3) & (out$diff < 0) & (new_col_name > x1))
          
          # Return vote
          vote <- ifelse(v1, "Arm 1", ifelse(v0, "Arm 0", "Neutral"))
          
        }
        
      } else if(is.factor(data[[j]])){
        
        if((length(unique(data[[j]])) == 1) |
           (length(unique(data[[treatment]])) == 1)){
          
          vote <- "Neutral"
          
        } else {
          
          mod <- chisq.test(data[[treatment]], data[[j]])
          
          # Format output as table
          out <- tibble(
            var = j,
            observed = list(mod$observed),
            expected = list(mod$expected),
            diff = list(mod$observed - mod$expected),
            stat = mod$statistic,
            p = mod$p.value)
          
          new_col_name <- new_data %>% select(get(j)) %>% pull()
          
          # Compute and return vote
          v0 <- (out$p < 0.3 & out$expected[[1]][new_col_name, 1] > out$observed[[1]][new_col_name, 1])
          v1 <- (out$p < 0.3 & out$expected[[1]][new_col_name, 2] > out$observed[[1]][new_col_name, 2])
          vote <- ifelse(v1, "Arm 1", ifelse(v0, "Arm 0", "Neutral"))
          
        }
        
      }
      
      mean_vote[[paste(j)]] <- vote
      
    }
    
    total_vote <- t(as.data.frame(c(overall_vote, center_vote, mean_vote)))
    #return(t(as.data.frame(total_vote)))
    
    vote_tab <- as.data.frame(table(total_vote))
    
    vt <- vote_tab %>% filter(total_vote != "Neutral")
    
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
  
}

# prob <- get_votes(center, covariates, treatment, data, new_data)

#' @name generate_assignment
#' @param prob numeric [0,1]: Probability of assignment to arm 1 (treatment)
#' @return treatment assigment as integer
#' @note 1 = treatment
generate_assignment <- function(prob){
  return(
    rbinom(1, 1, prob)
  )
}
