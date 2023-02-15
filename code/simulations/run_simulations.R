library(tidyverse)

source("./code/simulations/generate_pts.R")

generate_new_pt(1) %>% 
  complete_randomization_form()
