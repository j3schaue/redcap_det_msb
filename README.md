# REDCap Minimal Sufficient Balance Integration

This repository contains base code to link REDCap data entry triggers to algorithms to randomize participants in randomized experiments. 
The focus is on the Minimal Sufficient Balance (MSB) algorithm. 
There are two directories that offer implementation through REDCap's data entry trigger (DET) or via a scheduler.


# REDCap Project Setup

The implementation of the MSB algorithm and payload parsing assume that the study in question is a 2-armed RCT. 
It also assumes the following:

1. The REDCap database contains a separate instrument within the REDCap project is designated for identifying participants ready for randomization.

     a. Since randomization leverages the MSB algorithm, baseline measures must exist for the randomized and ready-to-be-randomized participants alike. As a means of quality control, we recommend (and the code requires) a designated instrument (see _instrument_ in _index.php_) and field (see _randomization_ready_ in _user_specified_variables.R_) that indicates a patient should indeed be randomized immediately.

2. No data used in the MSB is missing. 

     a. MSB balances individual covariates (specified in _bal_covariates_ in _user_specified_variables.R_) for a participant. No randomized or ready-to-be-randomized participants should be missing any data for those covariates.
     
3. The REDCap database contains multiple events and that MSB covariates are stored in the baseline event.

     a. Users must specify the baseline event name from REDCap (_user_specified_variables.R_).
     b. Code can be easily modified for data without repeat instances/different event names.

4. The REDCap project contains a designated field for the following, which are specified in the in _user_specified_variables.R_ file.

     a. Study arm (_study_arm_): This directory assumes a two-armed randomized trial. Values can be specified in the _already_randomized_value_ field, which assumes a strict ordering. 
    
     b. Randomization probability _prob_name_
     
     c. Randomization votes _vote_name_ 

5. The REDCap project in question is for a multi-center study. 

     a. Scripts can be amended easily for use with single-center studies.
     

# Configurations {.tabset}

## DET Configuration

The key scripts for DET integration of MSB are in the _det/_ directory as follows:

- _index.php_ Listens for DET payloads sent to the secure server. 

     - This script must be edited and tailored to specific REDCap projects, but should contain the REDCap PID and relevant instrument names.
     
- _user_specified_variables.R_

     - This script must be tailored to specific REDCap projects. 
     
- _parse_payload.R_

     - This script can be tailored to specific REDCap projects but you should start by modifying the _user_specified_variables.R_ script first. 

- _randomization.R_

     - This script can be modified in needed 

- _apply_msb.R_

In this workflow, R interacts with the REDCap database via the REDcap API. Tokens are not stored publicly on this repository.


### Server File Setup

These scripts are designed to run on a secure server. 
The directory structure on the server should be as follows:

Main directory: 

- _index.php_
- _parse_payload.R_
- _randomization.R_
- _apply_msb.R_
- _user_specified_variables.R_
- _./data/_ subdirectory

     - interim results will be written to the _data/_ subdirectory from the R scripts.
     
     
     
## Scheduler Configuration

The key scripts for setting up MSB via scheduler are in the _scheduler/_ directory as follows:

     
- _user_specified_variables.R_

     - This script must be tailored to specific REDCap projects. 
     
- _parse_data.R_

     - This script can be tailored to specific REDCap projects but you should start by modifying the _user_specified_variables.R_ script first. 

- _randomization.R_

     - This script can be modified in needed 

- _apply_msb.R_

In this workflow, R interacts with the REDCap database via the REDcap API. Tokens are not stored publicly on this repository.

In addition, basic scripts germane to the scheduler are also in this repository including:

- _scheduled_randomization.bat_ which should trigger the MSB _.R_ scripts above.

- _notify.py_ which will email relevant parties about the completion of MSB randomization.

Both of these will need to be modified to suit the parameters of the REDCap project.



### Server File Setup

These scripts are designed to run on a secure server. 
The directory structure on the server should be as follows:

Main directory: 

- _parse_data.R_
- _randomization.R_
- _apply_msb.R_
- _user_specified_variables.R_
- _scheduled_randomization.bat_
- _notify.py_
- _./data/_ subdirectory

     - interim results will be written to the _data/_ subdirectory from the R scripts.
