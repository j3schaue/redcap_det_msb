# REDCap Minimal Sufficient Balance Integration

This repository contains base code to link REDCap data entry triggers to algorithms to randomize participants in randomized experiments. 
The focus is on the Minimal Sufficient Balance (MSB) algorithm. 


The code is structured to run on a secure server, with the following scripts managing interactions with REDCap's data entry trigger:

- _index.php_ Listens for DET payloads sent to the secure server. 

     - This script must be edited and tailored to specific REDCap projects, but should contain the REDCap PID and relevant instrument names.

- _parse_payload.R_

     - This script must be tailored to specific REDCap projects. The opening lines contain modifiable arguments used in subsequent code. 

- _randomization.R_

     - This script can be updated 

- _source_msb.R_

In this workflow, R interacts with the REDCap database via the REDcap API. Tokens are not stored publicly on this repository.




# REDCap Project Setup

The implementation of the MSB algorithm and payload parsing assume that the study in question is a 2-armed RCT. 
It also assumes the following:

1. The REDCap database contains a separate instrument within the REDCap project is designated for identifying participants ready for randomization.

     a. Since randomization leverages the MSB algorithm, baseline measures must exist for the randomized and ready-to-be-randomized participants alike. As a means of quality control, we recommend (and the code requires) a designated instrument (see _instrument_ in _index.php_) and field (see _randomization_ready_ in _parse_payload.R_) that indicates a patient should indeed be randomized immediately.

2. No data used in the MSB is missing. 

     a. MSB balances individual covariates (specified in _bal_covariates_ in _parse_payload.R_) for a participant. No randomized or ready-to-be-randomized participants should be missing any data for those covariates.
     
3. The REDCap database contains multiple events and that MSB covariates are stored in the baseline event.

     a. Users must specify the baseline event name from REDCap.
     b. Code can be easily modified for data without repeat instances/different event names.

4. The REDCap project contains a designated field for the following:

     a. Study arm (_study_arm_ in _parse_payload.R_) 
     b. Randomization probability _rand_prob_field_
     c. Randomization votes _rand_votes_field_ 

5. The REDCap project in question is for a multi-center study. 

     a. Scripts can be amended easily for use with single-center studies.



# Server File Setup

These scripts are designed to run on a secure server. 
The directory structure on the server should be as follows:

Main directory: 

- _index.php_
- _parse_payload.R_
- _randomization.R_
- _source_msb.R_
- _./data/_ subdirectory

     - interim results will be written to the _data/_ subdirectory from the R scripts.
     
     
     
  
