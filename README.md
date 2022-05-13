# REDCap Minimal Sufficient Balance Integration

This repository contains base code to link REDCap data entry triggers to algorithms to randomize participants in randomized experiments. 
The focus is on the Minimal Sufficient Balance (MSB) algorithm. 


The code is structured to run on a secure server, with the following scripts managing interactions with REDCap's data entry trigger:

- _index.php_ Listens for DET payloads sent to the secure server. 

     - This script can be edited and tailored to specific REDCap projects, but should contain the REDCap PID and relevant instrument names.

- _parse_payload.R_

     - This script can be tailored to specific REDCap projects. The opening lines contain modifiable arguments used in subsequent code. 

- _randomization.R_
- _source_msb.R_

In this workflow, R interacts with the REDCap database via the REDcap API. Tokens are not stored publicly on this repository, but the code can be run as-is if the variable _TOKEN_ is set as a system variable.