<?php
if ($_SERVER["REQUEST_METHOD"] == "POST") {

  // collect values
  $redcapurl = $_POST['redcap_url'];
  $purl = $_POST['project_url'];
  $pid = $_POST['project_id'];
  $uname = $_POST['username'];
  $rid = $_POST['record'];
  $instrument = $_POST['instrument'];
  
  if ( $pid == 7130 && $instrument == "randomization" ) { // USER UPDATE: User must modify the PID and instrument to correspond to their REDCap build.
    shell_exec("Rscript parse_payload.R $pid $rid $instrument $status 2> logfile.txt");
  } else {
    shell_exec("echo 'DET received from wrong PID $pid or instrument $instrument' | tee wrong_pids.txt");
  }
}
?>