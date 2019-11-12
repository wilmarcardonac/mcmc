Program mcmc

  !####################
  ! LOAD NEEDED MODULES
  !####################

  use input
  use subroutines 

  !#########################################
  ! VARIABLES DECLARATION AND INITIALIZATION
  !#########################################

  Implicit none

  !######################
  ! CODE STARTS EXECUTION
  !######################

  call create_directories_if_needed()

  open(UNIT_FILE1,file=EXECUTION_INFORMATION)

  ! DEFINE STARTING POINT, COVARIANCE MATRIX, CREATE GETDIST FILES

  call initialisation()

  call create_getdist_files()
  
  ! COMPUTE THEORETICAL MODEL

  ! READ THEORETICAL MODEL AND COMPUTE LIKELIHOOD

  ! GENERATE NEW POINT IN PARAMETER SPACE AND DECIDE ABOUT PLAUSIBILITY

  ! COMPUTE LIKELIHOOD FOR NEW POINT

  ! DECIDE ABOUT NEW POINT: KEEP IT, REJECT IT

  ! COMPUTE ACCEPTANCE PROBABILITY AND DECIDE WHETHER OR NOT UPDATE COVARIANCE MATRIX 
  
  close(UNIT_FILE1)

  !####################
  ! CODE ENDS EXECUTION
  !####################
  
End Program mcmc
