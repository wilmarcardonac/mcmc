Program mcmc

  !####################
  ! LOAD NEEDED MODULES
  !####################

  use input
  use likelihoods
  use subroutines
  use fgsl

  !#########################################
  ! VARIABLES DECLARATION AND INITIALIZATION
  !#########################################

  Implicit none

  Integer*4 :: index

  t = fgsl_rng_env_setup()
  t = fgsl_rng_default
  r = fgsl_rng_alloc (t)

  !######################
  ! CODE STARTS EXECUTION
  !######################

  call create_directories_if_needed()

  open(UNIT_FILE1,file=EXECUTION_INFORMATION)

  open(UNIT_FILE4,file=CHAIN_FILE)

  open(UNIT_FILE5,file=CHAIN_FILE_AUX)

  ! DEFINE STARTING POINT, COVARIANCE MATRIX, CREATE GETDIST FILES

  call initialisation()

  call create_getdist_files()
  
  ! COMPUTE THEORETICAL MODEL

  call compute_theoretical_model()

  ! READ THEORETICAL MODEL AND COMPUTE LIKELIHOOD

  call compute_ln_likelihood(old_point,old_loglikelihood)

  ! GENERATE NEW POINT IN PARAMETER SPACE AND DECIDE ABOUT PLAUSIBILITY

  write(UNIT_FILE1,*) 'HEADER FOR CHAIN FILE IS: '
  
  write(UNIT_FILE1,*) '# WEIGHT   -ln(L/L_{max})    ', parameters(1:number_of_parameters)%name
  
  Do index=1,number_iterations
     
     call generate_new_point_in_parameter_space()

     ! COMPUTE LIKELIHOOD FOR NEW POINT

     call compute_ln_likelihood(current_point,current_loglikelihood)

     ! DECIDE ABOUT NEW POINT: KEEP IT, REJECT IT

     call take_decision_about_current_point(index)

     ! COMPUTE ACCEPTANCE PROBABILITY AND DECIDE WHETHER OR NOT UPDATE COVARIANCE MATRIX 

     call update_covariance_matrix(index)
     
  End Do

  call fgsl_rng_free(r)
  
  close(UNIT_FILE1)

  close(UNIT_FILE4)
  
  !####################
  ! CODE ENDS EXECUTION
  !####################
  
End Program mcmc
