Module input

  use fgsl
  
  Implicit none

  save 

  ! RANDOM NUMBER GENERATOR VARIABLES
  type(fgsl_rng) :: r
  type(fgsl_rng_type) :: t
  Real*8 :: random_uniform    ! RANDOM UNIFORM DEVIATE BETWEEN 0 AND 1
  
  !DEFINING PARAMETERS DATA TYPE:
  type parameters_mcmc 
     Character(len=20) :: name
     Real*8 :: mean
     Real*8 :: lower_limit
     Real*8 :: upper_limit
     Real*8 :: sigma
     Real*8 :: scale
     Character(len=20) :: latexname
  end type parameters_mcmc

  ! MCMC PARAMETERS 
  Character(len=*),parameter :: likelihood = 'gaussian' ! OPTIONS: 'gaussian','euclid'
  Character(len=*),parameter :: starting_point = 'bestfit' ! OPTIONS: 'mean','bestfit','random','last_point'
  Character(len=*),parameter :: starting_cov_mat = 'given' !'diagonal' ! OPTIONS: 'diagonal','given'
  
  Integer*4,parameter :: number_iterations = 110000 ! TOTAL NUMBER OF ITERATIONS IN MCMC RUN
  Integer*4,parameter :: number_of_parameters = 2 ! 11   ! TOTAL NUMBER VARYING PARAMETERS     
  Integer*4,parameter :: UNIT_FILE1 = 80  ! UNIT NUMBER EXECUTION INFORMATION FILE
  Integer*4,parameter :: UNIT_FILE2 = 81  ! UNIT NUMBER GETDIST FILES
  Integer*4,parameter :: UNIT_FILE3 = 82  ! UNIT NUMBER GETDIST FILES
  Integer*4,parameter :: UNIT_FILE4 = 83  ! UNIT NUMBER CHAIN FILE
  Integer*4,parameter :: UNIT_FILE5 = 84  ! UNIT NUMBER CHAIN FILE
  Integer*4,parameter :: UNIT_FILE6 = 85  ! UNIT NUMBER COVMAT FILE
  Integer*4,parameter :: UNIT_FILE7 = 86  ! UNIT NUMBER COVMAT FILE
  Integer*4,parameter :: UNIT_FILE8 = 87  ! UNIT NUMBER BESTFIT FILE
  Integer*4 :: number_accepted_points = 0 ! COUNT POINTS IN PARAMETER SPACE
  Integer*4 :: number_rejected_points = 0 ! COUNT POINTS IN PARAMETER SPACE
  Integer*4 :: weight = 1 ! COUNTS NUMBER OF TAKEN STEPS BEFORE MOVING TO A NEW POINT
  Integer*4,parameter    :: jumping_factor_update = 100    ! STEPS TAKEN BEFORE UPDATING JUMPING FACTOR (IF NEEDED)
  Integer*4,parameter    :: steps_taken_before_definite_run = 10000 !5000!10000!0 ! STEPS TAKEN BEFORE FREEZING COVARIANCE MATRIX
  Integer*4,parameter    :: covariance_matrix_update = 2000 !5000!10000!0 ! STEPS TAKEN BEFORE UPDATING COVARIANCE MATRIX (IF NEEDED)
  Integer :: status1
  Integer*4,dimension(13) :: buff
  
  Real*8,parameter       :: step_size_changes = 1.d-1      ! CHANGE IN STEP SIZE
  Real*8,dimension(number_of_parameters,number_of_parameters) :: Cov_mat ! COVARIANCE MATRIX
  Real*8,dimension(number_of_parameters) :: old_point, current_point, bestfit_point
  Real*8 :: jumping_factor = 2.38d0/sqrt(dble(number_of_parameters)) ! INCREASE/DECREASE TO MATCH INITIAL ACCEPTANCE PROBABILITY
  Real*8 :: old_loglikelihood,current_loglikelihood      ! STORE LIKELIHOOD VALUES
  Real*4,dimension(number_iterations) :: acceptance_probability
  Real*4,parameter :: lower_limit_ap = 0.25! LOWER LIMIT ACCEPTANCE PROBABILITY
  Real*4,parameter :: upper_limit_ap = 0.3! UPPER LIMIT ACCEPTANCE PROBABILITY
  Real*4 :: average_ap ! AVERAGE ACCEPTANCE PROBABILITY
!  Real*8 :: acceptance_ratio  ! IT STORES ACCEPTANCE RATIO FOR CURRENT CHAIN

  Logical :: bad_ap!,good_acceptance_probability ! CONTROL PLAUSIBLE VALUES OF COSMOLOGICAL PARAMETERS


  type(parameters_mcmc), dimension(number_of_parameters) :: parameters 

  ! FIXED PARAMETERS IN CODE CLASS 
  Real*8,parameter :: N_ur = 2.0328d0
  Real*8,parameter :: N_ncdm = 1.d0
  Real*8,parameter :: deg_ncdm = 1.d0
  Real*8,parameter :: tau = 5.96d-2
  !Real*8,parameter :: sigma_tau = 8.9d-3
  
  ! PATHS TO FILES:
  Character(len=*),parameter :: OUTPUT = './output'
  Character(len=*),parameter :: CHAINS = './chains'
  Character(len=*),parameter :: DATA = './data'
  Character(len=*),parameter :: COVMAT = './covmat'
  Character(len=*),parameter :: BESTFIT = './bestfit'
  Character(len=*),parameter :: FIGURES = './figures'
  Character(len=*),parameter :: EXECUTION_INFORMATION = './output/execution_information.txt'
  Character(len=*),parameter :: RANGES_FILE = './chains/mcmc_final_output.ranges'
  Character(len=*),parameter :: PARAMNAMES_FILE = './chains/mcmc_final_output.paramnames'
  Character(len=*),parameter :: CHAIN_FILE = './chains/mcmc_final_output.txt'
  Character(len=*),parameter :: CHAIN_FILE_AUX = './output/mcmc_output.txt'
  Character(len=*),parameter :: COVMAT_FILE_AUX = './output/covmat.txt'
  Character(len=*),parameter :: BESTFIT_FILE_AUX = './output/bestfit.txt'
  Character(len=*),parameter :: COVMAT_FILE = COVMAT//trim('/covmat')//'.txt'
  Character(len=*),parameter :: BESTFIT_FILE = BESTFIT//trim('/bestfit')//'.txt'
  
  !  Character(len=*),parameter :: PATH_TO_CHAINS_CHAIN = './output/chains/mcmc_final_output_'
!  Character(len=*),parameter :: EXECUTION_INFORMATION_CHAIN = './output/chains/execution_information_chain_'

!  Character(len=*),parameter :: PATH_TO_INI_FILES = './ini_files/current_euclid_galaxy_cl_lensing_'
!  Character(len=*),parameter :: PATH_TO_CURRENT_CL = './output/current_euclid_galaxy_lensing_'
!  Character(len=*),parameter :: PATH_TO_INI_FILES_CMB = './ini_files/current_fake_planck_cl_'
!  Character(len=*),parameter :: PATH_TO_CURRENT_CL_CMB = './output/current_fake_planck_'
  
End Module input
