Module input

  Implicit none

  save 

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

  ! PARAMETERS 
  Character(len=*),parameter :: likelihood = 'gaussian' ! OPTIONS: 'gaussian','euclid'
  Character(len=*),parameter :: starting_point = 'mean' ! OPTIONS: 'mean','bestfit','random','last_point'

  Integer*4,parameter :: number_iterations = 100 ! TOTAL NUMBER OF ITERATIONS IN MCMC RUN
  Integer*4,parameter :: number_of_parameters = 2 ! 11   ! TOTAL NUMBER VARYING PARAMETERS     
  Integer*4,parameter :: UNIT_FILE1 = 80  ! UNIT NUMBER EXECUTION INFORMATION FILE
  Integer*4,parameter :: UNIT_FILE2 = 81  ! UNIT NUMBER GETDIST FILES
  Integer*4,parameter :: UNIT_FILE3 = 82  ! UNIT NUMBER GETDIST FILES
  Integer*4 :: number_accepted_points = 0 ! COUNT POINTS IN PARAMETER SPACE
  Integer*4 :: number_rejected_points = 0 ! COUNT POINTS IN PARAMETER SPACE
  Integer*4 :: weight = 1 ! COUNTS NUMBER OF TAKEN STEPS BEFORE MOVING TO A NEW POINT

  Real*8,dimension(number_of_parameters,number_of_parameters) :: Cov_mat ! COVARIANCE MATRIX
  Real*8,dimension(number_of_parameters) :: old_point, current_point
  Real*8 :: jumping_factor = 1.d0 !2.38d0/sqrt(dble(number_of_parameters)) ! INCREASE/DECREASE TO MATCH INITIAL ACCEPTANCE PROBABILITY
  Real*8 :: old_loglikelihood,current_loglikelihood      ! STORE LIKELIHOOD VALUES
!  Real*4 :: average_acceptance_probability
!  Real*8 :: acceptance_ratio  ! IT STORES ACCEPTANCE RATIO FOR CURRENT CHAIN

!  Logical :: not_good_app,non_plausible_parameters,good_acceptance_probability ! CONTROL PLAUSIBLE VALUES OF COSMOLOGICAL PARAMETERS
!  Logical,dimension(number_of_parameters) :: plausibility  

  type(parameters_mcmc), dimension(number_of_parameters) :: parameters 

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
  
!  Character(len=*),parameter :: PATH_TO_CHAINS = './output/chains/mcmc_final_output.txt'
!  Character(len=*),parameter :: PATH_TO_CHAINS_CHAIN = './output/chains/mcmc_final_output_'
!  Character(len=*),parameter :: EXECUTION_INFORMATION_CHAIN = './output/chains/execution_information_chain_'

!  Character(len=*),parameter :: PATH_TO_INI_FILES = './ini_files/current_euclid_galaxy_cl_lensing_'
!  Character(len=*),parameter :: PATH_TO_CURRENT_CL = './output/current_euclid_galaxy_lensing_'
!  Character(len=*),parameter :: PATH_TO_INI_FILES_CMB = './ini_files/current_fake_planck_cl_'
!  Character(len=*),parameter :: PATH_TO_CURRENT_CL_CMB = './output/current_fake_planck_'
  
End Module input
