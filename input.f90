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


  Character(len=*),parameter :: likelihood = 'gaussian' ! OPTIONS: 'gaussian','euclid'

  Integer*4,parameter    :: number_iterations = 1000000 ! TOTAL NUMBER OF ITERATIONS IN MCMC RUN
  Integer*4,parameter    :: number_of_parameters = 11   ! TOTAL NUMBER VARYING PARAMETERS     

  type(parameters_mcmc), dimension(number_of_parameters) :: parameters 


  ! PATHS TO FILES:
  Character(len=*),parameter :: OUTPUT = './output'
  Character(len=*),parameter :: CHAINS = './chains'
  Character(len=*),parameter :: DATA = './data'
  Character(len=*),parameter :: COVMAT = './covmat'
  Character(len=*),parameter :: BESTFIT = './bestfit'
  Character(len=*),parameter :: FIGURES = './figures'
  
!  Character(len=*),parameter :: PATH_TO_CHAINS = './output/chains/mcmc_final_output.txt'
!  Character(len=*),parameter :: PATH_TO_CHAINS_CHAIN = './output/chains/mcmc_final_output_'
  
!  Character(len=*),parameter :: Execution_information = './output/chains/execution_information.txt'
!  Character(len=*),parameter :: EXECUTION_INFORMATION_CHAIN = './output/chains/execution_information_chain_'
  

!  Character(len=*),parameter :: PATH_TO_RANGES_FILE = './output/chains/mcmc_final_output.ranges'
!  Character(len=*),parameter :: PATH_TO_PARAMNAMES_FILE = './output/chains/mcmc_final_output.paramnames'
!  Character(len=*),parameter :: PATH_TO_INI_FILES = './ini_files/current_euclid_galaxy_cl_lensing_'
!  Character(len=*),parameter :: PATH_TO_CURRENT_CL = './output/current_euclid_galaxy_lensing_'
!  Character(len=*),parameter :: PATH_TO_INI_FILES_CMB = './ini_files/current_fake_planck_cl_'
!  Character(len=*),parameter :: PATH_TO_CURRENT_CL_CMB = './output/current_fake_planck_'

  
End Module input
