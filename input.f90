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
  Character(len=*),parameter :: likelihood = 'euclid' ! OPTIONS: 'gaussian','euclid'
  Character(len=*),parameter :: starting_point = 'mean' ! OPTIONS: 'mean','bestfit','random','last_point'
  Character(len=*),parameter :: starting_cov_mat = 'diagonal' !'diagonal' ! OPTIONS: 'diagonal','given'
  
  Integer*4,parameter :: number_iterations = 120000 ! TOTAL NUMBER OF ITERATIONS IN MCMC RUN
  Integer*4,parameter :: number_of_parameters = 11   ! TOTAL NUMBER VARYING PARAMETERS     
  Integer*4,parameter :: UNIT_FILE1 = 80  ! UNIT NUMBER EXECUTION INFORMATION FILE
  Integer*4,parameter :: UNIT_FILE2 = 81  ! UNIT NUMBER GETDIST FILES
  Integer*4,parameter :: UNIT_FILE3 = 82  ! UNIT NUMBER GETDIST FILES
  Integer*4,parameter :: UNIT_FILE4 = 83  ! UNIT NUMBER CHAIN FILE
  Integer*4,parameter :: UNIT_FILE5 = 84  ! UNIT NUMBER CHAIN FILE
  Integer*4,parameter :: UNIT_FILE6 = 85  ! UNIT NUMBER COVMAT FILE
  Integer*4,parameter :: UNIT_FILE7 = 86  ! UNIT NUMBER COVMAT FILE
  Integer*4,parameter :: UNIT_FILE8 = 87  ! UNIT NUMBER BESTFIT FILE
  Integer*4,parameter :: UNIT_FILE9 = 88  ! UNIT NUMBER INI FILE
  Integer*4,parameter :: UNIT_FILE10 = 89  ! UNIT NUMBER SPECTRA
  Integer*4 :: number_accepted_points = 0 ! COUNT POINTS IN PARAMETER SPACE
  Integer*4 :: number_rejected_points = 0 ! COUNT POINTS IN PARAMETER SPACE
  Integer*4 :: weight = 1 ! COUNTS NUMBER OF TAKEN STEPS BEFORE MOVING TO A NEW POINT
  Integer*4,parameter    :: jumping_factor_update = 250    ! STEPS TAKEN BEFORE UPDATING JUMPING FACTOR (IF NEEDED)
  Integer*4,parameter    :: steps_taken_before_definite_run = 20000 !10000 !5000!10000!0 ! STEPS TAKEN BEFORE FREEZING COVARIANCE MATRIX
  Integer*4,parameter    :: covariance_matrix_update = 2000 !5000!10000!0 ! STEPS TAKEN BEFORE UPDATING COVARIANCE MATRIX (IF NEEDED)
  Integer :: status1
  Integer*4,dimension(13) :: buff
  
  Real*8,parameter       :: step_size_changes = 4.d-1      ! CHANGE IN STEP SIZE
  Real*8,dimension(number_of_parameters,number_of_parameters) :: Cov_mat ! COVARIANCE MATRIX
  Real*8,dimension(number_of_parameters) :: old_point, current_point, bestfit_point
  Real*8, allocatable, dimension(:,:) :: Nl ! SHOT NOISE
  Real*8, allocatable, dimension(:,:,:) :: El, Elnl, Cl_fid, Cl_obs, Cl_current
  Real*8 :: jumping_factor = 2.38d0/sqrt(dble(number_of_parameters)) ! INCREASE/DECREASE TO MATCH INITIAL ACCEPTANCE PROBABILITY
  Real*8 :: old_loglikelihood,current_loglikelihood      ! STORE LIKELIHOOD VALUES
  Real*4,dimension(number_iterations) :: acceptance_probability
  Real*4,parameter :: lower_limit_ap = 0.1! LOWER LIMIT ACCEPTANCE PROBABILITY
  Real*4,parameter :: upper_limit_ap = 0.4! UPPER LIMIT ACCEPTANCE PROBABILITY
  Real*4 :: average_ap ! AVERAGE ACCEPTANCE PROBABILITY

  Logical :: bad_ap!,good_acceptance_probability ! CONTROL PLAUSIBLE VALUES OF COSMOLOGICAL PARAMETERS
  Logical,parameter :: lensing = .true.
  Logical :: cl_current_found

  type(parameters_mcmc), dimension(number_of_parameters) :: parameters 

  ! FIXED PARAMETERS IN CODE CLASS 
  Real*8,parameter :: N_ur = 2.0328d0
  Real*8,parameter :: N_ncdm = 1.d0
  Real*8,parameter :: deg_ncdm = 1.d0
  Real*8,parameter :: tau = 5.96d-2
  !Real*8,parameter :: sigma_tau = 8.9d-3

  Integer*4,parameter :: nbins = 10
  Integer*4,parameter :: lmax_class = 2000
  Integer*4,parameter :: lmin = 2
  Integer*4,parameter :: lmax = 400

  Real*8,parameter    :: Pi = 3.141592653589793d0
  Real*8,parameter    :: zmin = 0.1d0
  Real*8,parameter    :: zmax = 2.0d0
  Real*8,parameter    :: dz = 1.0d-3
  Real*8,parameter    :: gal_per_sqarcmn = 30.d0
  Real*8,parameter    :: fsky = 1.5d4/4.1253d4
  Real*8,parameter    :: theoreticalerror = 0.d0 !5.d-2
  Real*8,dimension(int((zmax - zmin)/dz)) :: z_array
  Real*8,dimension(int(nbins+1)) :: z_bin_edges
  Real*8,dimension(nbins) :: z_bin_centers, z_bin_widths, z_bin_bias, s_z_mag_bias
  Real*8,parameter :: s_0 = 0.1194d0
  Real*8,parameter :: s_1 = 0.2122d0
  Real*8,parameter :: s_2 = -0.0671d0
  Real*8,parameter :: s_3 = 0.1031d0
  
  Character(len=*),parameter :: selection = 'tophat' ! OPTIONS: 'tophat', 'gaussian'

  ! PATHS TO FILES:

  Character(len=*),parameter :: ROOT_PATH = '/home/projects/dea/mcmc'
  Character(len=*),parameter :: OUTPUT = ROOT_PATH//trim('/')//'output'
  Character(len=*),parameter :: CHAINS = ROOT_PATH//trim('/')//'chains'
  Character(len=*),parameter :: DATA = ROOT_PATH//trim('/')//'data'
  Character(len=*),parameter :: COVMAT = ROOT_PATH//trim('/')//'covmat'
  Character(len=*),parameter :: BESTFIT = ROOT_PATH//trim('/')//'bestfit'
  Character(len=*),parameter :: FIGURES = ROOT_PATH//trim('/')//'figures'
  Character(len=*),parameter :: EXECUTION_INFORMATION = OUTPUT//trim('/')//'execution_information.txt'
  Character(len=*),parameter :: RANGES_FILE = CHAINS//trim('/')//'mcmc_final_output.ranges'
  Character(len=*),parameter :: PARAMNAMES_FILE = CHAINS//trim('/')//'mcmc_final_output.paramnames'
  Character(len=*),parameter :: CHAIN_FILE = CHAINS//trim('/')//'mcmc_final_output.txt'
  Character(len=*),parameter :: CHAIN_FILE_AUX = OUTPUT//trim('/')//'mcmc_output.txt'
  Character(len=*),parameter :: COVMAT_FILE_AUX = OUTPUT//trim('/')//'covmat.txt'
  Character(len=*),parameter :: BESTFIT_FILE_AUX = OUTPUT//trim('/')//'bestfit.txt'
  Character(len=*),parameter :: COVMAT_FILE = COVMAT//trim('/covmat')//'.txt'
  Character(len=*),parameter :: BESTFIT_FILE = BESTFIT//trim('/bestfit')//'.txt'
  Character(len=*),parameter :: INI_FILE = OUTPUT//trim('/')//'file.ini'
  Character(len=*),parameter :: EL_FILE = DATA//trim('/El_cl')//'.dat'
  Character(len=*),parameter :: ELNL_FILE = DATA//trim('/Elnl_cl')//'.dat'
  Character(len=*),parameter :: CLFID_FILE = DATA//trim('/Clfid_cl')//'.dat'
  Character(len=*),parameter :: CL_FILE = OUTPUT//trim('/Cl_cl')//'.dat'  
  
  Character(len=*),parameter :: CLASS_EXECUTABLE = './class_EFCLASS'
  Character(len=*),parameter :: HIGH_PRE = './class_EFCLASS/cl_lss.pre'
  Character(len=*),parameter :: LOW_PRE_G5 = './class_EFCLASS/cl_lss_low_g5.pre'
  Character(len=*),parameter :: LOW_PRE_T5 = './class_EFCLASS/cl_lss_low_t5.pre'
  Character(len=*),parameter :: LOW_PRE_G10 = './class_EFCLASS/cl_lss_low_g10.pre'
  Character(len=*),parameter :: LOW_PRE_T10 = './class_EFCLASS/cl_lss_low_t10.pre'
  
End Module input
