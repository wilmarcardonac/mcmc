Program mcmc

  !####################
  ! LOAD NEEDED MODULES
  !####################

  use input
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

  call system('rm '//trim(OUTPUT)//'/*.ini '//trim(OUTPUT)//'/*.dat')

  open(UNIT_FILE1,file=EXECUTION_INFORMATION)

  open(UNIT_FILE4,file=CHAIN_FILE)

  open(UNIT_FILE5,file=CHAIN_FILE_AUX)

  call initialisation() ! DEFINE STARTING POINT, COVARIANCE MATRIX, CREATE GETDIST FILES

  call create_getdist_files()

  allocate (El(lmin:lmax,0:nbins,0:nbins),Cl_fid(lmin:lmax,0:nbins,0:nbins),&
       Cl_obs(lmin:lmax,0:nbins,0:nbins), Cl_current(lmin:lmax,0:nbins,0:nbins),&
       Nl(1:nbins,1:nbins),Elnl(lmin:lmax,0:nbins,0:nbins))
  
  call load_data()   ! LOAD DATA

  call compute_theoretical_model()   ! COMPUTE THEORETICAL MODEL

  call compute_ln_likelihood(current_point,old_loglikelihood)   ! READ THEORETICAL MODEL AND COMPUTE LIKELIHOOD

  write(UNIT_FILE1,*) 'HEADER FOR CHAIN FILE IS: '
  
  write(UNIT_FILE1,*) '# WEIGHT   -ln(L/L_{max})    ', parameters(1:number_of_parameters)%name

  write(UNIT_FILE1,*) 'ln(L/L_{max}) AT STARTING POINT = ', old_loglikelihood

  Do index=1,number_iterations
     
     call generate_new_point_in_parameter_space()   ! GENERATE NEW POINT IN PARAMETER SPACE AND DECIDE ABOUT PLAUSIBILITY

     call compute_theoretical_model() ! COMPUTE LIKELIHOOD FOR NEW POINT
     
     call compute_ln_likelihood(current_point,current_loglikelihood)

     call take_decision_about_current_point(index) ! DECIDE ABOUT NEW POINT: KEEP IT, REJECT IT

     call update_covariance_matrix(index) ! COMPUTE ACCEPTANCE PROBABILITY AND DECIDE WHETHER OR NOT UPDATE COVARIANCE MATRIX 
     
  End Do

  call write_cov_mat()

  call write_bestfit()
  
  write(UNIT_FILE1,*) 'CODE SUCCESSFULLY ENDS EXECUTION'
  
  call fgsl_rng_free(r)

  deallocate(Nl,El,Elnl,Cl_fid,Cl_obs,Cl_current) 
  
  close(UNIT_FILE1)

  close(UNIT_FILE4)
  
  !####################
  ! CODE ENDS EXECUTION
  !####################
  
End Program mcmc
