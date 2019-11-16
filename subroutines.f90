Module subroutines

  Implicit none
     
contains

  subroutine create_directories_if_needed()

    use input
    Implicit none

    Logical :: dir_exist

    inquire(file=OUTPUT,exist=dir_exist)

    If (dir_exist) then

       continue
       
    Else

       call system('mkdir ./output')
       
    End if

    inquire(file=CHAINS,exist=dir_exist)

    If (dir_exist) then

       continue

    Else

       call system('mkdir ./chains')

    End if

    inquire(file=DATA,exist=dir_exist)

    If (dir_exist) then

       continue

    Else

       call system('mkdir ./data')

    End if

    inquire(file=COVMAT,exist=dir_exist)

    If (dir_exist) then

       continue

    Else

       call system('mkdir ./covmat')

    End if

    inquire(file=BESTFIT,exist=dir_exist)

    If (dir_exist) then

       continue

    Else

       call system('mkdir ./bestfit')

    End if

    inquire(file=FIGURES,exist=dir_exist)

    If (dir_exist) then

       continue

    Else

       call system('mkdir ./figures')

    End if
    
  end subroutine create_directories_if_needed

  subroutine initialisation()

    use input
    Implicit none

    Character(len=16) :: string
    Integer*4 :: index
    
    write(UNIT_FILE1,*) 'MCMC STARTS...'
    
    If (likelihood .eq. 'gaussian') then

       write(UNIT_FILE1,*) 'WORKING WITH A GAUSSIAN LIKELIHOOD (TESTING THE CODE)'

       Do index=1,number_of_parameters
          
          write(string,'(i2.2)') index
          
          parameters(index)%name = 'p'//trim(string)//''
          parameters(index)%mean = 0.d0
          parameters(index)%lower_limit = -1.d1
          parameters(index)%upper_limit = 1.d1
          parameters(index)%sigma = 5.d-1
          parameters(index)%scale = 1.d0
          parameters(index)%latexname = 'p_{'//trim(string)//'}'

       End Do
       
    Else if (likelihood .eq. 'euclid') then

       write(UNIT_FILE1,*) 'WORKING WITH A FAKE EUCLID LIKELIHOOD'
       
       parameters(1)%name = 'omega_b'
       parameters(1)%mean = 2.218d-2
       parameters(1)%lower_limit = 1.d-4
       parameters(1)%upper_limit = 1.d-1
       parameters(1)%sigma = 1.5d-4
       parameters(1)%scale = 1.d0
       parameters(1)%latexname = '\omega_b'

       parameters(2)%name = 'omega_cdm'
       parameters(2)%mean = 1.205d-1
       parameters(2)%lower_limit = 1.d-4
       parameters(2)%upper_limit = 1.d0
       parameters(2)%sigma = 1.4d-3
       parameters(2)%scale = 1.d0
       parameters(2)%latexname = '\omega_{cdm}'

       parameters(3)%name = 'n_s'
       parameters(3)%mean = 9.619d-1
       parameters(3)%lower_limit = 9.d-2
       parameters(3)%upper_limit = 2.d0
       parameters(3)%sigma = 4.5d-3
       parameters(3)%scale = 1.d0
       parameters(3)%latexname = 'n_s'

       write(UNIT_FILE1,*) 'IMPLEMENT ln 10^10 A_s!'

       stop

       parameters(4)%name = 'A_s'
       parameters(4)%mean = 2.12424d-9
       parameters(4)%lower_limit = 1.d-11
       parameters(4)%upper_limit = 1.d-7
       parameters(4)%sigma = 3.82d-11
       parameters(4)%scale = 1.d0
       parameters(4)%latexname = 'A_s'

       parameters(5)%name = 'H0'
       parameters(5)%mean = 6.693d1
       parameters(5)%lower_limit = 3.d1
       parameters(5)%upper_limit = 9.d1
       parameters(5)%sigma = 0.62d0
       parameters(5)%scale = 1.d0
       parameters(5)%latexname = 'H_0'

       parameters(6)%name = 'm_ncdm'
       parameters(6)%mean = 6.0d-2
       parameters(6)%lower_limit = 0.d0
       parameters(6)%upper_limit = 2.d0
       parameters(6)%sigma = 5.d-3
       parameters(6)%scale = 1.d0
       parameters(6)%latexname = 'm_{\nu}'

       parameters(7)%name = 'nc_bias_b0'
       parameters(7)%mean = 1.0d0
       parameters(7)%lower_limit = 0.d0
       parameters(7)%upper_limit = 3.d0
       parameters(7)%sigma = 1.0d-1
       parameters(7)%scale = 1.d0
       parameters(7)%latexname = 'b_0' 

       If (number_of_parameters .eq. 10) then

          write(UNIT_FILE1,*) 'IMPLEMENT logcs2!'

          stop

          parameters(8)%name = 'cs2_fld'
          parameters(8)%mean = 1.d0
          parameters(8)%lower_limit = -1.d1
          parameters(8)%upper_limit = 0.d0
          parameters(8)%sigma = 1.0d-1
          parameters(8)%scale = 1.d0
          parameters(8)%latexname = 

       Else if (number_of_parameters .eq. 11) then

          write(UNIT_FILE1,*) 'IMPLEMENT logceff2!'

          stop
          
          parameters(8)%name = 'cs2_fld' 
          parameters(8)%mean = 4.3d0
          parameters(8)%lower_limit = -3.d3
          parameters(8)%upper_limit = 0.d0
          parameters(8)%sigma = 1.0d-1
          parameters(8)%scale = 1.d0
          parameters(8)%latexname = '\log c_{eff}^2'

       End if

       parameters(9)%name = 'w0_fld'
       parameters(9)%mean = -8.0d-1
       parameters(9)%lower_limit = -2.d0
       parameters(9)%upper_limit = 0.d0
       parameters(9)%sigma = 2.2d-1
       parameters(9)%scale = 1.d0
       parameters(9)%latexname = 'w'

       If (number_of_parameters .eq. 10) then
          
          parameters(10)%name = 'e_pi'
          parameters(10)%mean = 0.0d0
          parameters(10)%lower_limit = -5.d-1
          parameters(10)%upper_limit = 5.d-1
          parameters(10)%sigma = 1.0d-1
          parameters(10)%scale = 1.d0
          parameters(10)%latexname = 'e_{\pi}'

       Else if (number_of_parameters .eq. 11) then

          parameters(10)%name = 'f_pi'
          parameters(10)%mean = 5.0d0
          parameters(10)%lower_limit = -3.d3
          parameters(10)%upper_limit = 1.d1
          parameters(10)%sigma = 1.0d-1
          parameters(10)%scale = 1.d0
          parameters(10)%latexname = 'f_{\pi}'

          write(UNIT_FILE1,*) 'IMPLEMENT log10g_pi!'

          stop

          parameters(11)%name = 'g_pi'
          parameters(11)%mean = 0.0d0 ! THIS IS ACTUALLY log10 g_pi
          parameters(11)%lower_limit = -3.d1
          parameters(11)%upper_limit = 3.d1
          parameters(11)%sigma = 1.0d0 ! THIS IS THE ERROR ON log10 g_pi
          parameters(11)%scale = 1.d0
          parameters(11)%latexname = '\log g_{\pi}'

       End if

       stop
       
    Else

       write(UNIT_FILE1,*) 'CURRENT OPTIONS FOR LIKELIHHOD ARE: gaussian OR euclid. GIVEN UNRECOGNISED OPTION IN INPUT'

       stop
       
    End if

    call set_covariance_matrix()

    call set_starting_point()
    
  end subroutine initialisation

  subroutine set_covariance_matrix()

    use input
    Implicit none
    
    Integer*4 :: index1,index2

    If (starting_cov_mat .eq. 'diagonal') then

       Do index1=1,number_of_parameters  

          Do index2=1,number_of_parameters 

             If (index1 .eq. index2) then      

                Cov_mat(index1,index2) = parameters(index1)%sigma*parameters(index2)%sigma

             Else 

                Cov_mat(index1,index2) = 0.d0

             End If

          End Do

       End Do

    Else if (starting_cov_mat .eq. 'given') then

       call read_cov_mat()

    Else

       write(UNIT_FILE1,*) 'UNRECOGNISED OPTION FOR starting_cov_mat PARAMETER'

       stop

    End If

  end subroutine set_covariance_matrix

  subroutine set_starting_point()

    use input
    use fgsl
    Implicit none

    Integer*4 :: index

    write(UNIT_FILE1,*) 'STARTING POINT IS: '
    
    If (starting_point .eq. 'mean') then

       Do index=1,number_of_parameters
          
          old_point(index) = parameters(index)%mean 
          
       End Do
       
    Else if (starting_point .eq. 'bestfit') then

       call read_bestfit()

       Do index=1,number_of_parameters
          
          old_point(index) = bestfit_point(index)
          
       End Do

    Else if (starting_point .eq. 'random') then

       Do index=1,number_of_parameters
          
          old_point(index) = fgsl_ran_flat (r, parameters(index)%lower_limit, parameters(index)%upper_limit)  
          
       End Do

    Else if (starting_point .eq. 'last_point') then

       write(UNIT_FILE1,*) 'last_point OPTION FOR starting_point PARAMETER NOT YET IMPLEMENTED'

       stop

    Else

       write(UNIT_FILE1,*) 'UNRECOGNISED OPTION FOR starting_point PARAMETER'

       stop
       
    End if

    Do index=1,number_of_parameters
          
       write(UNIT_FILE1,*) parameters(index)%name, ' =', old_point(index) 
          
    End Do

  end subroutine set_starting_point

  subroutine create_getdist_files()

    use input
    Implicit none

    Integer*4 :: index
    
    open(UNIT_FILE2,file=RANGES_FILE)

    open(UNIT_FILE3,file=PARAMNAMES_FILE)

    Do index=1,number_of_parameters

       write(UNIT_FILE2,*) parameters(index)%name, parameters(index)%lower_limit, parameters(index)%upper_limit

       write(UNIT_FILE3,*) parameters(index)%name, parameters(index)%latexname
       
    End Do
    
    close(UNIT_FILE2)

    close(UNIT_FILE3)

  end subroutine create_getdist_files

  subroutine load_data()

    use input

    Implicit none

    If (likelihood .eq. 'gaussian') then

       continue
       
    Else if (likelihood .eq. 'euclid') then

    End If
    
    Real*8, allocatable, dimension(:,:,:) :: El, Cl_fid, Cl_obs
    Real*8, allocatable, dimension(:,:,:) :: Cl_syst,Cl_current
    Real*8, allocatable, dimension(:,:) :: Nl
    
  end subroutine load_data
  
  subroutine compute_theoretical_model()

    use input

    Implicit none

    If (likelihood .eq. 'gaussian') then

       continue
       
    Else if (likelihood .eq. 'euclid') then

       write(UNIT_FILE1,*) 'WORKING WITH A FAKE EUCLID LIKELIHOOD. NOT YET IMPLEMENTED'

       stop    
       
    End if
    
  end subroutine compute_theoretical_model

  subroutine compute_ln_likelihood(point,lnlikelihood)

    use input
    use likelihoods
    
    Implicit none

    Integer*4 :: index
    Real*8,dimension(number_of_parameters) :: point
    Real*8 :: lnlikelihood
    Logical :: plausible_parameters
    Logical,dimension(number_of_parameters) :: plausibility

    Do index=1,number_of_parameters

       plausibility(index) = (point(index) .lt. parameters(index)%lower_limit) .or. &
            (point(index) .gt. parameters(index)%upper_limit) 

       If (plausibility(index)) then

          plausible_parameters = .false.

          exit

       Else

          plausible_parameters = .true.
          
       End if
       
    End Do

    If (plausible_parameters) then
       
       If (likelihood .eq. 'gaussian') then

          lnlikelihood = log_Gaussian_likelihood(point)

       Else if (likelihood .eq. 'euclid') then

          write(UNIT_FILE1,*) 'WORKING WITH A FAKE EUCLID LIKELIHOOD. NOT YET IMPLEMENTED'

          stop    

       End If

    Else

       lnlikelihood = -1.d10
       
    End If
    
  end subroutine compute_ln_likelihood

  subroutine generate_new_point_in_parameter_space()

    use input
    use fgsl

    Implicit none

    integer(fgsl_size_t), parameter :: n = number_of_parameters 
    type(fgsl_matrix) :: a
    type(fgsl_vector) :: b, x
    integer(fgsl_int) :: status
    real(fgsl_double), target :: af(n, n), bf(n), xf(n)
    
    a = fgsl_matrix_init(type=1.0_fgsl_double)
    b = fgsl_vector_init(type=1.0_fgsl_double)
    x = fgsl_vector_init(type=1.0_fgsl_double)

    af = Cov_mat

    bf = old_point
    
    status = fgsl_matrix_align(af, n, n, n, a)
    status = fgsl_vector_align(bf, n, b, n, 0_fgsl_size_t, 1_fgsl_size_t)
    status = fgsl_vector_align(xf, n, x, n, 0_fgsl_size_t, 1_fgsl_size_t)
    status = fgsl_linalg_cholesky_decomp1(a)

    status = fgsl_ran_multivariate_gaussian(r,b,a,x)

    current_point = xf
    
    call fgsl_matrix_free(a)
    call fgsl_vector_free(b)
    call fgsl_vector_free(x)
    
  end subroutine generate_new_point_in_parameter_space

  subroutine take_decision_about_current_point(m)

    use input
    use fgsl

    Implicit none

    Integer*4 :: m

    If (current_loglikelihood .ge. old_loglikelihood) then ! ACCEPTS CURRENT POINT 

       number_accepted_points = number_accepted_points + 1    

       acceptance_probability(m) = min(1.d0,exp(current_loglikelihood - old_loglikelihood))    

       If (m .le. steps_taken_before_definite_run) then

          write(UNIT_FILE5,*) weight,-old_loglikelihood,old_point(1:number_of_parameters)

       Else

          write(UNIT_FILE4,*) weight,-old_loglikelihood,old_point(1:number_of_parameters)

       End If

       weight = 1    

       old_loglikelihood = current_loglikelihood

       old_point = current_point

    Else 

       random_uniform = fgsl_ran_flat (r, 0.d0 , 1.d0) 

       If ( random_uniform .le. exp(current_loglikelihood-old_loglikelihood)) then ! ACCEPT CURRENT POINT

          number_accepted_points = number_accepted_points + 1 

          acceptance_probability(m) = min(1.d0,exp(current_loglikelihood - old_loglikelihood))    

          If (m .le. steps_taken_before_definite_run) then
          
             write(UNIT_FILE5,*) weight,-old_loglikelihood,old_point(1:number_of_parameters)
          
          else

             write(UNIT_FILE4,*) weight,-old_loglikelihood,old_point(1:number_of_parameters)

          End If

          weight = 1

          old_loglikelihood = current_loglikelihood

          old_point = current_point

       Else   ! REJECT CURRENT POINT 

          If (m .gt. steps_taken_before_definite_run) then

             number_rejected_points = number_rejected_points + 1            

          End If

          acceptance_probability(m) = min(1.d0,exp(current_loglikelihood - old_loglikelihood))    

          weight = weight + 1

       End If

    End If

  end subroutine take_decision_about_current_point

  subroutine update_covariance_matrix(m)

    use input

    Implicit none

    Integer*4 :: m
    
    If ((mod(m,jumping_factor_update) .eq. 0) .and. (m .le. steps_taken_before_definite_run) ) then

       average_ap = sum(acceptance_probability(m-jumping_factor_update+1:m))&
            /real(jumping_factor_update)

       If (average_ap .lt. lower_limit_ap) then ! DECREASE STEP SIZE

          jumping_factor = (1.d0 - step_size_changes)  

          Cov_mat = jumping_factor*Cov_mat

       Else if (average_ap .gt. upper_limit_ap) then ! INCREASE STEP SIZE 

          jumping_factor = (1.d0 + step_size_changes)     

          Cov_mat = jumping_factor*Cov_mat
          
       End If

       bad_ap = (average_ap .lt. lower_limit_ap) .or. (average_ap .gt. upper_limit_ap)

       If ( (mod(m,covariance_matrix_update) .eq. 0) .and. bad_ap ) then

          call stat(CHAIN_FILE_AUX,buff,status1)

          If ((status1 .eq. 0) .and. (buff(8) .gt. 0)) then
             
             close(UNIT_FILE5)

             call compute_cov_mat()
          
             call system('rm '//trim(CHAIN_FILE_AUX)//' ')

             open(UNIT_FILE5,file=CHAIN_FILE_AUX)

          End If

       End If

       call write_cov_mat()

       write(UNIT_FILE1,*) 'CURRENT AVERAGE ACCEPTANCE PROBABILITY IS: ',average_ap

    Else if ((mod(m,steps_taken_before_definite_run) .eq. 0) .and. (m .gt. steps_taken_before_definite_run) ) then

       average_ap = sum(acceptance_probability(m-steps_taken_before_definite_run+1:m))&
            /real(steps_taken_before_definite_run)

       bad_ap = (average_ap .lt. lower_limit_ap) .or. (average_ap .gt. upper_limit_ap)

       If (bad_ap) then
          
          write(UNIT_FILE1,*) 'BAD CURRENT AVERAGE ACCEPTANCE PROBABILITY: ',average_ap

          call write_bestfit()
          
          stop

       End If
       
    End If
    
  end subroutine update_covariance_matrix

  subroutine compute_cov_mat()

    use input
    use fgsl
    
    Implicit none

    Integer*8 :: arrays_dimension,p,index,index2,index_bestfit
    Integer :: stat
    Real*8,allocatable,dimension(:,:) :: parameters_vector
    Real*8,allocatable, dimension(:) :: lnlkl_vector    
    Integer*4,allocatable, dimension(:) :: weight_vector
    real(fgsl_double),allocatable,dimension(:,:) :: data_fgsl,data2_fgsl
    real(fgsl_double),allocatable,dimension(:) :: data4_fgsl
    real(fgsl_double),allocatable,dimension(:,:,:) :: data3_fgsl
    real(fgsl_double),dimension(number_of_parameters) :: mean_fgsl
    real(fgsl_double) :: minlnlkl
    real(fgsl_double),dimension(number_of_parameters,number_of_parameters) :: matrix
    integer(fgsl_size_t), parameter :: n = number_of_parameters
    integer(fgsl_int) :: status,signum
    type(fgsl_matrix) :: a
    real(fgsl_double), target :: af(n, n)
    type(fgsl_permutation) :: q
    real(fgsl_double) :: det_matrix
    
    a = fgsl_matrix_init(type=1.0_fgsl_double)
    q = fgsl_permutation_alloc(n)
    
    open(UNIT_FILE6,file=CHAIN_FILE_AUX)

    arrays_dimension = 0

    Do 

       read(UNIT_FILE6,*,iostat=stat)

       If (stat .ne. 0) then

          exit

       Else

          arrays_dimension = arrays_dimension + 1 

       End If

    End Do

    close(UNIT_FILE6)

    allocate (parameters_vector(1:number_of_parameters,1:arrays_dimension),&
         data2_fgsl(1:number_of_parameters,1:arrays_dimension),&
         data3_fgsl(1:number_of_parameters,1:number_of_parameters,1:arrays_dimension),&
         weight_vector(1:arrays_dimension),lnlkl_vector(1:arrays_dimension),&
         data_fgsl(1:number_of_parameters,1:arrays_dimension),&
         data4_fgsl(1:arrays_dimension),stat=status1)

    open(UNIT_FILE6,file=CHAIN_FILE_AUX)

    Do p=1,arrays_dimension

       read(UNIT_FILE6,*) weight_vector(p),lnlkl_vector(p),parameters_vector(1:number_of_parameters,p)

    End Do

    close(UNIT_FILE6)

    Do index=1,number_of_parameters
       
       Do p=1,arrays_dimension

          data_fgsl(index,p) = parameters_vector(index,p)

          If (index .eq. 1) then
             
             data4_fgsl(p) = lnlkl_vector(p)
             
          End if
          
       End Do

       mean_fgsl(index) = fgsl_stats_mean(data_fgsl(index,:), 1_fgsl_size_t, arrays_dimension)
       
    End Do

    minlnlkl = fgsl_stats_min(data4_fgsl(:), 1_fgsl_size_t, arrays_dimension)

    Do p=1,arrays_dimension

       If (minlnlkl .eq. data4_fgsl(p)) then

          index_bestfit = p

          exit
          
       End If
       
    End Do
    
    Do index=1,number_of_parameters

       bestfit_point(index) = parameters_vector(index,index_bestfit)
       
       Do p=1,arrays_dimension

          data2_fgsl(index,p) = data_fgsl(index,p) - mean_fgsl(index)
          
       End Do

    End Do

    write(UNIT_FILE1,*) 'CURRENT MIN CHI^2/2 IS: ', minlnlkl

    write(UNIT_FILE1,*) 'CURRENT BESTFIT IS: ', bestfit_point
    
    Do index=1,number_of_parameters

       Do index2=1,number_of_parameters

          Do p=1,arrays_dimension
             
             data3_fgsl(index,index2,p) = data2_fgsl(index,p)*data2_fgsl(index2,p)

          End Do

       End Do

    End Do
    
    Do index=1,number_of_parameters

       Do index2=1,number_of_parameters

          matrix(index,index2) = fgsl_stats_mean(data3_fgsl(index,index2,:), 1_fgsl_size_t, arrays_dimension) 
          
       End Do

    End Do
    
    deallocate(parameters_vector,weight_vector,lnlkl_vector,data_fgsl,data2_fgsl,data3_fgsl,data4_fgsl)

    af = matrix

    status = fgsl_matrix_align(af, n, n, n, a)
    status = fgsl_linalg_LU_decomp (a, q, signum)

    det_matrix = fgsl_linalg_LU_det(a,signum)

    If (abs(det_matrix) .gt. 0.d0) then
    
       Cov_mat = matrix

       write(UNIT_FILE1,*) 'COVARIANCE MATRIX UPDATED'
       
    Else

       write(UNIT_FILE1,*) 'SINGULAR COVARIANCE MATRIX FOUND. COVARIANCE MATRIX WAS NOT UPDATED'
       
    End If

    call fgsl_matrix_free(a)
    call fgsl_permutation_free(q)

  end subroutine compute_cov_mat

  subroutine write_cov_mat()

    use input

    Implicit none

    Integer*4 :: index1
    Character*16 :: fmt
    Character*16 :: string

    write(string,'(i2)') number_of_parameters

    fmt = '('//trim(string)//'es16.7)'

    open(UNIT_FILE7,file=COVMAT_FILE_AUX)

    Do index1=1,number_of_parameters

       write(UNIT_FILE7,fmt) Cov_mat(index1,1:number_of_parameters)

    End Do

    close(UNIT_FILE7)

  end subroutine write_cov_mat

  subroutine read_cov_mat()
    
    use input
    
    Implicit none
    
    Integer*4 :: index1
    Logical :: exist 

    inquire(file=COVMAT_FILE,exist=exist)

    If (exist) then

       open(UNIT_FILE6,file=COVMAT_FILE)

       Do index1=1,number_of_parameters

          read(UNIT_FILE6,*) Cov_mat(index1,1:number_of_parameters)

       End Do

       close(UNIT_FILE6)
       
    Else

       write(UNIT_FILE1,*) 'NO COVARIANCE MATRIX FOUND IN COVMAT FOLDER'

       stop

    End If

  end subroutine read_cov_mat

  subroutine read_bestfit()
    
    use input
    
    Implicit none
    
    Integer*4 :: index1
    Logical :: exist

    inquire(file=BESTFIT_FILE,exist=exist)

    If (exist) then
       
       open(UNIT_FILE8,file=BESTFIT_FILE)

       Do index1=1,number_of_parameters

          read(UNIT_FILE8,*) bestfit_point(index1)

       End Do

       close(UNIT_FILE8)

    Else

       write(UNIT_FILE1,*) 'NO BESTFIT FILE FOUND IN BESTFIT FOLDER'

       stop

    End if

  end subroutine read_bestfit

  subroutine write_bestfit()
    
    use input
    
    Implicit none
    
    Integer*4 :: index1
    
    open(UNIT_FILE8,file=BESTFIT_FILE_AUX)
    
    Do index1=1,number_of_parameters
       
       write(UNIT_FILE8,'(es16.7)') bestfit_point(index1)
       
    End Do
    
    close(UNIT_FILE8)
    
  end subroutine write_bestfit
  
End Module subroutines
