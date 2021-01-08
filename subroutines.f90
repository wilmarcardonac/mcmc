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

       call system('mkdir -p '//trim(OUTPUT)//'')
       
    End if

    inquire(file=CHAINS,exist=dir_exist)

    If (dir_exist) then

       continue

    Else

       call system('mkdir -p '//trim(CHAINS)//'')

    End if

    inquire(file=DATA,exist=dir_exist)

    If (dir_exist) then

       continue

    Else

       call system('mkdir -p '//trim(DATA)//'')

    End if

    inquire(file=COVMAT,exist=dir_exist)

    If (dir_exist) then

       continue

    Else

       call system('mkdir -p '//trim(COVMAT)//'')

    End if

    inquire(file=BESTFIT,exist=dir_exist)

    If (dir_exist) then

       continue

    Else

       call system('mkdir -p '//trim(BESTFIT)//'')

    End if

    inquire(file=FIGURES,exist=dir_exist)

    If (dir_exist) then

       continue

    Else

       call system('mkdir -p '//trim(FIGURES)//'')

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

       cl_current_found = .false.

       write(UNIT_FILE1,*) 'WORKING WITH A FAKE EUCLID LIKELIHOOD'

       write(UNIT_FILE1,*) 'ANALYSIS FOR: '

       write(UNIT_FILE1,*) 'selection = ', selection

       write(UNIT_FILE1,*) 'NUMBER OF BINS = ', nbins

       write(UNIT_FILE1,*) 'LENSING = ', lensing

       call bin_centers_widths_bias()

       parameters(1)%name = 'omega_b'
       parameters(1)%mean = 2.247d-2
       parameters(1)%lower_limit = 1.d-4
       parameters(1)%upper_limit = 4.d-2
       parameters(1)%sigma = 1.3d-4
       parameters(1)%scale = 1.d0
       parameters(1)%latexname = '\omega_b'

       parameters(2)%name = 'omega_cdm'
       parameters(2)%mean = 1.193d-1
       parameters(2)%lower_limit = 1.d-4
       parameters(2)%upper_limit = 1.d0
       parameters(2)%sigma = 7.7d-4
       parameters(2)%scale = 1.d0
       parameters(2)%latexname = '\omega_{cdm}'

       parameters(3)%name = 'n_s'
       parameters(3)%mean = 9.679d-1
       parameters(3)%lower_limit = 9.d-2
       parameters(3)%upper_limit = 2.d0
       parameters(3)%sigma = 3.0d-3
       parameters(3)%scale = 1.d0
       parameters(3)%latexname = 'n_s'

       parameters(4)%name = 'ln10^{10}A_s'
       parameters(4)%mean = 3.044d0 !2.12424d-9
       parameters(4)%lower_limit = 2.0d0 !1.d-11
       parameters(4)%upper_limit = 4.0d0
       parameters(4)%sigma = 1.4d-2 !3.82d-11
       parameters(4)%scale = 1.d0
       parameters(4)%latexname = '\ln 10^{10} A_s'

       parameters(5)%name = 'H0'
       parameters(5)%mean = 6.738d1
       parameters(5)%lower_limit = 3.d1
       parameters(5)%upper_limit = 9.d1
       parameters(5)%sigma = 5.3d-1
       parameters(5)%scale = 1.d0
       parameters(5)%latexname = 'H_0'

       parameters(6)%name = 'm_ncdm'
       parameters(6)%mean = 3.1d-2
       parameters(6)%lower_limit = 0.d0
       parameters(6)%upper_limit = 5.d0
       parameters(6)%sigma = 2.7d-2
       parameters(6)%scale = 1.d0
       parameters(6)%latexname = 'm_{\nu}'

       parameters(7)%name = 'nc_bias_b0'
       parameters(7)%mean = 1.0d0
       parameters(7)%lower_limit = 0.d0
       parameters(7)%upper_limit = 3.d0
       parameters(7)%sigma = 1.0d-1
       parameters(7)%scale = 1.d0
       parameters(7)%latexname = 'b_0' 

!!$       If (number_of_parameters .eq. 10) then
!!$
!!$          parameters(8)%name = 'log10cs2_fld'
!!$          parameters(8)%mean = 0.d0
!!$          parameters(8)%lower_limit = -3.d3
!!$          parameters(8)%upper_limit = 0.d0
!!$          parameters(8)%sigma = 1.0d-1
!!$          parameters(8)%scale = 1.d0
!!$          parameters(8)%latexname = '\log c_s^2'
!!$
!!$       Else if (number_of_parameters .eq. 11) then
!!$
!!$          parameters(8)%name = 'log10ceff2' 
!!$          parameters(8)%mean = -0.0132d0
!!$          parameters(8)%lower_limit = -3.d3
!!$          parameters(8)%upper_limit = 0.d0
!!$          parameters(8)%sigma = 1.0d-1
!!$          parameters(8)%scale = 1.d0
!!$          parameters(8)%latexname = '\log c_{eff}^2'
!!$
!!$       End if

       parameters(8)%name = 'w0_fld'
       parameters(8)%mean = -9.8d-1
       parameters(8)%lower_limit = -2.d0
       parameters(8)%upper_limit = -3.d-1
       parameters(8)%sigma = 1.6d-2
       parameters(8)%scale = 1.d0
       parameters(8)%latexname = 'w'

       parameters(9)%name = 'tau_reio'
       parameters(9)%mean = 5.43d-2
       parameters(9)%lower_limit = 1.d-2
       parameters(9)%upper_limit = 8.d-1
       parameters(9)%sigma = 6.17d-3
       parameters(9)%scale = 1.d0
       parameters(9)%latexname = '\tau_{reio}'

       parameters(10)%name = 'alpha_model'
       parameters(10)%mean = 7.23d-2
       parameters(10)%lower_limit = 0.d0
       parameters(10)%upper_limit = 1.d2
       parameters(10)%sigma = 1.07d-2
       parameters(10)%scale = 1.d0
       parameters(10)%latexname = '\alpha_{model}'

!!$       If (number_of_parameters .eq. 10) then
!!$          
!!$          parameters(10)%name = 'e_pi'
!!$          parameters(10)%mean = 0.0d0
!!$          parameters(10)%lower_limit = -2.d0
!!$          parameters(10)%upper_limit = 2.d0
!!$          parameters(10)%sigma = 1.0d-1
!!$          parameters(10)%scale = 1.d0
!!$          parameters(10)%latexname = 'e_{\pi}'
!!$
!!$       Else if (number_of_parameters .eq. 11) then
!!$
!!$          parameters(10)%name = 'f_pi'
!!$          parameters(10)%mean = 5.0d0
!!$          parameters(10)%lower_limit = -3.d3
!!$          parameters(10)%upper_limit = 1.d1
!!$          parameters(10)%sigma = 1.0d-1
!!$          parameters(10)%scale = 1.d0
!!$          parameters(10)%latexname = 'f_{\pi}'
!!$
!!$          parameters(11)%name = 'log10g_pi'
!!$          parameters(11)%mean = 0.0d0 ! THIS IS ACTUALLY log10 g_pi
!!$          parameters(11)%lower_limit = -3.d1
!!$          parameters(11)%upper_limit = 3.d1
!!$          parameters(11)%sigma = 1.0d0 ! THIS IS THE ERROR ON log10 g_pi
!!$          parameters(11)%scale = 1.d0
!!$          parameters(11)%latexname = '\log g_{\pi}'
!!$
!!$       End if
    
       prior_parameters(1)%name = 'omega_b'
       prior_parameters(1)%mean = 2.247d-2

       prior_parameters(2)%name = 'omega_cdm'
       prior_parameters(2)%mean = 1.193d-1

       prior_parameters(3)%name = 'n_s'
       prior_parameters(3)%mean = 9.679d-1

       prior_parameters(4)%name = 'ln10^{10}A_s'
       prior_parameters(4)%mean = 3.044d0 !2.12424d-9

       prior_parameters(5)%name = 'H0'
       prior_parameters(5)%mean = 6.738d1

       prior_parameters(6)%name = 'w0_fld'
       prior_parameters(6)%mean = -9.8d-1

       prior_parameters(7)%name = 'tau_reio'
       prior_parameters(7)%mean = 5.43d-2

       prior_parameters(8)%name = 'alpha_model'
       prior_parameters(8)%mean = 7.23d-2

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
    Integer   :: stati

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

       open(UNIT_FILE4,file=CHAIN_FILE)
       
       Do index=1,number_iterations

          read(UNIT_FILE4,*,iostat=stati) weight,old_loglikelihood,old_point(1:number_of_parameters)

          If (stati .ne. 0) then

             exit

          Else

             continue

          End if

       End do

       close(UNIT_FILE4)

       old_loglikelihood = -old_loglikelihood

       weight = 1

       call system('head -n -1 '//trim(CHAIN_FILE)//' > '//trim(CHAIN_FILE_AUX)//'')

       call system('cp '//trim(CHAIN_FILE_AUX)//' '//trim(CHAIN_FILE)//'')

    Else

       write(UNIT_FILE1,*) 'UNRECOGNISED OPTION FOR starting_point PARAMETER'

       stop
       
    End if

    Do index=1,number_of_parameters
          
       write(UNIT_FILE1,*) parameters(index)%name, ' =', old_point(index)

       current_point(index) = old_point(index)
          
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

    Logical :: exist 

    If (likelihood .eq. 'gaussian') then

       continue

    Else if (likelihood .eq. 'euclid') then

       write(UNIT_FILE1,*) 'COMPUTING SHOT NOISE'
       
       call compute_shot_noise()

       inquire(file=CLFID_FILE,exist=exist)

       If (exist) then 

          write(UNIT_FILE1,*) 'READING FIDUCIAL CL'
          
          call read_spectra('Clfid',Cl_fid)

       Else

          write(UNIT_FILE1,*) 'COMPUTING FIDUCIAL CL'
          
          call write_ini_file(parameters(:)%mean,'Clfid')

          call compute_spectra('Clfid')

          write(UNIT_FILE1,*) 'FIDUCIAL CL COMPUTED'
          
          call read_spectra('Clfid',Cl_fid)

          write(UNIT_FILE1,*) 'FIDUCIAL CL WAS READ'

       End If

       inquire(file=CLFIDHALOFIT_FILE,exist=exist)

       If (exist) then 

          write(UNIT_FILE1,*) 'READING FIDUCIAL CL INCLUDING HALOFIT CORRECTIONS'
          
          call read_spectra('Clfidhalofit',Cl_fid_halofit)

       Else

          write(UNIT_FILE1,*) 'COMPUTING FIDUCIAL CL INCLUDING HALOFIT CORRECTIONS'
          
          call write_ini_file(parameters(:)%mean,'Clfidhalofit')

          call compute_spectra('Clfidhalofit')

          write(UNIT_FILE1,*) 'FIDUCIAL CL INCLUDING HALOFIT CORRECTIONS COMPUTED'
          
          call read_spectra('Clfidhalofit',Cl_fid_halofit)

          write(UNIT_FILE1,*) 'FIDUCIAL CL INCLUDING HALOFIT CORRECTIONS WAS READ'

       End If

       inquire(file=CLFIDNL_FILE,exist=exist)

       If (exist) then 

          write(UNIT_FILE1,*) 'READING FIDUCIAL CL NEGLECTING LENSING'
          
          call read_spectra('Clfidnl',Cl_fid_nl)

       Else

          write(UNIT_FILE1,*) 'COMPUTING FIDUCIAL CL NEGLECTING LENSING'
          
          call write_ini_file(parameters(:)%mean,'Clfidnl')

          call compute_spectra('Clfidnl')

          write(UNIT_FILE1,*) 'FIDUCIAL CL NEGLECTING LENSING COMPUTED'
          
          call read_spectra('Clfidnl',Cl_fid_nl)

          write(UNIT_FILE1,*) 'FIDUCIAL CL NEGLECTING LENSING WAS READ'

       End If

       inquire(file=CLFIDNLHALOFIT_FILE,exist=exist)

       If (exist) then 

          write(UNIT_FILE1,*) 'READING FIDUCIAL CL NEGLECTING LENSING AND INCLUDING HALOFIT CORRECTIONS'
          
          call read_spectra('Clfidnlhalofit',Cl_fid_nl_halofit)

       Else

          write(UNIT_FILE1,*) 'COMPUTING FIDUCIAL CL NEGLECTING LENSING AND INCLUDING HALOFIT CORRECTIONS'
          
          call write_ini_file(parameters(:)%mean,'Clfidnlhalofit')

          call compute_spectra('Clfidnlhalofit')

          write(UNIT_FILE1,*) 'FIDUCIAL CL NEGLECTING LENSING AND INCLUDING HALOFIT CORRECTIONS COMPUTED'
          
          call read_spectra('Clfidnlhalofit',Cl_fid_nl_halofit)

          write(UNIT_FILE1,*) 'FIDUCIAL CL NEGLECTING LENSING AND INCLUDING HALOFIT CORRECTIONS WAS READ'

       End If

!!$       inquire(file=EL_FILE,exist=exist)
!!$
!!$       If (exist) then 
!!$
!!$          write(UNIT_FILE1,*) 'READING ERROR FILE INCLUDING LENSING'
!!$          
!!$          call read_spectra('El',El)
!!$
!!$       Else
!!$
!!$          write(UNIT_FILE1,*) 'COMPUTING ERROR FILE INCLUDING LENSING'
!!$          
!!$          call write_ini_file(parameters(:)%mean,'El')
!!$
!!$          call compute_spectra('El')
!!$
!!$          call read_spectra('El',El)
!!$
!!$       End If
!!$
!!$       If (lensing) then
!!$
!!$          continue
!!$
!!$       Else
!!$
!!$          inquire(file=ELNL_FILE,exist=exist)
!!$
!!$          If (exist) then 
!!$
!!$             write(UNIT_FILE1,*) 'READING ERROR FILE NEGLECTING LENSING'
!!$
!!$             call read_spectra('Elnl',Elnl)
!!$
!!$          Else
!!$
!!$             write(UNIT_FILE1,*) 'COMPUTING ERROR FILE NEGLECTING LENSING'
!!$
!!$             call write_ini_file(parameters(:)%mean,'Elnl')
!!$
!!$             call compute_spectra('Elnl')
!!$
!!$             call read_spectra('Elnl',Elnl)
!!$
!!$          End If
!!$
!!$       End If

       write(UNIT_FILE1,*) 'COMPUTING OBSERVED CL'
       
       call compute_observed_Cl()

       If (use_gaussian_planck_prior) then

          call read_prior_cov()

          call compute_inv_prior_cov(prior_cov)

       Else

          continue

       End if
       
    End If

  end subroutine load_data

  subroutine compute_spectra(spectra)
    
    use input
    
    Implicit none

    Character(len=*) :: spectra

    If ( ( (spectra .eq. 'Clfid') .or. (spectra .eq. 'Clfidhalofit') ) .or. ( (spectra .eq. 'Clfidnl')&
         .or. (spectra .eq. 'Clfidnlhalofit') ) ) then
       
       call system(''//trim(CLASS_EXECUTABLE)//'/./class '//trim(INI_FILE)//' '//trim(HIGH_PRE)//'')

    Else if (spectra .eq. 'Cl') then

       If ( (selection .eq. 'tophat') .and. (nbins .eq. 5) ) then
          
          call system(''//trim(CLASS_EXECUTABLE)//'/./class '//trim(INI_FILE)//' '//trim(LOW_PRE_T5)//'')       

       Else if ( (selection .eq. 'tophat') .and. (nbins .eq. 10) ) then
          
          call system(''//trim(CLASS_EXECUTABLE)//'/./class '//trim(INI_FILE)//' '//trim(LOW_PRE_T10)//'')       

       Else if ( (selection .eq. 'gaussian') .and. (nbins .eq. 5) ) then

          call system(''//trim(CLASS_EXECUTABLE)//'/./class '//trim(INI_FILE)//' '//trim(LOW_PRE_G5)//'')

       Else if ( (selection .eq. 'gaussian') .and. (nbins .eq. 10) ) then

          call system(''//trim(CLASS_EXECUTABLE)//'/./class '//trim(INI_FILE)//' '//trim(LOW_PRE_G10)//'')
          
       End If
       
    Else

       write(UNIT_FILE1,*) 'UNRECOGNISED OPTION IN subroutine compute_spectra'
       
    End if
       
  end subroutine compute_spectra

  subroutine read_spectra(spectra,Cl)
    
    use input
    
    Implicit none

    Character(len=*) :: spectra
    
    Real*8,dimension(lmin:lmax,0:nbins,0:nbins) :: Cl
    Integer*4 m,p,i

    Logical :: exist

    If (spectra .eq. 'Clfid') then

       inquire(file=CLFID_FILE,exist=exist)

       If (exist) then
          
          open(UNIT_FILE10,file=CLFID_FILE)

       End If

    Else if (spectra .eq. 'Clfidnl') then

       inquire(file=CLFIDNL_FILE,exist=exist)

       If (exist) then
          
          open(UNIT_FILE10,file=CLFIDNL_FILE)

       End If

    Else if (spectra .eq. 'Clfidhalofit') then

       inquire(file=CLFIDHALOFIT_FILE,exist=exist)

       If (exist) then
          
          open(UNIT_FILE10,file=CLFIDHALOFIT_FILE)

       End If

    Else if (spectra .eq. 'Clfidnlhalofit') then

       inquire(file=CLFIDNLHALOFIT_FILE,exist=exist)

       If (exist) then
          
          open(UNIT_FILE10,file=CLFIDNLHALOFIT_FILE)

       End If
       
    Else if (spectra .eq. 'Cl') then

       inquire(file=CL_FILE,exist=exist)

       If (exist) then
          
          open(UNIT_FILE10,file=CL_FILE)

       End If
       
    Else

       write(UNIT_FILE1,*) 'UNRECOGNISED OPTION IN subroutine read_spectra'

       stop
       
    End If

    If (exist) then

       cl_current_found = .true. 

       Do m=-5,lmax

          If (m .le. 1) then

             read(UNIT_FILE10,*)

          else

             If (nbins .eq. 5) then

                read(UNIT_FILE10,*) Cl(m,0,0),Cl(m,1,1),Cl(m,1,2),Cl(m,1,3),Cl(m,1,4),Cl(m,1,5),&
                     Cl(m,2,2),Cl(m,2,3),Cl(m,2,4),Cl(m,2,5),Cl(m,3,3),Cl(m,3,4),Cl(m,3,5),&
                     Cl(m,4,4),Cl(m,4,5),Cl(m,5,5)

             Else if (nbins .eq. 10) then

                read(UNIT_FILE10,*) Cl(m,0,0),Cl(m,1,1),Cl(m,1,2),Cl(m,1,3),Cl(m,1,4),Cl(m,1,5),&
                     Cl(m,1,6),Cl(m,1,7),Cl(m,1,8),Cl(m,1,9),Cl(m,1,10),Cl(m,2,2),Cl(m,2,3),&
                     Cl(m,2,4),Cl(m,2,5),Cl(m,2,6),Cl(m,2,7),Cl(m,2,8),Cl(m,2,9),Cl(m,2,10),&
                     Cl(m,3,3),Cl(m,3,4),Cl(m,3,5),Cl(m,3,6),Cl(m,3,7),Cl(m,3,8),Cl(m,3,9),&
                     Cl(m,3,10),Cl(m,4,4),Cl(m,4,5),Cl(m,4,6),Cl(m,4,7),Cl(m,4,8),Cl(m,4,9),&
                     Cl(m,4,10),Cl(m,5,5),Cl(m,5,6),Cl(m,5,7),Cl(m,5,8),Cl(m,5,9),Cl(m,5,10),&
                     Cl(m,6,6),Cl(m,6,7),Cl(m,6,8),Cl(m,6,9),Cl(m,6,10),Cl(m,7,7),Cl(m,7,8),&
                     Cl(m,7,9),Cl(m,7,10),Cl(m,8,8),Cl(m,8,9),Cl(m,8,10),Cl(m,9,9),Cl(m,9,10),&
                     Cl(m,10,10)

             End if

             Do p=1,nbins

                Do i=1,nbins

                   If (p .gt. i) then

                      Cl(m,p,i) = Cl(m,i,p)

                   End If

                End Do

             End Do

          End If

       End Do

       close(UNIT_FILE10)
       
    Else

       cl_current_found = .false.

    End If
    
  end subroutine read_spectra
  
  subroutine compute_shot_noise()

    use input

    Implicit none

    Integer*4 :: m,n

    Do m = 1,nbins

       Do n = 1,nbins

          If (m .ne. n) then

             Nl(m,n) = 0.d0

          else

             Nl(m,n) = real(nbins)*(1.d0/(3600.d0*gal_per_sqarcmn*(180.d0/Pi)**2))

          End If

       End Do

    End Do

  end subroutine compute_shot_noise

  subroutine compute_observed_Cl()

    use input
    
    Implicit none

    Integer*4 :: m,p,i

    Do m=lmin,lmax

       Do p=1,nbins

          Do i=1,nbins

             El(m,p,i) = abs(Cl_fid_halofit(m,p,i) - Cl_fid(m,p,i))

             Elnl(m,p,i) = abs(Cl_fid_nl_halofit(m,p,i) - Cl_fid_nl(m,p,i)) 

             Cl_obs(m,p,i) = 2.d0*Pi*(Cl_fid(m,p,i) + El(m,p,i))/real(m)/(real(m)+1.d0) + Nl(p,i) 

          End Do

       End Do

    End Do

  end subroutine compute_observed_Cl

  subroutine compute_theoretical_model()

    use input

    Implicit none

    Integer*4 :: index
    Real*8,dimension(number_of_parameters) :: point
    Logical :: plausible_parameters
    Logical,dimension(number_of_parameters) :: plausibility

    Do index=1,number_of_parameters

       point(index) = current_point(index)

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

          cl_current_found = .true.

          continue

       Else if (likelihood .eq. 'euclid') then

          call write_ini_file(current_point,'Cl')

          call compute_spectra('Cl')

          call read_spectra('Cl',Cl_current)

       End if

    Else

       cl_current_found = .false.
       
    End if
 
  end subroutine compute_theoretical_model

  subroutine compute_ln_likelihood(point,lnlikelihood)

    use input
    
    Implicit none

    Real*8,dimension(number_of_parameters) :: point
    Real*8 :: lnlikelihood

    If (cl_current_found) then
       
       If (likelihood .eq. 'gaussian') then

          lnlikelihood = log_Gaussian_likelihood(point)

       Else if (likelihood .eq. 'euclid') then
          
          lnlikelihood = euclid_galaxy_cl_likelihood(Cl_current)

       End if
       
    Else
             
       lnlikelihood = -1.d10 

    End if
          
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

    Else if ((mod(m,covariance_matrix_update) .eq. 0) .and. (m .gt. steps_taken_before_definite_run) ) then

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
    integer(fgsl_int) :: status!,signum
    type(fgsl_matrix) :: a
    real(fgsl_double), target :: af(n, n)
    type(fgsl_permutation) :: q
    !real(fgsl_double) :: det_matrix
    type(fgsl_error_handler_t) :: std, off
    
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

    std = fgsl_set_error_handler_off()

    status = fgsl_linalg_cholesky_decomp1(a)

    off = fgsl_set_error_handler(std)

    If (status .eq. fgsl_edom) then

       write(UNIT_FILE1,*) 'COVARIANCE MATRIX IS NOT POSITIVE-DEFINITE'

       write(UNIT_FILE1,*) 'COVARIANCE MATRIX WAS NOT UPDATED'

    Else

       Cov_mat = matrix
       
       write(UNIT_FILE1,*) 'COVARIANCE MATRIX UPDATED'
    
    End If
       
    call fgsl_matrix_free(a)

    call fgsl_permutation_free(q)

  end subroutine compute_cov_mat

  function compute_determinant(matrix)

    use input
    use fgsl
    
    Implicit none
    
    real(fgsl_double),dimension(nbins,nbins) :: matrix
    integer(fgsl_size_t), parameter :: n = nbins
    integer(fgsl_int) :: status,signum
    type(fgsl_matrix) :: a
    real(fgsl_double), target :: af(n, n)
    type(fgsl_permutation) :: q
    real(fgsl_double) :: compute_determinant
    
    a = fgsl_matrix_init(type=1.0_fgsl_double)
    q = fgsl_permutation_alloc(n)

    af = matrix

    status = fgsl_matrix_align(af, n, n, n, a)
    status = fgsl_linalg_LU_decomp (a, q, signum)

    compute_determinant = fgsl_linalg_LU_det(a,signum)

    call fgsl_matrix_free(a)
    call fgsl_permutation_free(q)

  end function compute_determinant

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
    use fgsl
    
    Implicit none
    
    Integer*4 :: index1
    Logical :: exist 

    integer(fgsl_size_t), parameter :: n = number_of_parameters 
    type(fgsl_matrix) :: a
    integer(fgsl_int) :: status
    real(fgsl_double), target :: af(n, n)

    type(fgsl_error_handler_t) :: std, off

    inquire(file=COVMAT_FILE,exist=exist)

    If (exist) then

       open(UNIT_FILE6,file=COVMAT_FILE)

       Do index1=1,number_of_parameters

          read(UNIT_FILE6,*) Cov_mat(index1,1:number_of_parameters)

       End Do

       close(UNIT_FILE6)

       af = Cov_mat

       a = fgsl_matrix_init(type=1.0_fgsl_double)
    
       status = fgsl_matrix_align(af, n, n, n, a)

       std = fgsl_set_error_handler_off()
       
       status = fgsl_linalg_cholesky_decomp1(a)

       off = fgsl_set_error_handler(std)
       
       call fgsl_matrix_free(a)

       If (status .eq. fgsl_edom) then

          write(UNIT_FILE1,*) 'COVARIANCE MATRIX IS NOT POSITIVE-DEFINITE'

          stop
          
       Else

          continue
          
       End If
       
    Else

       write(UNIT_FILE1,*) 'NO COVARIANCE MATRIX FOUND IN COVMAT FOLDER'

       stop

    End If

  end subroutine read_cov_mat

  subroutine read_prior_cov()
    
    use input
    use fgsl

    Implicit none

    Integer*4 :: index1
    Logical :: exist 

    integer(fgsl_size_t), parameter :: n = number_of_prior_parameters 
    type(fgsl_matrix) :: a
    integer(fgsl_int) :: status
    real(fgsl_double), target :: af(n, n)

    type(fgsl_error_handler_t) :: std, off

    inquire(file=PRIOR_COVMAT_FILE,exist=exist)

    If (exist) then

       open(UNIT_FILE11,file=PRIOR_COVMAT_FILE)

       Do index1=1,number_of_prior_parameters

          read(UNIT_FILE11,*) prior_cov(index1,1:number_of_prior_parameters)

       End Do

       close(UNIT_FILE11)

       af = prior_cov

       a = fgsl_matrix_init(type=1.0_fgsl_double)

       status = fgsl_matrix_align(af, n, n, n, a)

       std = fgsl_set_error_handler_off()

       status = fgsl_linalg_cholesky_decomp1(a)

       off = fgsl_set_error_handler(std)

       call fgsl_matrix_free(a)

       If (status .eq. fgsl_edom) then

          write(UNIT_FILE1,*) 'PRIOR COVARIANCE MATRIX IS NOT POSITIVE-DEFINITE'

          stop

       Else

          continue

       End If

    Else

       write(UNIT_FILE1,*) 'NO PRIOR COVARIANCE MATRIX FOUND IN COVMAT FOLDER'

       stop

    End If

  end subroutine read_prior_cov

  subroutine compute_inv_prior_cov(matrix)

    use input
    use fgsl
    
    Implicit none
    
    real(fgsl_double),dimension(number_of_prior_parameters,number_of_prior_parameters) :: matrix
    integer(fgsl_size_t), parameter :: n = number_of_prior_parameters
    integer(fgsl_int) :: status,signum
    type(fgsl_matrix) :: a
    real(fgsl_double), target :: af(n, n)
    real(fgsl_double), pointer :: matrixb(:,:)
    type(fgsl_permutation) :: q
    type(fgsl_error_handler_t) :: std,off
    
    a = fgsl_matrix_init(type=1.0_fgsl_double)
    q = fgsl_permutation_alloc(n)

    af = matrix

    status = fgsl_matrix_align(af, n, n, n, a)

    std = fgsl_set_error_handler_off()

    status = fgsl_linalg_cholesky_decomp1(a)

    off = fgsl_set_error_handler(std)

    If (status .eq. fgsl_edom) then

       write(UNIT_FILE1,*) 'PRIOR COVARIANCE MATRIX IS NOT POSITIVE-DEFINITE'

    Else

       write(UNIT_FILE1,*) 'PRIOR COVARIANCE MATRIX IS POSITIVE-DEFINITE'

    End If

    status = fgsl_linalg_cholesky_invert(a)

    status = fgsl_matrix_align(matrixb,a)

    inv_prior_cov = matrixb

    call fgsl_matrix_free(a)

    call fgsl_permutation_free(q)

  end subroutine compute_inv_prior_cov

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

  subroutine write_ini_file(point,spectra)

    use input

    Implicit none

    Character(len=*) :: spectra 

    Integer*4 :: index

    Real*8,dimension(number_of_parameters) :: point

    open(UNIT_FILE9,file=INI_FILE)

    If (spectra .eq. 'Clfidnl') then 

       write(UNIT_FILE9,*) 'root = '//trim(DATA)//'/Clfidnl_'

    Else if (spectra .eq. 'Clfidhalofit') then 

       write(UNIT_FILE9,*) 'root = '//trim(DATA)//'/Clfidhalofit_'

       write(UNIT_FILE9,'(a20)') 'non linear = halofit'

    Else if (spectra .eq. 'Clfidnlhalofit') then 

       write(UNIT_FILE9,*) 'root = '//trim(DATA)//'/Clfidnlhalofit_'

       write(UNIT_FILE9,'(a20)') 'non linear = halofit'

    Else if (spectra .eq. 'Clfid') then

       write(UNIT_FILE9,*) 'root = '//trim(DATA)//'/Clfid_'
       
    Else if (spectra .eq. 'Cl') then

       write(UNIT_FILE9,*) 'root = '//trim(OUTPUT)//'/Cl_'

    Else

       write(UNIT_FILE1,*) 'UNRECOGNISED OPTION IN subroutine write_ini_file'
       
    End If

    If (lensing) then

       write(UNIT_FILE9,'(a59)') 'number count contributions = density, rsd, lensing, doppler'

    Else

       write(UNIT_FILE9,'(a50)') 'number count contributions = density, rsd, doppler'

    End if

    If ( ( (spectra .eq. 'Clfid') .or. (spectra .eq. 'Clfidnl') ) .or. ( (spectra .eq. 'Clfidhalofit')&
         .or. (spectra .eq. 'Clfidnlhalofit') ) ) then

       Do index=1,number_of_parameters

          write(UNIT_FILE9,*) parameters(index)%name//trim(' =')//' ', parameters(index)%mean

       End Do

    Else

       Do index=1,number_of_parameters

          write(UNIT_FILE9,*) parameters(index)%name//trim(' =')//' ', point(index)

       End Do

    End If

    write(UNIT_FILE9,'(a12)') 'cs2_fld = 1.'

    write(UNIT_FILE9,'(a12)') 'use_ppf = no'

    write(UNIT_FILE9,'(a7, f5.3)') 'N_ur = ', real(N_ur)

    write(UNIT_FILE9,'(a9, f5.3)') 'N_ncdm = ', real(N_ncdm)

    write(UNIT_FILE9,'(a11, f5.3)') 'deg_ncdm = ', real(deg_ncdm)

    write(UNIT_FILE9,'(a10,f5.3)') 'Omega_k = ', real(0.)

    write(UNIT_FILE9,'(a15,f5.3)') 'Omega_Lambda = ', real(0.)

    write(UNIT_FILE9,'(a12)') 'output = nCl'

    write(UNIT_FILE9,'(a25)') 'dNdz_selection = analytic'

    write(UNIT_FILE9,'(a25)') 'dNdz_evolution = analytic'

    write(UNIT_FILE9,*) 'selection = '//trim(selection)//' '

    If (nbins .eq. 5) then

       write(UNIT_FILE9,'(a17, 4(f10.8, a1),f10.8)') 'selection_mean = ', z_bin_centers(1),',', z_bin_centers(2),',',&
            z_bin_centers(3),',',z_bin_centers(4),',',z_bin_centers(5)
       
       write(UNIT_FILE9,'(a18, 4(f10.8, a1),f10.8)') 'selection_width = ', z_bin_widths(1),',',z_bin_widths(2),',',&
            z_bin_widths(3),',',z_bin_widths(4),',',z_bin_widths(5)

       write(UNIT_FILE9,'(a17, 4(f10.8, a1),f10.8)') 'selection_bias = ', z_bin_bias(1),',',z_bin_bias(2),',',&
            z_bin_bias(3),',',z_bin_bias(4),',',z_bin_bias(5)

       write(UNIT_FILE9,'(a31, 4(f10.8, a1),f10.8)') 'selection_magnification_bias = ', s_z_mag_bias(1),',',&
            s_z_mag_bias(2),',',s_z_mag_bias(3),',',s_z_mag_bias(4),',',s_z_mag_bias(5)

    Else if (nbins .eq. 10) then
       
       write(UNIT_FILE9,'(a17, 9(f10.8, a1),f10.8)') 'selection_mean = ', z_bin_centers(1),',', z_bin_centers(2),',',&
            z_bin_centers(3),',',z_bin_centers(4),',',z_bin_centers(5),',',z_bin_centers(6),',',z_bin_centers(7),',',&
            z_bin_centers(8),',',z_bin_centers(9),',',z_bin_centers(10)

       write(UNIT_FILE9,'(a18, 9(f10.8, a1),f10.8)') 'selection_width = ', z_bin_widths(1),',',z_bin_widths(2),',',&
            z_bin_widths(3),',',z_bin_widths(4),',',z_bin_widths(5),',',z_bin_widths(6),',',z_bin_widths(7),',',&
            z_bin_widths(8),',',z_bin_widths(9),',',z_bin_widths(10)

       write(UNIT_FILE9,'(a17, 9(f10.8, a1),f10.8)') 'selection_bias = ', z_bin_bias(1),',',z_bin_bias(2),',',&
            z_bin_bias(3),',',z_bin_bias(4),',',z_bin_bias(5),',',z_bin_bias(6),',',z_bin_bias(7),',',z_bin_bias(8),',',&
            z_bin_bias(9),',',z_bin_bias(10)

       write(UNIT_FILE9,'(a31, 9(f10.8, a1),f10.8)') 'selection_magnification_bias = ', s_z_mag_bias(1),',',&
            s_z_mag_bias(2),',',s_z_mag_bias(3),',',s_z_mag_bias(4),',',s_z_mag_bias(5),',',s_z_mag_bias(6),',',&
            s_z_mag_bias(7),',',s_z_mag_bias(8),',',s_z_mag_bias(9),',',s_z_mag_bias(10)

    Else

       write(UNIT_FILE1,*) 'nbins MUST BE EITHER 5 OR 10'

       stop
       
    End if
    
    write(UNIT_FILE9,'(a15,i2)') 'non_diagonal = ',nbins-1

    write(UNIT_FILE9,'(a13)') 'headers = yes'

    write(UNIT_FILE9,'(a17)') 'bessel file = yes'

    write(UNIT_FILE9,'(a12,i4)') 'l_max_lss = ', lmax_class

    write(UNIT_FILE9,'(a8,i1)') 'l_min = ', lmin

    write(UNIT_FILE9,'(a14)') 'format = class'

    write(UNIT_FILE9,'(a17)') 'gauge = newtonian'

    close(UNIT_FILE9)

  end subroutine write_ini_file
  
  subroutine bin_centers_widths_bias()

    use input

    Implicit none

    Real*8 :: n_tot, gd_1, gd_2, gal_count, z, delta_z
    Integer*4 :: m,n,i
    Integer*4 :: p,nb

    nb = nbins + 1

    delta_z = (zmax - zmin)/real(nbins)

    If (selection .eq. 'gaussian') then
       
       p = int((zmax - zmin)/dz)


       z_array(1) = zmin

       Do m=2,p

          z_array(m) = z_array(m-1) + dz

       End do

       z_array(p) = zmax

       n_tot = 0.d0

       Do m=1,p-1

          gd_1 = galaxy_distribution(z_array(m))

          gd_2 = galaxy_distribution(z_array(m+1))

          n_tot = (gd_1 + gd_2)/2.*dz + n_tot 

       End Do

       z_bin_edges(1) = zmin

       Do n=1,nbins

          gal_count = 0.d0

          z = z_bin_edges(n)

          Do i=1,10000

             If (gal_count .gt. n_tot/nbins) exit

             gd_1 = galaxy_distribution(z)

             gd_2 = galaxy_distribution(z+dz)

             gal_count = (gd_1 + gd_2)/2.*dz + gal_count

             z = z + dz 

          End Do

          z_bin_edges(n+1) = z

       End Do

       z_bin_edges(nbins+1) = zmax

       Do n=2,nbins+1

          z_bin_centers(n-1) = (z_bin_edges(n) + z_bin_edges(n-1))/2.

          z_bin_widths(n-1) = (z_bin_edges(n) - z_bin_edges(n-1))/2.

          z_bin_bias(n-1) = sqrt(1.d0 + z_bin_centers(n-1))

          s_z_mag_bias(n-1) = s_0 + s_1*z_bin_centers(n-1) + s_2*z_bin_centers(n-1)**2 + s_3*z_bin_centers(n-1)**3

       End Do

    Else if (selection .eq. 'tophat') then

       z_bin_edges(1) = zmin

       Do n=2,nbins

          z_bin_edges(n) = z_bin_edges(n-1) + delta_z

       End Do

       z_bin_edges(nb) = zmax

       Do n=1,nbins 

          z_bin_centers(n) = (z_bin_edges(n+1) + z_bin_edges(n))/2.d0
          
          z_bin_widths(n) = (z_bin_edges(n+1) - z_bin_edges(n))/2.d0

       End Do

       Do n=2,nb

          z_bin_bias(n-1) = sqrt(1.d0 + z_bin_centers(n-1))

          s_z_mag_bias(n-1) = s_0 + s_1*z_bin_centers(n-1) + s_2*z_bin_centers(n-1)**2 + s_3*z_bin_centers(n-1)**3

       End Do

    Else

       write(UNIT_FILE1,*) 'UNRECOGNISED OPTION FOR selection'

       stop

    End If

  end subroutine bin_centers_widths_bias

  function galaxy_distribution(z)

    Implicit none

    Real*8 :: zmean,z0,z,galaxy_distribution

    zmean = 0.9d0

    z0 = zmean/1.412d0

    galaxy_distribution = z**2*exp(-(z/z0)**(1.5))

  end function galaxy_distribution

  function log_Gaussian_likelihood(array)

    use input

    Implicit none

    Integer*4 :: index
    Real*8 :: log_Gaussian_likelihood
    Real*8,dimension(number_of_parameters) :: array

    log_Gaussian_likelihood = 0.d0

    Do index=1,number_of_parameters

       log_Gaussian_likelihood = array(index)**2 + log_Gaussian_likelihood

    End Do

    log_Gaussian_likelihood = -log_Gaussian_likelihood/2.d0

  end function log_Gaussian_likelihood

  function euclid_galaxy_cl_likelihood(Cl)

    use input

    Implicit none

    Integer*4,parameter :: L = lmax - lmin + 1
    Integer*4 :: indexl,indexbin_i,indexbin_j,indexbin_k,indexbin_p
    Real*8,parameter :: epsilon_min = 0.d0
    Real*8,parameter :: epsilon_max = 1.d2
    Real*8,dimension(lmin:lmax,0:nbins,0:nbins) :: Clth,Cl,Elth
    Real*8,dimension(1:nbins,1:nbins) :: Cov_mix,Cov_obs,Cov_the!,Cov_the_El!,Cov_mix_new
    Real*8 :: euclid_galaxy_cl_likelihood,chi2,det_obs,det_the,det_mix!,det_the_El!,det_the_El_mix!,epsilon_l
    !    Real*8 :: nss,ass,h00,ob,ocdm
    !    Real*8,dimension(5) :: param_vector
    !    Real*8,parameter,dimension(5) :: fiducial_vector = [omega_b,omega_cdm,n_s,dlog(1.d10*A_s),H0]

    !    param_vector(1) = ob
    !    param_vector(2) = ocdm
    !    param_vector(3) = nss
    !    param_vector(4) = dlog(1.d10*ass)
    !    param_vector(5) = h00

    Do indexl=lmin,lmax

       Do indexbin_i=1,nbins

          Do indexbin_j=1,nbins

             Clth(indexl,indexbin_i,indexbin_j) = 2.d0*Pi*(Cl(indexl,indexbin_i,indexbin_j) + &
                  El(indexl,indexbin_i,indexbin_j) )/dble(indexl)/(dble(indexl) + 1.d0) + Nl(indexbin_i,indexbin_j)

             If (lensing) then
                
                Elth(indexl,indexbin_i,indexbin_j) = 2.d0*Pi*El(indexl,indexbin_i,indexbin_j)/&
                     dble(indexl)/(dble(indexl) + 1.d0)*sqrt(dble(L))

             Else

                Elth(indexl,indexbin_i,indexbin_j) = 2.d0*Pi*Elnl(indexl,indexbin_i,indexbin_j)/&
                     dble(indexl)/(dble(indexl) + 1.d0)*sqrt(dble(L))

             End If
             
          End Do

       End Do

    End Do

    chi2 = 0.d0

    Do indexl=lmin,lmax

       Do indexbin_i=1,nbins

          Do indexbin_j=1,nbins

             Cov_obs(indexbin_i,indexbin_j) = Cl_obs(indexl,indexbin_i,indexbin_j)

             Cov_the(indexbin_i,indexbin_j) = Clth(indexl,indexbin_i,indexbin_j)

          End Do

       End Do

       det_obs = compute_determinant(Cov_obs)

       det_the = compute_determinant(Cov_the)

       det_mix = 0.d0

!!$       If (theoreticalerror .gt. 0.d0) then 
!!$
!!$          Do  indexbin_k=1,nbins
!!$
!!$             Do indexbin_i=1,nbins
!!$
!!$                Do indexbin_j=1,nbins
!!$
!!$                   Cov_mix(indexbin_i,indexbin_j) = Clth(indexl,indexbin_i,indexbin_j)
!!$
!!$                End Do
!!$
!!$             End Do
!!$
!!$             Do indexbin_p=1,nbins
!!$
!!$                Cov_mix(indexbin_p,indexbin_k) = Cl_obs(indexl,indexbin_p,indexbin_k) 
!!$
!!$             End Do
!!$
!!$             det_mix = compute_determinant(Cov_mix) + det_mix
!!$
!!$          End Do
!!$
!!$          ! Here function to minimize chi2 w.r.t must be called  
!!$
!!$          epsilon_l = 0.d0 ! In the meantime we disregard epsilon_l (output of function above)
!!$
!!$          !If ( (epsilon_l-epsilon_min < 1.d-5/dble(L)) .or. (epsilon_max-epsilon_l<1.d-5/dble(L)) ) then 
!!$          !    print *,'Minimization did not converge for ', indexl, 'having epsilon_l equal to ',epsilon_l
!!$          !End If
!!$
!!$          Do indexbin_i=1,nbins
!!$
!!$             Do indexbin_j=1,nbins
!!$
!!$                Cov_the_El(indexbin_i,indexbin_j) = Clth(indexl,indexbin_i,indexbin_j)&
!!$                     + epsilon_l*Elth(indexl,indexbin_i,indexbin_j)
!!$
!!$             End Do
!!$
!!$          End Do
!!$
!!$          det_the_El = compute_determinant(Cov_the_El)
!!$
!!$          det_the_El_mix = 0.d0
!!$
!!$          Do  indexbin_k=1,nbins
!!$
!!$             Do indexbin_i=1,nbins
!!$
!!$                Do indexbin_j=1,nbins
!!$
!!$                   Cov_mix_new(indexbin_i,indexbin_j) = Clth(indexl,indexbin_i,indexbin_j)&
!!$                        + epsilon_l*Elth(indexl,indexbin_i,indexbin_j)
!!$
!!$                End Do
!!$
!!$             End Do
!!$
!!$             Do indexbin_p=1,nbins
!!$
!!$                Cov_mix_new(indexbin_p,indexbin_k) = Cl_obs(indexl,indexbin_p,indexbin_k) 
!!$
!!$             End Do
!!$
!!$             det_the_El_mix = compute_determinant(Cov_mix_new) + det_the_El_mix
!!$
!!$          End Do
!!$
!!$          chi2 = fsky*(2.d0*dble(indexl)+1.d0)*(log(det_the_El/det_obs) + det_the_El_mix/det_the_El &
!!$               - dble(nbins)) + chi2 + epsilon_l**2
!!$
!!$       Else

       Do  indexbin_k=1,nbins

          Do indexbin_i=1,nbins

             Do indexbin_j=1,nbins

                Cov_mix(indexbin_i,indexbin_j) = Clth(indexl,indexbin_i,indexbin_j)

             End Do

          End Do

          Do indexbin_p=1,nbins

             Cov_mix(indexbin_p,indexbin_k) = Cl_obs(indexl,indexbin_p,indexbin_k) 

          End Do

          det_mix = compute_determinant(Cov_mix) + det_mix

       End Do

       chi2 = fsky*(2.d0*dble(indexl)+1.d0)*(log(det_the/det_obs) + det_mix/det_the - dble(nbins)) + chi2

!!$       End if

    End Do

    If (use_gaussian_planck_prior) then 

       Do indexbin_k=1,number_of_parameters

          Do indexbin_p=1,number_of_parameters

             Do indexbin_i=1,number_of_prior_parameters

                Do indexbin_j=1,number_of_prior_parameters

                   If ( (parameters(indexbin_k)%name .eq. prior_parameters(indexbin_i)%name) .and. &
                        (parameters(indexbin_p)%name .eq. prior_parameters(indexbin_j)%name) ) then 

                   
                      chi2 = (prior_parameters(indexbin_i)%mean-current_point(indexbin_k))*inv_prior_cov(indexbin_i,&
                           indexbin_j)*(prior_parameters(indexbin_j)%mean - current_point(indexbin_p)) + chi2 

                   Else

                      continue

                   End if

                End Do

             End Do

          End Do

       End Do

    Else

       continue

    End If

    If (abs(chi2).ge.0.d0) then

       euclid_galaxy_cl_likelihood = -chi2/2.d0   

    Else

       euclid_galaxy_cl_likelihood = -1.d10     

    End If

  end function euclid_galaxy_cl_likelihood

End Module subroutines
