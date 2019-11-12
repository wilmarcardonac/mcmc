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
          parameters(index)%lower_limit = -1.d0
          parameters(index)%upper_limit = 1.d0
          parameters(index)%sigma = 5.d-1
          parameters(index)%scale = 1.d0
          parameters(index)%latexname = 'p_{'//trim(string)//'}'

       End Do
       
    Else if (likelihood .eq. 'euclid') then

       write(UNIT_FILE1,*) 'WORKING WITH A FAKE EUCLID LIKELIHOOD. NOT YET IMPLEMENTED'

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

    If (likelihood .eq. 'gaussian') then

       Do index1=1,number_of_parameters  

          Do index2=1,number_of_parameters 

             If (index1 .eq. index2) then      
                
                Cov_mat(index1,index2) = parameters(index1)%sigma*parameters(index2)%sigma

             Else 

                Cov_mat(index1,index2) = 0.d0

             End If

          End Do

       End Do

    Else if (likelihood .eq. 'euclid') then

       write(UNIT_FILE1,*) 'WORKING WITH A FAKE EUCLID LIKELIHOOD. NOT YET IMPLEMENTED'

       stop
       
    Else

       stop
       
    End if

    Cov_mat = jumping_factor*Cov_mat
    
  end subroutine set_covariance_matrix

  subroutine set_starting_point()

    use input
    Implicit none

    Integer*4 :: index
    
    write(UNIT_FILE1,*) 'STARTING POINT IS: '
    
    If (starting_point .eq. 'mean') then

       Do index=1,number_of_parameters
          
          old_point(index) = parameters(index)%mean 
          
       End Do
       
    Else if (starting_point .eq. 'bestfit') then

       write(UNIT_FILE1,*) 'bestfit OPTION FOR starting_point PARAMETER NOT YET IMPLEMENTED'

       stop

    Else if (starting_point .eq. 'random') then

       write(UNIT_FILE1,*) 'random OPTION FOR starting_point PARAMETER NOT YET IMPLEMENTED'

       stop

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
  
End Module subroutines
