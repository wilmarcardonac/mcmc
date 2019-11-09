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
  
End Module subroutines
