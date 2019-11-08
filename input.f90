Module input

  Implicit none

  save 

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

  
End Module input
