Module input

  Implicit none

  save 

  Character(len=*),parameter :: likelihood = 'gaussian' ! OPTIONS: 'gaussian','euclid'

  Integer*4,parameter    :: number_iterations = 1000000 ! TOTAL NUMBER OF ITERATIONS IN MCMC RUN
  Integer*4,parameter    :: number_of_parameters = 11   ! TOTAL NUMBER VARYING PARAMETERS     
  
End Module input
