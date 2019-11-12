Module likelihoods

  Implicit none
     
contains

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

End Module likelihoods
