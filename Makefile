FC 	= gfortran	# FORTRAN COMPILER
LC	= $(FC)		
EXE	= mcmc	# EXECUTABLE NAME
INCLUDE	= /usr/local/include #/fgsl
LIBRARY	= /usr/local/lib
#BUILD	= ./build/
F_FL	= -O3 -fimplicit-none -Wall -fcheck=all -I$(INCLUDE) -L$(LIBRARY) #-lfgsl #COMPILER OPTIONS. SEE GFORTRAN MANUAL FOR DETAILS: man gfortran 

OBJ = input.o likelihoods.o subroutines.o mcmc.o  #  output.o 

mcmc	 : $(OBJ)
	$(FC) $(F_FL) -o $(EXE) $(OBJ) 

mcmc.o :	mcmc.f90
	$(FC) $(F_FL) -c mcmc.f90   

input.o : input.f90
	$(FC) $(F_FL) -c input.f90 

likelihoods.o : likelihoods.f90
	$(FC) $(F_FL) -c likelihoods.f90 

subroutines.o : subroutines.f90
	$(FC) $(F_FL) -c subroutines.f90

#output.o : output.f90
#	$(FC) -c output.f90 #-J$(BUILD)

clean :
	rm -f *.o *~ *.mod $(EXE)

