
#FC  = gfortran
FC  = ifort
#FFLAGS := -I$(INC_NETCDF) -L$(LIB_NETCDF) -lnetcdff -lnetcdf -g -fbacktrace -fcheck=all -DSCMDIAG 
#FFLAGS := -I$(INC_NETCDF) -L$(LIB_NETCDF) -lnetcdff -lnetcdf -fcheck=all -fbacktrace
#FFLAGS := -I$(INC_NETCDF) -L$(LIB_NETCDF) -lnetcdff -lnetcdf -DSCMDIAG -fbounds-check -g
#FFLAGS := -I$(INC_NETCDF) -L$(LIB_NETCDF) -lnetcdff -lnetcdf -DSCMDIAG -fbounds-check -g -check all noarg_temp_created -fpe0 -traceback
FFLAGS := -I$(INC_NETCDF) -L$(LIB_NETCDF) -lnetcdff -lnetcdf -DSCMDIAG -fbounds-check -g -check all -check noarg_temp_created -fpe0 -traceback

#FFLAGS := -I$(INC_NETCDF) -L$(LIB_NETCDF) -lnetcdff -lnetcdf -fbounds-check 

VPATH = ../src

SRC = scmdiag.f90 conv_jp.F90 buoysort.f90 shr_nl_mod.F90 nnparameter.F90

OBJ = $(addsuffix .o, $(basename $(SRC)))

all: main

main: $(OBJ) main.o
	$(FC) $(OBJ) main.o -o $@ $(FFLAGS)

%.o : %.F90
	$(FC) -c $< $(FFLAGS)

%.o : %.f90
	$(FC) -c $< $(FFLAGS)

clean:
	rm *.o *.mod

#dependence

nnparameter.o : shr_nl_mod.o 

conv_jp.o : scmdiag.o buoysort.o nnparameter.o shr_nl_mod.o 

