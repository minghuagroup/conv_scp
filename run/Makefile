
#FC  = gfortran
FC  = ifort
#FFLAGS := -I$(INC_NETCDF) -L$(LIB_NETCDF) -lnetcdff -lnetcdf -g -fbacktrace -fcheck=all -DSCMDIAG 
#FFLAGS := -I$(INC_NETCDF) -L$(LIB_NETCDF) -lnetcdff -lnetcdf -fcheck=all -fbacktrace
FFLAGS := -I$(INC_NETCDF) -L$(LIB_NETCDF) -lnetcdff -lnetcdf -DSCMDIAG

VPATH = ../src

SRC = scp_conv.F90 scmdiag.f90 jp_conv.F90


OBJ = $(addsuffix .o, $(basename $(SRC)))

all: main

main: $(OBJ) main.o
	$(FC) $(FFLAGS) $(OBJ) main.o -o $@

%.o : %.F90
	$(FC) $(FFLAGS) -c $<

%.o : %.f90
	$(FC) $(FFLAGS) -c $<

clean:
	rm *.o *.mod

#dependence

scp_conv.o : scmdiag.o

scp_conv_intr.o: shr_const_mod.o

