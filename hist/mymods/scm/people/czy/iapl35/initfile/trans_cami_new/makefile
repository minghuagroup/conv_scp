F90   =	 ifort
OBJS  =	Grid.o Var_name_inic.o prognostic.o read_IAPL35coordinate.o \
	calsig.o Inp_sp_sig.o read_ini.o write_ini.o gs_ll.o \
	intlay.o intp3d.o intpav.o Inp_gs_ll.o check.o GG2LL.o Interp_inidata.o 

.SUFFIXES: .f90 .o
FC=ifort
NCLIB = -L/public/software/mathlib/netcdf/4.3.2/intel/lib -lnetcdff
NCINC = -I/public/software/mathlib/netcdf/4.3.2/intel/include

all:$(OBJS)
	$(FC) -o GCMini  $(OBJS) $(NCLIB)
.f90.o:
	$(F90) -c  $(NCINC) $<
.f.o:
	$(F90) -c $(NCINC) $<
clean:
	rm *.o *.mod	
