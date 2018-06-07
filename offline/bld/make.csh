
export INC_NETCDF=/T3/yhy/apps/netcdf-4.5.0/include
export LIB_NETCDF=/T3/yhy/apps/netcdf-4.5.0/lib
export LAPACK_LIBDIR=/T3/yhy/softwares/lapack-3.8.0

#export FC=ifort
export FC=gfortran
#export FFLAGS="-I${INC_NETCDF} -L${LIB_NETCDF} -lnetcdff -lnetcdf -L${LAPACK_LIBDIR} -llapack -lblas -DOFFLINECP -g -check all -check noarg_temp_created -fpe0 -traceback"
#export FFLAGS="-L${LAPACK_LIBDIR} -llapack -lblas -DOFFLINECP -g -check all -check noarg_temp_created -fpe0 -traceback -c"
export FFLAGS="-g"
#export FFLAGS="-L${LAPACK_LIBDIR} -llapack -lblas -DOFFLINECP"

$FC -c shr_nl_mod.F90  $FFLAGS 
$FC -c buoysort.F90 $FFLAGS 
#$FC -c nnparameter.F90 $FFLAGS 
#$FC -c conv_jp.F90 $FFLAGS

#f2py --fcompiler=gfortran -c --f90flags="-fPIC -L${LAPACK_LIBDIR} -llapack -lblas " -DOFFLINECP -L${LAPACK_LIBDIR} -llapack -lblas -m shr_nl_mod shr_nl_mod.F90 
#f2py --fcompiler=gfortran -c --f90flags="-fPIC -L${LAPACK_LIBDIR} -llapack -lblas " -DOFFLINECP -L${LAPACK_LIBDIR} -llapack -lblas -m buoysort buoysort.F90 

f2py --fcompiler=gfortran -c --f90flags="-fPIC" -m conv_jp shr_nl_mod.F90 buoysort.F90  conv_jp.F90


