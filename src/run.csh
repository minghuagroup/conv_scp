#ifort test_buoysort.F90
#./a.out
ifort test_lapack.F90 -L/T3/yhy/softwares/lapack-3.8.0 -llapack -lblas
#ifort -L/T3/yhy/softwares/lapack-3.8.0 -llapack test_lapack.F90
./a.out
rm -fv a.out *.mod



