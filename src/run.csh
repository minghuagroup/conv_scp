rm -fv a.out *.mod
ifort test_buoysort.F90
./a.out
rm -fv a.out *.mod

