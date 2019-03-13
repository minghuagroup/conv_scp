#!/bin/csh -f
set casename = F2000Climo
set casename2 = CTL
mkdir $casename2
echo "casename is $casename"
echo "casename2 is $casename2"
#cd $casename2


ln -s {$PWD}/{$casename}/{$casename}_ANN_climo.nc {$casename2}_01_climo.nc
ln -s {$PWD}/{$casename}/{$casename}_ANN_climo.nc {$casename2}_02_climo.nc
ln -s {$PWD}/{$casename}/{$casename}_ANN_climo.nc {$casename2}_03_climo.nc
ln -s {$PWD}/{$casename}/{$casename}_ANN_climo.nc {$casename2}_04_climo.nc
ln -s {$PWD}/{$casename}/{$casename}_ANN_climo.nc {$casename2}_05_climo.nc
ln -s {$PWD}/{$casename}/{$casename}_ANN_climo.nc {$casename2}_06_climo.nc
ln -s {$PWD}/{$casename}/{$casename}_ANN_climo.nc {$casename2}_07_climo.nc
ln -s {$PWD}/{$casename}/{$casename}_ANN_climo.nc {$casename2}_08_climo.nc
ln -s {$PWD}/{$casename}/{$casename}_ANN_climo.nc {$casename2}_09_climo.nc
ln -s {$PWD}/{$casename}/{$casename}_ANN_climo.nc {$casename2}_10_climo.nc
ln -s {$PWD}/{$casename}/{$casename}_ANN_climo.nc {$casename2}_11_climo.nc
ln -s {$PWD}/{$casename}/{$casename}_ANN_climo.nc {$casename2}_12_climo.nc

ln -s {$PWD}/{$casename}/{$casename}_ANN_climo.nc {$casename2}_ANN_climo.nc
ln -s {$PWD}/{$casename}/{$casename}_ANN_climo.nc {$casename2}_DJF_climo.nc
ln -s {$PWD}/{$casename}/{$casename}_ANN_climo.nc {$casename2}_MAM_climo.nc
ln -s {$PWD}/{$casename}/{$casename}_ANN_climo.nc {$casename2}_JJA_climo.nc
ln -s {$PWD}/{$casename}/{$casename}_ANN_climo.nc {$casename2}_SON_climo.nc

mv {$casename2}*.nc {$casename2}/
#cd ..

