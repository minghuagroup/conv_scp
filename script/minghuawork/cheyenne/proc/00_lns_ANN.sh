#!/bin/csh -f
set casename = $1
cd $casename
echo "casename is $casename"

ln -s ${casename}_ANN_climo.nc ${casename}_01_climo.nc
ln -s ${casename}_ANN_climo.nc ${casename}_02_climo.nc
ln -s ${casename}_ANN_climo.nc ${casename}_03_climo.nc
ln -s ${casename}_ANN_climo.nc ${casename}_04_climo.nc
ln -s ${casename}_ANN_climo.nc ${casename}_05_climo.nc
ln -s ${casename}_ANN_climo.nc ${casename}_06_climo.nc
ln -s ${casename}_ANN_climo.nc ${casename}_07_climo.nc
ln -s ${casename}_ANN_climo.nc ${casename}_08_climo.nc
ln -s ${casename}_ANN_climo.nc ${casename}_09_climo.nc
ln -s ${casename}_ANN_climo.nc ${casename}_10_climo.nc
ln -s ${casename}_ANN_climo.nc ${casename}_11_climo.nc
ln -s ${casename}_ANN_climo.nc ${casename}_12_climo.nc
cd ..

