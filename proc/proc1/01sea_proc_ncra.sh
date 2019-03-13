#!/bin/csh -f
# for 1 year run just soft link

echo take output JJA DJF ANN from 01yrxxx.csh
set casename = fc35_642def #20181225 #fc35_213
 #20181225 #famip5cx_20181221b #ptest60 #castest2 #casadv2e
echo "casename is $casename"
set model      = cam2 # cam for cesm for cam2 casesm

set start_year = 1979 #1983 #1979 # MUST the same
set end_year   = $start_year  #0000 #1981

# AMWG seasonal 
#mkdir $casename
#mv ${casename}.$model.h0.*.nc $casename/
#cp -L 01yr_proc_ncra.sh ${casename}/
#cp -L runlog_${casename} ${casename}/

cd {$casename}

set DJF_months = (01 02 03 04 05) 
set JJA_months = (06 07 08 09 10 11 12)

foreach mth ($DJF_months)
    set filein = {$casename}_DJF_climo.nc
    set fileout = {$casename}_{$mth}_climo.nc
   ln -sf $filein $fileout
   echo $filein
   echo $fileout
end  #months

foreach mth ($JJA_months)
    set filein = {$casename}_JJA_climo.nc
    set fileout = {$casename}_{$mth}_climo.nc
   ln -sf $filein $fileout
   echo $filein
   echo $fileout
end  #months

#ANNUAL
#    set filein = {$casename}_JJA_climo.nc
#    set fileout = {$casename}_ANN_climo.nc
#   ln -sf $filein $fileout

#ncra ${casename}_0[1-2]_climo.nc ${casename}_12_climo.nc ${casename}_DJF_climo.nc
#echo ncra ${casename}_0[1-2]_climo.nc ${casename}_12_climo.nc ${casename}_DJF_climo.nc

#ncra ${casename}_0[3-5]_climo.nc ${casename}_MAM_climo.nc
#ncra ${casename}_0[6-8]_climo.nc ${casename}_JJA_climo.nc
#ncra ${casename}_09_climo.nc ${casename}_1[0-1]_climo.nc ${casename}_SON_climo.nc
#
#ncra ${casename}_DJF_climo.nc ${casename}_MAM_climo.nc ${casename}_JJA_climo.nc ${casename}_ANN_climo.nc


cd ..


echo This is to take output JJA DJF ANN from 01yrxxx.csh to form monthly climo
echo ===========================================
echo "results in "$casename
echo "script 02_proc_obs.sh  and 03_proc_diff.sh  ready for editing"
echo "Done!"
echo ===========================================
