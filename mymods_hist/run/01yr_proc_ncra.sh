#!/bin/csh -f
# for 1 year run just soft link

set casename = test35b #cas5b #castest2 #casadv2e
echo "casename is $casename"
set model      = cam2 # cam for cesm for cam2 casesm

set start_year = 1979 # MUST the same
set end_year   = $start_year  #0000 #1981

# AMWG seasonal 
mkdir $casename
mv ${casename}.$model.h0.*.nc $casename/
cp -L 01yr_proc_ncra.sh ${casename}/
cp -L runlog_${casename} ${casename}/

cd $casename

set climo_months = (01 02 03 04 05 06 07 08 09 10 11 12)

foreach mth ($climo_months)
   set yr0 = $start_year
   set yr2 = $end_year

   if ($mth == 13) then  #for 0000 year only
#   if ($mth == 12) then 
       @ yr0 = $yr0 - 1
       @ yr2 = $yr2 - 1
    echo $yr0 $yr2
   endif

   set yr = $yr0 
   set files=''
   @ yr = $yr - 0   #space important! convert to number
   while ($yr <= $yr2)
      set yyyy = $yr 
      if($yr <= 999)then
        set yyyy = 0{$yr} 
      endif
      if($yr <= 99)then
        set yyyy = 00{$yr} 
      endif
      if($yr <= 9)then
        set yyyy = 000{$yr} 
      endif

   set filein = {$casename}.{$model}.h0.{$yyyy}-{$mth}.nc
   #echo $filein
   set fileout = {$casename}_{$mth}_climo.nc
   #echo $fileout

   ln -sf $filein $fileout

      @ yr++
   end #yrs
 end  #months

ncra ${casename}_0[1-2]_climo.nc ${casename}_12_climo.nc ${casename}_DJF_climo.nc
ncra ${casename}_0[3-5]_climo.nc ${casename}_MAM_climo.nc
ncra ${casename}_0[6-8]_climo.nc ${casename}_JJA_climo.nc
ncra ${casename}_09_climo.nc ${casename}_1[0-1]_climo.nc ${casename}_SON_climo.nc
#
ncra ${casename}_DJF_climo.nc ${casename}_MAM_climo.nc ${casename}_JJA_climo.nc ${casename}_ANN_climo.nc

cd ..


echo ===========================================
echo "results in "$casename
echo "script 02_proc_obs.sh  and 03_proc_diff.sh  ready for editing"
echo "Done!"
echo ===========================================
