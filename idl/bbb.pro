
_in files in ../infiles

contol case from original setting is  'my_testclm2' in lrun_mytest2/indata/myctl_test

1. SST replaced in the folder by 200801 and 200807 or 20170727 processed by sst2cam.pro
->change stream.docn.txt
->change ice_in

2. ATM initial condition replaced for 200801 or 200807 by test3.run initial data in this folder
   for 20170727 replaced by init.i.temp. in this folder processed by fnl2cam.pro
->change atm_in

3. LND initial data replace for 200801 or 200807 by test3.run initial data in this foldero
 for 20170727 replaced by init.i.temp. in this folder processed by fnl2clm.pro
->change lnd_in

4. DRV to change the stop and simulation date
-> drv_in



CASES in indata/
test1 and test2 are with more output fields
init1 and init2 are with original and my initial conditions
myctl_test 2 years of standard run with monthly initial and restart fields for cam and clm



ON STORM
