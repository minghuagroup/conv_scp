#!/bin/bash

#CASENAME="scam-getgood"
CASENAME="scam-test"
#CASENAME="scam-paper"
#CASENAME="scamcapt-nsj-nouw"
#CASENAME="scam-zm"


#CASE="sgp95"
#YEAR="1995"
#DAYS="0719 0721 0723 0725"
#LAST="2"

#DAYS="0719 0720 0721 0722 0723 0724 0725"
#LAST="1"


#CASE="twp"
#YEAR="2006"
#DAYS="0118 0120 0122 0124 0126"
#LAST="2"


#CASE="tg"
#YEAR="1992"
##DAYS="1219 1221 1223 1225 1227 1229"
##LAST="2"
#DAYS="1219"
#LAST="12"


CASE="kwj"
YEAR="1999"
DAYS="0725 0727 0729 0731 0802 0804 0806 0808 0810"
LAST="2"

rm ../scam

mv atm_in atm_in_backup
mv drv_in drv_in_backup

CPWD=$(pwd)
cd ../bld
make
cd $CPWD

for DAY in $DAYS
do
    cp atm_in_${CASE} atm_in
    sed "s/start_ymd.*/start_ymd = ${YEAR}${DAY}/; s/stop_n.*/stop_n = ${LAST}/"\
    drv_in_${CASE} > drv_in
    ../scam
done

rm -rf ${CASENAME}.nc
cdo mergetime camrun.cam.h0* ${CASENAME}.nc
rm -rf camrun*nc

ncl casename=\"${CASENAME}\" plot-int.ncl

mv atm_in_backup atm_in
mv drv_in_backup drv_in


