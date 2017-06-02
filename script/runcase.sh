#!/bin/bash

NAME=$1
sed "s/case_name.*/case_name = \"$NAME\"/g" drv_in > tmp
mv tmp drv_in

mkdir ./mymods-$NAME
cp ../mymods/* ./mymods-$NAME

#PWD=$(pwd)
#cd ../bld
#make
#cd $PWD

echo $NAME && mpirun -np 128 -H $2 ../cesm > runlog 2>&1 &

