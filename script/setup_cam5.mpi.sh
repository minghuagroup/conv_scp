#!/bin/bash

CURRENT_DIR=$(pwd)

#export INC_NETCDF=/opt/netcdf/include
#export LIB_NETCDF=/opt/netcdf/lib

#export INC_MPI=/usr/mpi/intel/openmpi-1.4.3-qlc/include
export INC_MPI=/usr/mpi/intel/openmpi-1.10.4-qlc/include
#export INC_MPI=/T1/xxie/local/mpich-gnu/include

export CCSM_ROOT=/T1/xxie/clmodel/cesm1_2_1
#export CCSM_ROOT=/T1/xxie/clmodel/cesm1_2_2

export CAM_ROOT=$CCSM_ROOT
export camcfg=$CAM_ROOT/models/atm/cam/bld

export bld_dir=$CURRENT_DIR/bld
export exe_dir=$CURRENT_DIR
export usr_src=$CURRENT_DIR/mymods
export run_dir=$CURRENT_DIR/run

if [ ! -d $run_dir ]; then
  echo "$run_dir does not exisit, creating..."
  mkdir $run_dir -p
fi

#if [ ! -d $bld_dir/mymods ]; then
if [ ! -d $usr_src ]; then
  mkdir -p $usr_src
fi

export CSMDATA=/R3/cesm/inputdata

#though to configure for SCAM< still need to set "-hgrid 64x128" a recognized hgrid

#ntasks=64
ntasks=128

if [ "$1" = "" ]; then
#$camcfg/configure -help
$camcfg/configure -cam_bld $bld_dir \
   -usr_src $usr_src -cam_exedir $exe_dir -ntasks $ntasks -nosmp \
   -cc mpicc -fc mpif90 -fc_type intel \
   -cam_exe cesm \
   -dyn fv -hgrid 1.9x2.5 -phys cam5 -chem none #\
#   -fflags '-DModLXZ'
   #-dyn fv -hgrid "0.9x1.25" -phys cam5 -chem none
#  -dyn fv -hgrid "0.47x0.63" -phys cam5 -chem none
#  -dyn eul -hgrid 64x128 -phys cam5 -chem none 
fi

   #-cc mpicc -fc mpif90 -fc_type gnu \
   #-cc mpicc -fc mpif90 -fc_type intel \

if [ "$1" = "nl" ]; then
$camcfg/build-namelist -test -ntasks $ntasks -config $bld_dir/config_cache.xml -dir $run_dir \
   -namelist "&camexp prescribed_aero_model='bulk'/"
fi

