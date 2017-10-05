#!/bin/bash

CURRENT_DIR=$(pwd)

export INC_NETCDF=/opt/netcdf/include
export LIB_NETCDF=/opt/netcdf/lib

export INC_MPI=/usr/mpi/intel/openmpi-1.4.3-qlc/include

export CCSM_ROOT=/T1/xxie/clmodel/cesm1_2_1
#export CCSM_ROOT=/T1/xxie/clmodel/cesm1_1_2
export CSMDATA=/R3/cesm/inputdata

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


#though to configure for SCAM< still need to set "-hgrid 64x128" a recognized hgrid

if [ "$1" = "" ]; then
#$camcfg/configure -help
$camcfg/configure -cam_bld $bld_dir -usr_src $usr_src \
  -fc ifort -cc icc -nospmd -nospm \
  -cam_exedir $exe_dir -cam_exe scam \
  -scam -dyn eul -hgrid 64x128 -phys cam5 -chem none \
  -fflags '-check all -traceback'
fi


if [ "$1" = "nl" ]; then
$camcfg/build-namelist -v -test -config $bld_dir/config_cache.xml -dir $run_dir \
  -namelist "&camexp scmlat=36.605 scmlon=262.515 prescribed_aero_model='bulk'/"
fi

