#!/bin/bash

export ecp_src=/glade/u/home/mzhang/minghua/conv_scp/src

ln -s $ecp_src/buoysort.F90 ./mymods
ln -s $ecp_src/conv_jp.F90 ./mymods
ln -s $ecp_src/nnparameter.F90 ./mymods
ln -s $ecp_src/scmdiag.F90 ./mymods

ln -s $ecp_src/mymods/cam_diagnostics.F90  ./mymods
ln -s $ecp_src/mymods/convect_deep.F90 ./mymods
ln -s $ecp_src/mymods/convect_shallow.F90 ./mymods
ln -s $ecp_src/mymods/conv_intr_jp.F90 ./mymods
ln -s $ecp_src/mymods/forecast.F90 ./mymods
ln -s $ecp_src/mymods/iop.F90 ./mymods
ln -s $ecp_src/mymods/macrop_driver.F90 ./mymods
#ln -s $ecp_src/mymods/micro_mg1_0.F90 ./mymods
ln -s $ecp_src/mymods/mod_foutput.F90 ./mymods
ln -s $ecp_src/mymods/phys_control.F90 ./mymods
ln -s $ecp_src/mymods/physics_types.F90 ./mymods
ln -s $ecp_src/mymods/physpkg.F90 ./mymods
ln -s $ecp_src/mymods/runtime_opts.F90 ./mymods
ln -s $ecp_src/mymods/uwshcu.F90 ./mymods
ln -s $ecp_src/mymods/vertical_diffusion.F90 ./mymods
ln -s $ecp_src/mymods/wv_saturation.F90 ./mymods
ln -s $ecp_src/mymods/zm_conv.F90 ./mymods
ln -s $ecp_src/mymods/zm_conv_intr.F90 ./mymods

ln -sf $ecp_src/mymods_cosz5/micro_mg1_0.F90 ./mymods/
ln -sf $ecp_src/mymods_cosz5/micro_mg_cam.F90 ./mymods/
ln -sf $ecp_src/mymods_cosz5/new_cosz.F90  ./mymods/
ln -sf $ecp_src/mymods_cosz5/shr_orb_mod.F90  ./mymods/
ln -sf $ecp_src/mymods_cosz5/subcolphy.f90  ./mymods/
ln -sf $ecp_src/mymods_cosz5/gamma_mod.f90  ./mymods/

