#!/bin/csh
ln -s /T1/xxie/minghua/conv_scp/src/buoysort.F90  ./mymods/
ln -s /T1/xxie/minghua/conv_scp/src/mymods/cam_diagnostics.F90  ./mymods/
ln -s /T1/xxie/minghua/conv_scp/src/mymods/cldwat2m_macro.F90  ./mymods/
ln -s /T1/xxie/minghua/conv_scp/src/mymods/convect_deep.F90  ./mymods/
ln -s /T1/xxie/minghua/conv_scp/src/mymods/convect_shallow.F90  ./mymods/
ln -s /T1/xxie/minghua/conv_scp/src/mymods/conv_intr_jp.F90  ./mymods/
ln -s /T1/xxie/minghua/conv_scp/src/conv_jp.F90  ./mymods/
ln -s /T1/xxie/minghua/conv_scp/src/nnparameter.F90  ./mymods/
ln -s /T1/xxie/minghua/conv_scp/src/mymods/phys_control.F90  ./mymods/
ln -s /T1/xxie/minghua/conv_scp/src/mymods/physics_types.F90  ./mymods/
ln -s /T1/xxie/minghua/conv_scp/src/mymods/physpkg.F90  ./mymods/
ln -s /T1/xxie/minghua/conv_scp/src/mymods/runtime_opts.F90  ./mymods/
ln -s /T1/xxie/minghua/conv_scp/src/mymods/uwshcu.F90  ./mymods/
ln -s /T1/xxie/minghua/conv_scp/src/mymods/vertical_diffusion.F90  ./mymods/
ln -s /T1/xxie/minghua/conv_scp/src/mymods/zm_conv.F90  ./mymods/
ln -s /T1/xxie/minghua/conv_scp/src/mymods/zm_conv_intr.F90  ./mymods/

ln -s /T1/xxie/minghua/conv_scp/src/mymods/mod_foutput.F90   ./mymods/

set new_mods = conv_scp/src/mymods_cosz5
echo $new_mods
ln -sf /T1/xxie/minghua/{$new_mods}/micro_mg1_0.F90  ./mymods/
ln -sf /T1/xxie/minghua/{$new_mods}/micro_mg_cam.F90  ./mymods/
ln -sf /T1/xxie/minghua/{$new_mods}/new_cosz.F90  ./mymods/
ln -sf /T1/xxie/minghua/{$new_mods}/shr_orb_mod.F90  ./mymods/
ln -sf /T1/xxie/minghua/{$new_mods}/subcolphy.f90  ./mymods/
ln -sf /T1/xxie/minghua/{$new_mods}/gamma_mod.f90 ./mymods/

#ls -l ./mymods/*
cd mymods
ls
cd ..
echo .. 'linked to '$new_mods
