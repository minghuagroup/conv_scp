
function cam_vars

 restore, 'cam_vars.sav0'
 return, cam_vars

 ; ----- below if the sav0 file does not exist

input_file = '../data/cam5_data/lens_ANN_climo3.nc'
vars = ncdf_vars2d(input_file)
vars = vars[sort(vars)]

cam_vars =  create_struct('vars_cam', vars)  ; to be in parallel with CMIP and ERA5
return, cam_vars

save, file = 'cam_vars.sav0', cam_vars

end



