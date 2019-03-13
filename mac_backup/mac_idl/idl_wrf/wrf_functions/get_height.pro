;get_height.pro
;
; simple function for retrieving the 3D pressure field from WRF output
;

function get_height, ph, phb

height = (phb + ph)/9.81

return, height

end
