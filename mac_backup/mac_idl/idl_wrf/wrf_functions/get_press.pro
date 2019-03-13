;get_press.pro
;
; simple function for retrieving the 3D pressure field from WRF output
;

function get_press, p, pb

press = pb + p

return, press

end
