;get_rho.pro

function get_rho, press, temp, qvapor

virtual_temp = temp*(1+(.611*qvapor)) ; -- virtual temperature equation
R = 287.05 ; J kg^-1 K^-1 -- gas constant for dry air

rho = press/(R*virtual_temp)

return, rho

end
