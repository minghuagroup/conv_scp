;get_temp.pro

function get_temp,press,pottemp

theta = pottemp + 300.

temp  = theta*((100000./press)^(-0.286))

return, temp

end
