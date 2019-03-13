;bulk_wind.pro
;
; simple function for retrieving the bulk wind from U and V components
;

function bulk_wind, u, v

wind = sqrt(u*u + v*v)

return, wind

end
