;get_td.pro
;
; simple function for retrieving the 2D dewpoint temp from WRF output
;

function get_td, t,q, psfc

;calculate the saturation vapor pressure - Clausius Clapeyron Equation
es = 6.116441*(10^((7.591386*(t-273.15))/(t-273.15 + 240.7263)))

;calculate the environmental vapor pressure - Clausius Clapeyron Equation
e = (q*psfc/.622)/(1+(q/.622))/100.

;calculate the relative humidity
rh = e/es*100.

;calculate the 2-meter dewpoint temperature - Clausius Clapeyron Equation
;  solved for T
td =243.04*(alog(rh/100)+((17.625*(t-273.15))/(243.04+t-273.15)))$
            /(17.625-alog(rh/100)-((17.625*(t-273.15))/(243.04+t-273.15)))

;convert dew point temperature from celsius to kelvin
td += 273.15

return, td

end
