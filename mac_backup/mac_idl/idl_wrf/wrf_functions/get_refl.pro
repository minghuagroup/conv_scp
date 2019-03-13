;get_refl.pro

function get_refl, lon, lat, qrain, qgraup, qsnow, rho, temp, height

expnt = 7./4.
expnt2 = -0.75

;these values are consistent with Thompson et al. 2008
alpha = 0.224 ;accounts for dielectric effects
rho_r = 1000. ; kg m^-3 -- density of rain
rho_g = 400. ; kg m^-3 -- density of graupel
rho_s = 100. ; kg m^-3 -- density of snow
t0 = 273.16 ; K -- triple point of water
N1 =  9D+9 ; m^-4 -- constant 1 for rain intercept parameter
N2 = 2D+6 ; m^-4 -- constant 2 for rain intercept parameter
qr0 = 1D-4 ; kg kg^-1 -- reference rain water mixing ratio

;calculate the intercept parameter for rain
Nor = ((N1-N2)/2.)*tanh((qr0 - qrain)/(4*qr0)) + (N1+N2)/2. ; m^-4

;calculate thte intercept parameter for snow
Nos = dblarr(n_elements(temp))
for i=0,n_elements(temp)-1 do begin
if (temp[i]-t0) GT -0.001 then diff = temp[i]-t0 else diff = -0.001
Nos[i] = 2D+6*exp(-0.12*diff) ; m^-4
if Nos[i] GT 2D+8 then Nos[i] = 2D+8
endfor

;calculate the intercept parameter for graupel
Nog = dblarr(n_elements(Nor))
for i =0, n_elements(Nor)-1 do begin
Nog[i] = (200./qgraup[i])  ; m^-4
if Nog[i] GT 5D+6 then Nog[i]=5D+6
if Nog[i] LT 1D+4 then Nog[i]=1D+4
endfor

Norstar = (gamma(7)/(!PI*rho_r*(gamma(4)/6.))^expnt)*((rho_r/rho_r)^2.) * $
Nor^(expnt2)

Nogstar = (gamma(7)/(!PI*rho_g*(gamma(4)/6.))^expnt)*((rho_g/rho_r)^2.) * $
Nog^(expnt2)

Nosstar = (gamma(7)/(!PI*rho_s*(gamma(4)/6.))^expnt)*((rho_s/rho_r)^2.) * $
Nos^(expnt2)

refl_mm = (Norstar*(rho*qrain)^(expnt) + alpha*Nogstar*(rho*qgraup)^(expnt) + $
alpha*Nosstar*(rho*qsnow)^(expnt))*1D+18

;remove non-meteorological values from the field so that reflectivity $
;at these points is 0
ind = where(refl_mm EQ 0)
refl_mm[ind] = 1

refl_dBZ = 10.*alog10(refl_mm)
dim_lon = size(lon,/DIMENSION)
dim_lat = size(lat,/DIMENSION)
comp_refl = fltarr(dim_lon[0],dim_lat[1])

ref_height = 4000. ;4 km composite reflectivity

for i=0,dim_lon[0]-1 do begin
   for j=0, dim_lat[1]-1 do begin
    diff = abs(height[i,j,*] - ref_height)
    ind = sort(diff)
    comp_refl[i,j] = max(refl_dBZ[i,j,0:ind[0]],/NAN)
    if comp_refl[i,j] LT 0 then comp_refl[i,j] = 0
   endfor
endfor


return, comp_refl

end
