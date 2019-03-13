;get_lwp.pro

;simple function for computing liquid water path from WRF output. Meant to be compared to MODIS
;  CLWP product

function get_lwp,temp,height,rho,qvapor, qcloud, qrain, qice, qsnow

;get the total mixing ratio of cloud snow and ice
mr = (qcloud + qice + qsnow)

dims = size(height, /dimension)
n_lon = dims[0]
n_lat = dims[1]
n_levels = dims[2]
dz = fltarr(n_lon,n_lat,n_levels-1)

;calculate the dz array
for i = 0, n_levels-2 do begin
dz[*,*,i] = height[*,*,i+1] - height[*,*,i]
endfor

;for i=0, n_levels-3 do begin
;dp[*,*,i] = press[*,*,i] - press[*,*,i+1]
;endfor

;dp[*,*,n_levels-2] = press[*,*,n_levels-2]

cloud_LWP = fltarr(n_lon,n_lat)

for i=0, n_lon-1 do begin
 for j=0, n_lat-1 do begin
   ;multiply the equation by 1000 to convert from kg m^-2 to g m^-2
   cloud_LWP[i,j] = 1000.*total(rho[i,j,0:24]*mr[i,j,0:24]*dz[i,j,0:24])
 endfor
endfor

return, cloud_LWP
end
