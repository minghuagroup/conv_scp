;get_pwat.pro
;
;This is a simple function to calculate the precipitable
; water from WRF output

function get_pwat, qv, rho, height

dims = size(qv, /dimension)

temp2 = []

for i=0,dims[2]-1 do begin
;  qvt = qv[*,*,i] + qv[*,*,i+1]/2.
;  rhot = rho[*,*,i] + rho[*,*,i+1]/2.
  temp = (qv[*,*,i]*rho[*,*,i]*$
         (height[*,*,i+1]-height[*,*,i]))*25.4/1000. ;convert to inches
  temp2 = [[[temp2]],[[temp]]]
endfor

pwat = fltarr(dims[0],dims[1])
for i=0, dims[0]-1 do begin
  for j=0, dims[1]-1 do begin
    pwat[i,j] = total(temp2[i,j,*])
  endfor
endfor

return, pwat

end

