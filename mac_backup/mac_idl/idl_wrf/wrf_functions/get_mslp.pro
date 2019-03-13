;get_td.pro
;
; simple function for retrieving the MSLP from WRF output
;

function get_mslp, h,p,t,q

vtemp = t*(1+0.611*q)
Rd = 287.05 ; J kg^-1 K^-1 (dry-air gas constant)

dims = size(h,/dimension)

;define the geopotential height on the midpoints
ght=[]
for i=0, dims[2]-2 do begin
  ght = [[[ght]],[[(h[*,*,i+1] + h[*,*,i])/2.]]]
endfor

tmp = vtemp[*,*,0] + (ght[*,*,0])*6.5/1000.

vtemp[*,*,0] = (vtemp[*,*,0]+tmp)/2.

mslp = p[*,*,0]*exp((9.81*ght[*,*,0])/(Rd*vtemp[*,*,0]))/100.

return, mslp

end
