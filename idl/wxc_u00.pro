
n=181
lat = indgen(n)*1.-90.
lat0 = 15.0
lat1 = 62.5

c1 = 0.2
c2 = 0.8
c3 = 0.5

width = 5.
fc = lat*0.0

for i=0,n-1 do begin
 if(abs(lat(i)) le lat1/2.0)then begin
  x = abs(lat[i]) - lat0
  y = tanh(x/width) 
  fac2 = c2*(1.+y)/2. + c1*(1-y)/2.
 endif else begin
  x = abs(lat[i]) - lat1
  y = tanh(x/width) 
  fac2 = c3*(1.+y)/2. + c2*(1-y)/2.
 endelse

  fc[i] = fac2
endfor

plot,lat,fc
end

