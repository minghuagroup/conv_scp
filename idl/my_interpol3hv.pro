
function my_interpol3hv, dd1, pp1, dd1s, pp1s,lon,lat, pp3, lon2,lat2, wrap=wrap

; interpolate data vertically to pp3[nx2,ny2,np2] at lon2, lat2,from dd1, pp1,[nx,ny,np],lon
; all 3d arrays except dd1s and ps input srf fields

if(not keyword_set(wrap))then wrap = 1

np = n_elements(dd1[0,0,*])

nx2 = n_elements(pp3[*,0,0])
ny2 = n_elements(pp3[0,*,0])

dd1j = fltarr(nx2,ny2,np)

for k = 0,np-1 do begin ; original vertical coordinate
 dj   = reform(dd1[*,*,k])
 dd1j[*,*,k] = my_interpol2(dj,lon,lat,lon2,lat2, wrap=wrap)
endfor 

 dd3 = my_interpol3v(dd1j,pp1,dd1s,pp1s, pp3)

 return,dd3
end
 
