
function ave2,d,first=first,missing=missing

  large_number = 999999.
  dd=d
 if(keyword_set(first))then dd=transpose(d)

 nx=n_elements(dd[*,0])
 dm = fltarr(nx)

 if(keyword_Set(missing))then large_number = missing

  for i=0,nx-1 do begin
   ddj=reform(dd[i,*])
   dm[i] = ave1(ddj,missing=missing)
  endfor

 return,dm

end

