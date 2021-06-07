
function ave20,d,first=first,missing=missing

  dd=d
 if(keyword_set(first))then dd=transpose(d)

 nx=n_elements(dd[*,0])
 dm = fltarr(nx)

 if(not keyword_Set(missing))then missing = 0.0

  for i=0,nx-1 do begin
   ddj=reform(dd[i,*])
   dm[i] = ave10(ddj,missing=missing)
  endfor

 return,dm

end

