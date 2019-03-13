
function ave2,d,first=first,missing=missing

  dd=d
 if(keyword_set(first))then dd=transpose(d)

 nx=n_elements(dd[*,0])
 dm = fltarr(nx)

 if(not keyword_Set(missing))then begin

 for i=0,nx-1 do begin
  dm[i] = mean(dd[i,*])
 endfor

 endif else begin

  for i=0,nx-1 do begin
   ddj=reform(dd[i,*])
   jj=where(ddj gt missing,cnt)
   dj = -9999.
   if(cnt gt 0)then dj=mean(ddj[jj]) 
   dm[i] = dj
;   print,dj,cnt
  
  endfor
 endelse
 return,dm

end

