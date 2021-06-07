function de_seasonal, d
  nt = n_elements(d)
  ny = nt/12
  d2 = d*0.0

  climo = fltarr(12)
  for i = 0,11 do begin
    indx = i + indgen(12)*ny
    dj = d[indx]
    jj = where(dj gt 100, cnt)
    if(cnt gt 0)then climo[i] = mean(dj[jj]) 
  endfor
  annual = 0

  jj = where(climo gt 100, cnt)
  if(cnt gt 0)then annual = ave10(climo)
  for i= 0,nt-1 do begin
    j = (i mod 12)
    if(d[i] gt 0)then d2[i] = d[i] - climo[j] + annual
;    print,j,d[i],d2[i]
  endfor
  
;stop

;print,'here ',max(d),max(d2)
;stop
  return,d2
end


