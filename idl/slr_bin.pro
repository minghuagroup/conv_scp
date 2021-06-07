
function slr_bin, dd, d22 = d22,xx = xx, yy=yy, oro = oro, hh=hh,missing=missing
; return d11 within a height bin, d22 above a height

 restore,'cam6.sav0'
 if(not keyword_set(xx)) then xx = lon2  ; cam
 if(not keyword_set(yy)) then yy = lat2
 if(not keyword_set(lsm)) then lsm = lsm2
 if(not keyword_set(oro)) then oro = phis2/9.8
 if(not keyword_set(hh)) then hh = indgen(12)*500.
 if(not keyword_set(missing)) then missing = 9999.

; after ncep20c.pro

 nh = n_elements(hh)

 sz = size(reform(dd))

 nx = n_elements(xx)
 ny = n_elements(yy)

 yy0 = yy*3.14/180
 xx0 = xx*3.14/180

 xx00 = replicate2(xx0,ny)
 yy00 = transpose( replicate2(yy0,nx))

 dpcos = cos(yy00)

if(sz[0] eq 2)then begin
 d11 = fltarr(nh)
 d22 = d11
endif else begin
 nt = n_elements(dd[0,0,*])
 d11 = fltarr(nh,nt)
 d22 = fltarr(nh,nt)
endelse

for ih = 0, nh-2 do begin
   ;kk   = where( (oro ge hh[ih]) and (oro lt hh[ih+1]) and (abs(dd) le missing), cnt1)
   kk   = where( (oro ge hh[ih]) and (oro lt hh[ih+1]), cnt1)
   area    = dpcos[kk]

   kk2   = where( (oro ge hh[ih]) , cnt2)
   area2    = dpcos[kk2]

  if(sz[0] eq 2)then begin
   d11[ih] = total(dd[kk] * area)/total(area)
   d22[ih] = total(dd[kk2] * area2)/total(area2)
  endif else begin
   for i=0,nt-1 do begin
     dj = reform(dd[*,*,i])
     d11[ih,i] = total(dj[kk] * area)/total(area)
     d22[ih,i] = total(dj[kk2] * area2)/total(area2)
   endfor 
  endelse

endfor ;ih
return,d11

end
 

