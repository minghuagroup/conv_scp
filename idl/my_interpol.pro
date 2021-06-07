
function my_interpol, d22,x22,x,missing=missing,wrap=wrap,dxh=dxh,noextra=noextra, speed=speed
 ; noextra - no extropolation, <0 treated as missing, > 0 as extension

if(keyword_Set(speed)) then return, interp1d(X22, d22, X)
; -------------------------------------------------------

  jjs = sort(x22)  ; sort first for interpolation
  d2  = d22[jjs]
  x2  = x22[jjs]

  nx = n_elements(x)
  nx2 = n_elements(d2)
  dd  = d2
  xx  = x2
  d=x*0.0
  if(not keyword_set(dxh))then dxh = abs(x[1]-x[0])/2
  if(not keyword_set(missing))then missing=-9999.
  if(not keyword_set(wrap))   then wrap = 0
  if(not keyword_set(noextra))   then noextra= 1  ; no extropolation

  if(wrap ne 0)then begin
   dx= x[1]-x[0]
   dd = [dd[nx2-wrap:*],dd,dd[0:wrap-1]]
   delx = dx*(indgen(wrap)+1 )
   xx = [xx[0]-delx,xx,xx[nx2-1]+delx]
  endif
  
;  if(missing lt 0)then jj = where(d2 gt missing, cnt) else jj = where(d2 lt missing, cnt)
  jj = where(abs(dd) lt abs(missing), cnt)
  if(cnt le 4)then begin ;
    return,x*0. - 9999.
  endif

  
  d = interpol(dd[jj],xx[jj],x)

  jj = where(xx eq x[0], cnt)
  if(cnt gt 0)then d[0] = dd[jj[0]]
  jj = where(xx eq x[-1], cnt)
  if(cnt gt 0)then d[-1] = dd[jj[0]]
 ;--------------- neglected here below 

; print,'d',max(dd),min(dd)
; print,'d',max(d),min(d)
; ix=1

; read,ix

  jj=where(d2 le missing,cnt)

if(cnt gt 0)then begin
  for i=0,nx-1 do begin  ; one grid at a time to check neighbouring grids
   dx =abs (xx-x[i]) 
   jj=where(dx le dxh,cnt1)
   if(cnt1 gt 0) then begin
     dj = dd[jj]
     jj2 = where(dj le missing,cnt2)
      if(cnt2 gt 0)then begin
       jj3 = where(dj gt missing,cnt3)
        d[i] = mean(dj[jj3])
      endif
   endif
 endfor
endif

if(keyword_set(noextra))then begin
 dd=d2
 x3=x2
 jj = where(dd gt missing,cnt)
 if(cnt gt 0)then begin
  dd = dd[jj]
  x3 = x3[jj]
 endif

 jj = where(x gt max(x3),cnt1)
 j1=where(x3 eq max(x3)) & j1 = j1[0]
 if(cnt1 gt 0)then  begin
  if(noextra gt 0) then d[jj] = dd[j1] else d[jj] = -999999.
 endif
 
 jj = where(x le min(x3),cnt1)
 j1=where(x3 eq min(x3)) & j1 = j1[0]
 if(cnt1 gt 0)then  begin
    if(noextra gt 0) then d[jj] = dd[j1] else d[jj] = -999999.
 endif
endif
return,d
end
