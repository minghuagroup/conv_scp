
function my_interpol3v, dd1, pp01, dd1s, pp1s, pp03, tdim = tdim, nods = nods

; interpolate data vertically to pp3[nx2,ny2,np2] from dd1, pp1,[nx,ny,np]
; all 3d arrays except dd1s and ps input srf fields, nods is for no surface fields

pp1 = pp01
pp3 = pp03

if(array_equal(pp01, pp03))then return, dd1

nx2 = n_elements(dd1[*,0,0])
ny2 = n_elements(dd1[0,*,0])

if(not keyword_Set(nods))then nods = 0

szp1 = size(pp1)
if(szp1[0] = 1)then pp1 = replicate3(dd1,pp01,2)


;==================================
if(not keyword_set(tdim))then begin
;=====================================

szp3 = size(pp3)
if(szp3[0] = 1)then begin
  np2 = szp3[1]
  dd3 = fltarr(nx2,ny2,np2)
  pp3 = replicate3(dd3,pp03,2)
endif
np2 = n_elements(pp3[0,0,*])
dd3 = fltarr(nx2,ny2,np2)

if(keyword_set(nods))then begin
  if(pp1[0,0,0] gt pp1[0,0,0])then ns = 0 else ns = np2-1 
   dd1s = dd1[*,*,ns]
   pp1s = pp1[*,*,ns]
endif

; vertical interpolation
for i=0,nx2-1 do begin
for j=0,ny2-1 do begin

 pp1j = reform(pp1[i,j,*])
 dxj  = (shift(pp1j,-1)-shift(pp1j,1))/4.
 dx   = mean(abs(dxj))

 if(pp1[i,j,2] gt pp1[i,j,0])then begin
 pw = [reform(pp1[i,j,*]) , pp1s[i,j]]   ;in mb
 dw = [reform(dd1[i,j,*]) , dd1s[i,j]]  ;dd2s surface fields to aid the vertical intepolation
 endif else begin
 pw = [pp1s[i,j],reform(pp1[i,j,*])]   ;in mb
 dw = [dd1s[i,j],reform(dd1[i,j,*])] 
 endelse

; kk= sort(pw)
; pw = pw[kk]
; dw = dw[kk]

 pp2w = reform(pp3[i,j,*])

;; if(i eq 100 and (j eq 100))then stop

 dd1j = my_interpol(dw,pw,pp2w,dx=dx,noextra=1)
 
 jj=where(pp2w gt pp1s[i,j], cnt)  ; no extropolation

 if(cnt gt 0)then begin
    dd1j[jj] = dd1s[i,j]         ; take the sfc field
    ;j1 = min(jj)
    ;dd1j[j1:*] = dd1s[i,j]         ; take the sfc field
 endif

 dd3[i,j,*] = dd1j

endfor
endfor

;stop
return,dd3

endif ;without time dimension

; next for with time dimension

nx2 = n_elements(dd1[*,0,0,0])
ny2 = n_elements(dd1[0,*,0,0])

np2 = n_elements(pp3[0,0,*,0])


nt = n_elements(dd1[0,0,0,*])
dd4 = fltarr(nx2,ny2,np2,nt)

for k = 0,nt -1 do begin
 dd1j = reform(dd1[*,*,*,k])
 pp1j = reform(pp1[*,*,*,k])
 dd1sj = reform(dd1s[*,*,k])
 pp1sj = reform(pp1s[*,*,k])
 pp3j = reform(pp3[*,*,*,k])

 dd4[*,*,*,k] = my_interpol3v( dd1j, pp1j, dd1sj, pp1sj, pp3j, nods = nods, speed=1)
endfor

return,dd4  

end
