  
nv=4
;run iap0 iap1  first !00_range
;get aa,xx,yy,lev1 

;var=casename+'_DCRF'

var = 'PRECC'
var = 'PRECL'
var = 'PRECT'
var = 'PRECL'
var = 'PRECCSH'
var = 'PRECCDZM'

vars = ['CLDTOT','CLDLOW','CLDMED','CLDHGH']
vars = ['CLDLOW'] 
vars = ['PBLH','CLDLOW','CLDHGH']
;vars = ['PBLH']
;vars = ['CLDLOW'] 
vars = ['CLDLOW','CLDHGH']
nv=n_elements(vars)

for iv=0,nv-1 do begin
;==========================
 var = vars[iv]

it = ihour/3
 dd3 = get_fld(fileins[0],var)*100.
 lev1j = cal_lev([0.,100],20)

if(var eq 'PBLH')then begin
  lev1j = cal_lev([0.,1500],20)
  dd3 = get_fld(fileins[0],var)
endif

aa  = reform(dd3[*,*,it])

  lev2 = [2000.,4000]+lev1j
titlename = '' ;var

  bb = aa
 londel = fix((lon_range[1] - lon_range[0])/12. )
 latdel = fix((lat_range[1] - lat_range[0])/6. )

 ny=n_elements(yy)  ; do weighted average
 aaw=aa*0
 bbw=aaw
 jj=where(aa gt -999.)
 aaw[jj] = aa[jj]
 bbw[jj] = 1.
 pi2=3.1416/180.
 for j=0,ny-1 do begin
  cosz = cos(yy[j]*pi2)
  aaw[*,j] = cosz*aaw[*,j]
  bbw[*,j] = cosz*bbw[*,j]
 endfor

 value = ' ('+strtrim(min(aa[jj]),2)+', '+strtrim(max(aa[jj]),2)+', '$
    +strtrim(mean(aaw)/mean(bbw),2)+')'

 gifname= titlename+var
 title= gifname+value

;===== for TRMM
;jj=where(aa le pthreshold,cnt) & if(cnt gt 0)then aa[jj]=lev1j[0]-1.
; yy2 = replicate2(yy,nx2) & YY2=transpose(YY2)
; jj=where((yy2 lt -60.) or (yy2 gt 60)) & aa[jj] = -9999.  ; mask high lat
;;===== for TRMM

ih=ihour
titlename = fileins[0]+'_h'+strtrim(ih,2)+'_'+var
window,/free,xsize=600,ysize=400,title=titlename

 if(max(aa) ne min(aa)) then begin
 jj = where(aa ge max(lev1j),cnt)
 if(cnt gt 0)then aa[jj] = max(lev1j)*0.9999
  plot_map4,aa,bb,xx,yy,lev1j,lev2,xrange=xrange,$
      yrange=yrange,londel=londel,latdel=latdel,isetz=1,$
          title=title,xtitle='longitude',ytitle=ytitle

;   if(igif)then begin
;       mygif,'gifs/'+gifname+'.gif'
;   endif
;  if(iix)then  read,ix

  endif ;max(aa)=/=min(aa)
ENDFOR ;iv


end
