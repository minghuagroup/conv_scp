  
;run iap0 iap1  first !00_range
; after iap2d
;get aa,xx,yy,lev1 


for iv=0,nv-1 do begin
;==========================
 var = vars[iv]
ihour = 6
if(var eq 'PRECCSH')then begin
  dd3 = get_fld(fileins[0],'PRECC') - get_fld(fileins[0],'PRECCDZM')
endif else begin
 dd3 = get_fld(fileins[0],var)
endelse
aa  = ave3(dd3[*,*,9:16],three=3) *100.
lev1j = cal_lev([0.,100],20)

  lev2 = [2000.,4000]
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

 gifname= titlename+'_Day2_'+var
 title= gifname+value

;===== for TRMM
;jj=where(aa le pthreshold,cnt) & if(cnt gt 0)then aa[jj]=lev1j[0]-1.
; yy2 = replicate2(yy,nx2) & YY2=transpose(YY2)
; jj=where((yy2 lt -60.) or (yy2 gt 60)) & aa[jj] = -9999.  ; mask high lat
;;===== for TRMM

window,/free,xsize=600,ysize=400,title=fileins[0]+'_Day2_'+var

titlename = fileins[0]+'_'+var
 if(max(aa) ne min(aa)) then begin
  plot_map4,aa,bb,xx,yy,lev1j,lev2,xrange=xrange,$
      yrange=yrange,londel=londel,latdel=latdel,isetz=1,$
          title=title,xtitle='longitude',ytitle=ytitle

;   if(igif)then begin
;       mygif,'gifs/'+gifname+'.gif'
;   endif
;  if(iix)then  read,ix
  endif
ENDFOR ;===== iv


end
