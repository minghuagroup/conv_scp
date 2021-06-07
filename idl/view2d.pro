  
;============
 pro view2d,var,aaa,bb=bbb,xx,yy,iave,lev1,lev2,setZ = setZ, $
  	ytitle=ytitle,xtitle = xtitle, title=title, stitle=stitle,  $
  	lon_range=lon_range,lat_range=lat_range,z_range=z_range, $
  	lon_width=lon_width, lat_width=lat_width,window=window, $
         colorbar_nn = colorbar_nn,xrange=xrange, yrange=yrange, $
         vect = vect,uvel=uvel,vvel=vvel,vlen = vlen,usteps = usteps,vsteps = vsteps,$
         missing=missing, missing2 = missing2, vcolor=vcolor,vmissing = vmissing, $
         min_limit = min_limit, max_limit = max_limit,no_contour=no_contour, $
         PS_file = PS_file ,sidebar=sidebar, noc_label=noc_label,charsize=charsize
;======================
; help,dd,x2,y2,z2  ; dd, dd2 needs to have same resolution, with dd2, the variable is different
; with view3dstr.lev_adjust, the lev1 and lev2 are reset based on info in view3dstr
; iave=1 ;zonal average 
; iave=2 ;meridional average
; iave=3 ;vertical average, minus range is k index
; iave=4 ;arbitrary cross section , needs both ranges and width

; lev2nn forces the number of contours when lev_adjust is set to 1 for auto generation

 aa = aaa
 if(not keyword_Set(bbb))then bb=aa else bb = bbb
 
 if(keyword_set(max_limit))then begin
  jj = where(aa ge max(lev1), cnt)
  dj = max(lev1)
  if(cnt gt 0)then aa[jj] = max(lev1)*(1. - signum(dj)*1.e-3)
 endif

 if(keyword_set(min_limit))then begin
  jj = where(aa le min(lev1), cnt)
  dj = min(lev1)
  if(cnt gt 0)then aa[jj] = min(lev1)*(1. + signum(dj)*1.e-3)
 endif


 xsize0 = 450
 ysize0 = 300
 xsize0 = 600
 ysize0 = 400

if(not keyword_set(setZ))then setZ = 0

 if(setZ ne 0)then begin

 DEVICE, Decomposed=0
 colors = GetColor(/Load, Start=1)
 Erase

 if(setZ eq 2)then begin
   set_plot, 'PS'
   !P.font=0
   DEVICE, file = PS_file, /color, BITS_PER_PIXEL= 8 , /Bold
   print,'ps file to save ', ps_file
  endif else begin
   set_plot,'Z'
   Device, Set_Resolution=[xsize0,ysize0];, Z_Buffer=0
  endelse

 !P.background = colors.white
 !P.color      = colors.black
 ;Device, Set_Resolution=[500,400];, Z_Buffer=0
endif else begin
 set_plot,'X'
 DEVICE, Decomposed=0
 colors = GetColor(/Load, Start=1)
 !P.background = colors.white
 !P.color      = colors.black
 window,xsize=xsize0,ysize=ysize0,/free,title=title
 ;window,xsize=500,ysize=400,/free,title=title
endelse

 if(not keyword_set(lon_range))then lon_range=[0.,360]
 if(not keyword_set(lat_range))then lat_range=[-90.,90]
 if(not keyword_set(z_range))  then   z_range  =[1000.,0]

 if(not keyword_set(lon_width)) then lon_width=2.
 if(not keyword_set(lat_width)) then lat_width=2.

 if(not keyword_set(title))then title=var
 if(not keyword_set(stitle))then stitle=''
 if(not keyword_set(xtitle))then xtitle=0
 if(not keyword_set(ytitle)) then    ytitle=0

if(not keyword_Set(missing))then missing = -999999.
if(not keyword_Set(missing2))then missing2 = missing
if(not keyword_Set(vmissing))then vmissing = 999.

 if(not keyword_set(vect))then begin
    vect = 0
    uverl = 0
    vvel = 0
    vlen = 0
  endif 

 if(not keyword_Set(usteps)) then usteps = 5
 if(not keyword_Set(vsteps)) then vsteps = 5
 if(not keyword_Set(vlen)) then vlen = 10 
 if(not keyword_Set(vcolor)) then vcolor = 5
 if(not keyword_Set(colorbar_nn)) then  colorbar_nn = 10 

; print,colorbar_nn

if(iave ne 3)then begin


 
;=======================================================
 plot_4dhtml,aa,bb,xx,yy,lev1,lev2,xrange=xrange,$
      yrange=yrange, colorbar_nn = colorbar_nn, $
          title=title+' '+stitle,xtitle=xtitle,ytitle=ytitle
;=======================================================

endif else begin
 londel = fix((lon_range[1] - lon_range[0])/12. )
 latdel = fix((lat_range[1] - lat_range[0])/6. )

 ny=n_elements(yy)  ; do weighted average 
 aaw=aaa*0
 bbw=aaw
 jj=where(aa gt -999.)
 aaw[jj] = aaa[jj]
 bbw[jj] = 1.
 pi2=3.1416/180.
 for j=0,ny-1 do begin
  cosz = cos(yy[j]*pi2)
  aaw[*,j] = cosz*aaw[*,j]
  bbw[*,j] = cosz*bbw[*,j]
 endfor

;print,'lev2',lev2
value = ' ('+strtrim(min(aaa[jj]),2)+', '+strtrim(max(aaa[jj]),2)+', '$
    +strtrim(mean(aaw)/mean(bbw),2)+')'

 title=title ;+value
;=======================================================

;print,colorbar_nn
;print,lev1
;stop
 if(not keyword_Set(vcolor))then vcolor=colors.black
 plot_map4,aa,bb,xx,yy,lev1,lev2,xrange=xrange,$
      yrange=yrange,londel=londel,latdel=latdel,isetz=1,$
          title=title+' '+stitle,xtitle=xtitle,ytitle=ytitle, $
      colorbar_nn = colorbar_nn, no_contour = no_contour, $
        vect = vect,uvel=uvel,vvel=vvel,vlen = vlen,usteps = usteps, vsteps = vsteps, $
        vmissing=vmissing, vcolor=colors.black,sidebar=sidebar, noc_label=noc_label,$
         charsize = charsize
;=======================================================
endelse

if(setZ eq 2)then begin
 !P.font = -1
 device,/close ;ps type
endif

;stop
return

end
