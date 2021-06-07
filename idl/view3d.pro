  
;============
 pro view3d,var,dd0,dd2=dd20,x2,y2,z2,iave,lev1,lev2,$
  	ytitle=ytitle,gifname=gifname,title=title,t2itle = title2,  $
  	lon_range=lon_range,lat_range=lat_range,z_range=z_range, $
  	lon_width=lon_width, lat_width=lat_width,gifolder = gifolder,window=window, $
         setZ = setZ, lev_adjust = lev_adjust, lev2_nn = lev2_nn, $
         lev1_max = lev1_max, lev1_min = lev1_min,  nn=nn, $
         lev2_max = lev2_max, lev2_min = lev2_min, colorbar_nn = colorbar_nn, $
         vect = vect,uvel=uvel,vvel=vvel,vlen = vlen,usteps = usteps,vsteps = vsteps,$
         missing=missing, m2issing = missing2, vcolor=vcolor,vmissing = vmissing, $
         min_limit = min_limit, max_limit = max_limit
;======================
;pro view3d,var,dd0,dd2=dd20,x2,y2,z2,iave,lev1,lev2,$
; 	ytitle=ytitle,gifname=gifname,title2=title2,  $
; 	lon_range=lon_range,lat_range=lat_range,z_range=z_range, $
; 	lon_width=lon_width, lat_width=lat_width,gifolder = gifolder,window=window, $
;        setZ = setZ,uvel=uvel,vvel=vvel,vect = vect, $ 
;        view3dstr = view3dstr
;======================
; help,dd,x2,y2,z2  ; dd, dd2 needs to have same resolution, with dd2, the variable is different
; with view3dstr.lev_adjust, the lev1 and lev2 are reset based on info in view3dstr
; iave=1 ;zonal average 
; iave=2 ;meridional average
; iave=3 ;vertical average, minus range is k index
; iave=4 ;arbitrary cross section , needs both ranges and width

; lev2nn forces the number of contours when lev_adjust is set to 1 for auto generation

; if(keyword_set(setZ)then mydeviceZ,1 else mydevice, 0

 dd = dd0
 if(not keyword_Set(dd2))then dd2 = dd0 else dd2 = dd20

 if(not keyword_set(title))then title=''
 if(not keyword_set(t2itle))then title2=''
 if(not keyword_set(lon_range))then lon_range=[0.,360]
 if(not keyword_set(lat_range))then lat_range=[-90.,90]
 if(not keyword_set(z_range))  then   z_range  =[1000.,0]

 if(not keyword_set(lon_width)) then lon_width=2.
 if(not keyword_set(lat_width)) then lat_width=2.

 ;if(not keyword_set(gifname))then gifname = var+'_iave'+strtrim(strdigit(iave,0),2)+'.gif'
 if(not keyword_set(ytitle)) then    ytitle='Pressure(mb)'

if(not keyword_Set(missing))then missing = -999999.
if(not keyword_Set(m2issing))then missing2 = missing
if(not keyword_Set(vmissing))then vmissing = 999.

 if(keyword_set(max_limit))then begin
  jj = where(dd ge max(lev1), cnt)
  if(cnt gt 0)then dd[jj] = max(lev1)*(1-1.0e-5)
 endif

 if(keyword_set(min_limit))then begin
  jj = where(dd le min(lev1), cnt)
  if(cnt gt 0)then dd[jj] = min(lev1)*(1.+1.e-5)
 endif

 if(not keyword_set(vect))then begin
    vect = 0
    uverl = 0
    vvel = 0
    vlen = 0
    usteps = 0
    vsteps = 0
  endif 

 if(not keyword_set(nn )) then nn=20  ; force the number of lines in the second array
 if(not keyword_Set(lev_adjust)) then lev_adjust = 0
 if(not keyword_Set(vcolor)) then vcolor = 0
 if(not keyword_Set(colorbar_nn)) then  colorbar_nn = 15 
 if(not keyword_Set(lev2_nn)) then lev2_nn = 1  ; every line used  ; spaced levels

; print,colorbar_nn

 if(not keyword_Set(vlen)) then vlen  = 3
 if(not keyword_Set(usteps)) then   usteps    = 3
 if(not keyword_Set(vsteps)) then   vsteps    = 3
 if(not keyword_Set(vcolor)) then   vcolor = 0
    ;lev1_min    = view3dstr.lev1_min    ;set later
    ;lev1_max    = view3dstr.lev1_max  
    ;lev2_min    = view3dstr.lev2_min   
    ;lev2_max    = view3dstr.lev2_max  

case iave of
;--- zonal average
 1: begin ; zonal average
     xtitle = 'Latitude'
     ytitle = 'Pressure'
     xx = y2
     yy = z2
     xrange  = lat_range
     yrange  = z_range

     rtitle = ' ['+ $
          strtrim(strdigit(lon_range[0],0),2)+','+strtrim(strdigit(lon_range[1],0),2)+'] '
     
     ii = where(x2 ge lon_range[0] and (x2 le lon_range[1]),cnt)
     aa = dd[ii,*,*]
     aa = ave3(aa,three=1,dim2=1,missing=missing)

     bb = dd2[ii,*,*]
     bb = ave3(bb,three=1,dim2=1,missing=missing2)
    end

 2: begin ; meridonal average
     xtitle = 'Longitude'
     ytitle = 'Pressure'
     xx = x2
     yy = z2
     xrange  = lon_range
     yrange  = z_range

     rtitle = ' ['+ $
          strtrim(strdigit(lat_range[0],0),2)+','+strtrim(strdigit(lat_range[1],0),2)+'] '
     
     ii = where(y2 ge lat_range[0] and (y2 le lat_range[1]),cnt)
     aa = dd[*,ii,*]
     aa = ave3(aa,three=2,dim2=1 ,missing=missing)

     bb = dd2[*,ii,*]
     bb = ave3(bb,three=2,dim2=1 ,missing=missing2)
    end

  3: begin ; vertical average
     xtitle = 'Longitude'
     ytitle='Latitude'
     xx = x2
     yy = y2
     xrange  = lon_range
     yrange  = lat_range

     rtitle = ' ['+ $
          strtrim(strdigit(min(z_range),0),2)+','+strtrim(strdigit(max(z_range),0),2)+'] '
     
 sz = size(dd)
if(sz[0] = 2)then begin
  aa = dd
  bb = dd2
endif else begin
     ii = where(z2 ge min(z_range) and (z2 le max(z_range)),cnt)
 if(z_range[0] le 0)then begin
   ii = fix(- z_range[0]) ; LEVEL
   z_range[0] = z2[ii]
   z_range[1] = z2[ii]
 endif else begin
     if(cnt le 0)then begin
      distz = abs(z2 - z_range[0])
      ii = where(distz eq min(distz) )
     endif
 endelse
     aa = dd[*,*,ii]
     aa = ave3(aa,three=3,dim2=1,missing=missing)

     bb = dd2[*,*,ii]
     bb = ave3(bb,three=3,dim2=1,missing=missing2)


     if(keyword_set(vect))then begin
       uu = uvel[*,*,ii]
       uu = ave3(uu,three=3,dim2=1,missing=-990.)
       vv = vvel[*,*,ii]
       vv = ave3(vv,three=3,dim2=1,missing=-990.)
     endif

endelse
    end
 
 4: begin ; arbitrary cross section
     xtitle = 'Longitude'
     ytitle = ytitle
     rtitle = ' ['+ $
          strtrim(strdigit(lat_range[0],0),2)+','+strtrim(strdigit(lon_range[0],0),2)+ '] to [' $
         + strtrim(strdigit(lat_range[1],0),2)+','+strtrim(strdigit(lon_range[1],0),2)+']'
     


     xstart = lon_range[0]
     ystart = lat_range[0]

     xend   = lon_range[1]
     yend   = lat_range[1]

     del_x  = (x2[1]-x2[0])
     del_y  = (y2[1]-y2[0])

     nxline  =fix( (xend-xstart)/del_x )
     nyline  =fix( (yend-ystart)/del_y )
     nzline  = max([nyline,nxline])
     xline =indgen(nzline+1)*(xend-xstart)/nzline + xstart
     yline =indgen(nzline+1)*(yend-ystart)/nzline + ystart

     nz= n_elements(z2)
     dcross = fltarr(nzline+1,nz)-9999.
     dcross2 = fltarr(nzline+1,nz)-9999.
     for i=0,nzline do begin
      distx = abs(x2 - xline[i])/2.
      disty = abs(y2 - yline[i])/2.
      ii=where(distx le lon_width,cnt1)
      jj=where(disty le lat_width,cnt2)
        if (cnt1*cnt2 gt 0)then begin
          ddw = dd[ii,jj,*]
          dcross[i,*] = ave3(ddw,first=3,missing=missing)

          ddw2 = dd2[ii,jj,*]
          dcross2[i,*] = ave3(ddw2,first=3 ,missing=missing2)
        endif
     endfor
     nzline=nzline+1

     aa = dcross
     bb = dcross2

     xx = xline
     yy = z2
     xrange  = [min(xline),max(xline)]
     yrange  = z_range
    end

  else: 
 endcase

title= var + rtitle

if(not keyword_Set(lev1_max))then lev1_max = max(aa)
if(not keyword_Set(lev1_min))then lev1_min = min(aa)
if(not keyword_Set(lev2_max))then lev2_max = max(bb)
if(not keyword_Set(lev2_min))then lev2_min = min(bb)

; ===========  set self adjust levels

if(keyword_Set(lev_adjust))then begin
  lev1 = get_lev_adjust(aa,lev1, missing=missing)
  lev2 = get_lev_adjust(bb,lev2, missing=missing)
endif
; ====================
if(lev2_nn lt 0)then begin  ; negative values no lines
  lev2 = [1.0e5, 1.5e6]
endif else begin
  lev2 = array_spaced(lev2, lev2_nn)
endelse

;stop
xsize0 = 600 & ysize0 = 400

if(keyword_Set(setZ))then begin
 set_plot,'Z'
 DEVICE, Decomposed=0
 colors = GetColor(/Load, Start=1)
 !P.background = colors.white
 !P.color      = colors.black
 ;Device, Set_Resolution=[500,350];, Z_Buffer=0
 Device, Set_Resolution=[xsize0,ysize0];, Z_Buffer=0
 Erase
endif else begin
 set_plot,'X'
 DEVICE, Decomposed=0
 colors = GetColor(/Load, Start=1)
 !P.background = colors.white
 !P.color      = colors.black
 if(not keyword_set(window))then  window,xsize=xsize0,ysize=ysize0,/free,title=title
endelse

if(iave ne 3)then begin

 
;=======================================================
 plot_4dhtml,aa,bb,xx,yy,lev1,lev2,xrange=xrange,$
      yrange=yrange, colorbar_nn = colorbar_nn, $
          title=title+' '+title2,xtitle=xtitle,ytitle=ytitle
;=======================================================

endif else begin
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

;print,'lev2',lev2
value = ' ('+strtrim(min(aa[jj]),2)+', '+strtrim(max(aa[jj]),2)+', '$
    +strtrim(mean(aaw)/mean(bbw),2)+')'
 title=title+value
;=======================================================
 if(not keyword_Set(vcolor))then vcolor=colors.black
 plot_map4,aa,bb,xx,yy,lev1,lev2,xrange=xrange,$
      yrange=yrange,londel=londel,latdel=latdel,isetz=1,$
          title=title+' '+title2,xtitle='longitude',ytitle=ytitle, $
      colorbar_nn = colorbar_nn, $
        vect = vect,uvel=uu,vvel=vv,vlen = vlen,usteps = usteps, vsteps = vsteps,vmissing=vmissing, vcolor=colors.black
;=======================================================
endelse

 if(keyword_Set(gifname))then begin
   if(not keyword_set(gifolder))then begin
       mygif,'gif_3d/'+gifname+'.gif' 
       ;print,'gif_3d/'+gifname+'.gif' 
    endif else begin
       mygif,gifname+'.gif'
       ;print,gifname+'.gif'
    endelse
 endif

;stop
return

end
