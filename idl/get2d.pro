  
;==================================================================
 pro get2d,var,dd,dd2=dd2,x2,y2,z2,iave,aa=aa,bb=bb,xx=xx,yy=yy,                   $ 
        title= title, xtitle=xtitle, ytitle=ytitle, rtitle = rtitle,             $
  	lon_range=lon_range,lat_range=lat_range,z_range=z_range, $
  	lon_width=lon_width, lat_width=lat_width,xrange = xrange, yrange=yrange
;==================================================================
; help,dd,x2,y2,z2  ; dd, dd2 needs to have same resolution, with dd2, the variable is different
; with view3dstr.lev_adjust, the lev1 and lev2 are reset based on info in view3dstr
; iave=1 ;zonal average 
; iave=2 ;meridional average
; iave=3 ;vertical average, minus range is k index
; iave=4 ;arbitrary cross section , needs both ranges and width

; lev2nn forces the number of contours when lev_adjust is set to 1 for auto generation

 if(not keyword_Set(dd2))then dd2 = dd

 if(not keyword_set(lon_range))then lon_range=[0.,360]
 if(not keyword_set(lat_range))then lat_range=[-90.,90]
 if(not keyword_set(z_range))  then   z_range  =[1000.,0]

 if(not keyword_set(lon_width)) then lon_width=2.
 if(not keyword_set(lat_width)) then lat_width=2.

 if(not keyword_set(ytitle)) then    ytitle='Pressure(mb)'

if(not keyword_Set(missing))then missing = -999999.
if(not keyword_Set(missing2))then missing2 = missing

case iave of
;--- zonal average
 1: begin ; zonal average
     xtitle = 'Latitude'
     ytitle = 'Pressure'
     xx = y2
     yy = z2
     xrange  = lat_range
     yrange  = z_range

     if(keyword_Set(rtitle))then  rtitle = ' ['+ $
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

     if(keyword_Set(rtitle))then  rtitle = ' ['+ $
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

    if(keyword_Set(rtitle))then   rtitle = ' ['+ $
          strtrim(strdigit(min(z_range),2),0)+','+strtrim(strdigit(max(z_range),0),2)+'] '
     
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
     if(keyword_Set(rtitle))then  rtitle = ' ['+ $
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

if(keyword_Set(rtitle))then title= var + rtitle else title = var

return

end
