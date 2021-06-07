
;------------------------

print,' run 00_range.pro first ok ?? 1 for yes'
read,ix

NF =0   ;!!!

   filecs   ='../obs/'+['c3m200801.nc','c3m200807.nc']
   filein2  = filecs[nf]

titlename= 'C3M2d_'+months[nf]+'_'

print,filein2,titlename

;-------c3m data
 ncdf_vars,filein2,vars2
 x2  =get_fld(filein2,'lon')
 y2  =get_fld(filein2,'lat')
 z2  =get_fld(filein2,'lev')

 CLDTOT = get_fld(filein2,'CLDTOT')/100.
 CLDHGH = get_fld(filein2,'CLDHGH')/100.
 CLDMED = get_fld(filein2,'CLDMED')/100.
 CLDLOW = get_fld(filein2,'CLDLOW')/100.

 CLOUD  = get_fld(filein2,'CLOUD')/100.
 CLDLIQ = get_fld(filein2,'CLDLIQ')/100.
 CLDICE = get_fld(filein2,'CLDICE')/100.

 TGCLDLWP = sum2cld(cldliq,z2) 
 TGCLDIWP = sum2cld(cldice,z2) 
 TGCLDCWP = TGCLDLWP+TGCLDIWP
 
 VARS=['CLDHGH',  'CLDLOW', 'CLDMED', 'CLDTOT', 'TGCLDCWP', 'TGCLDIWP', 'TGCLDLWP']

FOR iv=0,n_elements(VARS)-1 DO BEGIN
;====================
 xx = x2
 yy = y2

  case VARS[iv]of 

   'CLDHGH':    begin & aa2 = CLDHGH   &  levc=lev_cld		 & end
   'CLDLOW':    begin & aa2 = CLDLOW   &  levc=lev_cld		& end
   'CLDMED':    begin & aa2 = CLDMED   &  levc=lev_cld		 & end
   'CLDTOT':    begin & aa2 = CLDTOT   &  levc=lev_cld   & end
   'TGCLDCWP':  begin & aa2 = TGCLDCWP*100*100 &  levc=lev_tgcldlwp & end  ; unit?
   'TGCLDIWP':  begin & aa2 = TGCLDIWP*100*100 &  levc=lev_tgcldlwp & end
   'TGCLDLWP':  begin & aa2 = TGCLDLWP*100*100 &  levc=lev_tgcldlwp & end

   else: print,'Variable not found!!'
  endcase
  
  var  = VARS[iv] 
  lev1 = levc
  lev2 = [1000.,2000]
;;  lev2 = (levc-levc[0])*3+levc[0]  ;[1000.,2000]

  aa = aa2
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

 if(max(aa) ne min(aa)) then begin
  plot_map4,aa,bb,xx,yy,lev1,lev2,xrange=xrange,$
      yrange=yrange,londel=londel,latdel=latdel,isetz=1,$
          title=title,xtitle='longitude',ytitle=ytitle

   if(igif)then begin
       mygif,'gif_2d/'+gifname+'.gif'
   endif
  if(iix)then  read,ix

 endif

ENDFOR ;iv
;====================

end
