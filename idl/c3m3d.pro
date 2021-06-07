; do vertical cross sections

;UNIT!! kg/m3 

igif=1
iix = 0
;filems=['test1.cam.h0.2008-01.nc','test1.cam.h0.2008-07.nc']
;filecs=['c3m200801.nc','c3m200807.nc']

;============
; run 00_range first!

;; for different months, summer and july
;==================
 filein  = '../obs/CAM/'+filems[nf]
 filein2 = '../obs/'+filecs[nf]
 print,''
 print,filein 
 ncdf_vars,filein,vars

 x=get_fld(filein,'lon')
 y=get_fld(filein,'lat')
 lev=get_fld(filein,'lev')

 ncdf_vars,filein2,vars2
 x2  =get_fld(filein2,'lon')
 y2  =get_fld(filein2,'lat')
 z2  =get_fld(filein2,'lev')
 nx=n_elements(x2)
 ny=n_elements(y2)
 nz=n_elements(z2)

;model
 Z3 = get_fld(filein,'Z3')
 T3 = get_fld(filein,'T')
 PS3 = get_fld(filein,'PS')/100.
 hyam = get_fld(filein,'hyam')
 hybm = get_fld(filein,'hybm')
 P0 = 1000.
 P3 = Z3*0
 for k=0,n_elements(lev)-1 do begin
  P3[*,*,k] = hyam[k]*p0+hybm[k]*ps3[*,*]
 endfor
 RHO3 = P3/(287.*T3)*100.

 RHO3 = p2z(RHO3,Z3,Z2)
 RHO2 = fltarr(nx,ny,nz)
 for k=0,nz-1 do begin
  rho2[*,*,k] = my_interpol2(reform(RHO3[*,*,k]),x,y,x2,y2)
 endfor


;-------c3m data
 
 CLDTOT2 = get_fld(filein2,'CLDTOT')/100.
 CLDHGH2 = get_fld(filein2,'CLDHGH')/100.
 CLDMED2 = get_fld(filein2,'CLDMED')/100.
 CLDLOW2 = get_fld(filein2,'CLDLOW')/100.

 CLOUD2  = get_fld(filein2,'CLOUD')
  jjc = where(CLOUD2 le -999.,cntc)
 CLOUD2 = CLOUD2/100.
 CLDLIQ2 = get_fld(filein2,'CLDLIQ')*1.0e5
 CLDICE2 = get_fld(filein2,'CLDICE')*1.0e5

 TGCLDLWP2 = sum2cld(cldliq2,z2) 
 TGCLDIWP2 = sum2cld(cldice2,z2)
 
 CLDLIQ2 = CLDLIQ2/RHO2
 CLDICE2 = CLDICE2/RHO2

 if(cntc gt 0)then begin
  CLOUD2[jjc] = -9999.
  CLDLIQ2[jjc]= -9999.
  CLDICE2[jjc]= -9999.
 endif
;---- next in p pcordinate
 lev3 = indgen(101)*10   ;!!!! resolution
;===========================
 PS2 = my_interpol2(PS3,x,y,x2,y2)

 cloud3  = z2p(cloud2, z2,z3,lev3=lev3,ps=ps2)
 cldliq3 = z2p(cldliq2,z2,z3,lev3=lev3,ps=ps2)
 cldice3 = z2p(cldice2,z2,z3,lev3=lev3,ps=ps2)

 VARS = ['cloud','cldliq','cldice','cldw']

FOR iv=0,3 DO BEGIN
 xx = x2
 yy = y2
 zz = lev3
  case iv of 
   0: aa2= cloud3     
   1: aa2 = cldliq3
   2: aa2 = cldice3
   3: aa2 = cldice3+cldliq3
   else:
  endcase
 if(iz  eq 1)then begin  ;====z coordinate
  case iv of
   0: aa2= cloud2   
   1: aa2 = cldliq2
   2: aa2 = cldice2
   3: aa2 = cldice2+cldliq2
   else:
  endcase
   zz = z2/1000.
 endif
  
  var  = VARS[iv] 
  lev1 = lev_cldliq
  lev2 = [1000.,2000]
  if(var eq 'cloud')then lev1=lev_cld

  aa = aa2

  gifname = 0
  var = datatype+var
  if(igif)then gifname = var

  view3d,var,aa,xx,yy,zz,iave,lev1,lev2,$
        ytitle=ytitle,gifname=gifname,  $
        lon_range=lon_range,lat_range=lat_range,z_range=z_range, $
        lon_width=lon_width, lat_width=lat_width
  print,''
  print,var, '  ',ytitle, ' iave=',iave

  ix=0
  if(iix)then  read,ix

ENDFOR ;iv


end
