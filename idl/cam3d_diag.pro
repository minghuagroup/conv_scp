
; need to readin the variables and plot specification
; input filein2, datatype,igif
;------------------------


print,filein2
print,' run 00_range first ok ?? 1 for yes '
read,ix


 x2  =get_fld(filein2,'lon')
 y2  =get_fld(filein2,'lat')
 z2  =get_fld(filein2,'lev')

;===========================

 VARS = ['CLOUD','RELHUM'] ;'Q','W','RH','U','V','cloud','cldliq','cldice','cldw'] ;10

FOR iv=0,n_elements(vars)-1 DO BEGIN
 var=vars[iv]
FOR ii=0,nf-1 DO BEGIN
;====================
 filein2 = filecam_sav[ii]

 xx = x2
 yy = y2
 zz = Z2
  
 
 dd = get_fld(filein2,var)
 get_lev,dd, VAR,LEVC,SCALE

  case strupcase(vars[iv]) of 
   'CLOUD':         begin &  levc=cal_lev([0.,80],20)               & end
   'DCQ':         begin &  levc=cal_lev([-1.5,1.5],15)               & end
   else:
  endcase
  
  lev1 = levc
  lev2 = [1000.,2000]
  aa = dd*scale

if(max(aa) eq min(aa))then begin
  print,' Skipped...',var,max(aa)
  goto,jump_loop1
endif

  
  gifname = ''
  varj = expid[ii]+'_'+var
  if(igif)then gifname = varj

  window,/free,title=varj
  view3d,varj,aa,xx,yy,zz,iave,lev1,lev2,$
        ytitle=ytitle,gifname=gifname,  $
        lon_range=lon_range,lat_range=lat_range,z_range=z_range, $
        lon_width=lon_width, lat_width=lat_width
  print,''
  print,var, '  ',ytitle, ' iave=',iave

  ix=0
  if(iix)then  read,ix

jump_loop1:

ENDFOR ;ii  ; files

 aa = get_fld(filecam_sav[0],var)*scale - aa

  levc = cal_lev([-10.,10],20)
  case strupcase(vars[iv]) of 
   'RELHUM':    levc=cal_lev([-10.,10],20)
   'CLOUD':     levc=cal_lev([-8.,8],16)
   'DCQ':     levc=cal_lev([-0.2,0.2],10)
   else:
  endcase
  lev1=levc
  lev2 = levc*1

  gifname=''
  varj = expid[0]+'_'+expid[1]+'_'+var
  if(igif)then gifname = varj

  window,/free,title=varj
  view3d,varj,aa,xx,yy,zz,iave,lev1,lev2,$
        ytitle=ytitle,gifname=gifname,  $
        lon_range=lon_range,lat_range=lat_range,z_range=z_range, $
        lon_width=lon_width, lat_width=lat_width
  if(iix)then read,ix

ENDFOR ;iv
;====================



end
