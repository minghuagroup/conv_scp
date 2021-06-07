; interpolation and ensemble averaing, bin
;goto,jump

dirW  = '../W/sav2/'
dir0  = '../W/sav0/'
var  = 'ps'
expids = ['historical','ssp585']

restore,'cam6.sav0'
 xx = lon2  ; cam
 yy = lat2
 oro = phis2/9.8

 nx  =n_elements(xx)
 ny  =n_elements(yy)

FOR IEXP = 0, N_ELEMENTS(EXPIDS)-1 DO BEGIN
; =========================================
 EXPID = EXPIDS[IEXP]
 IF(EXPID EQ 'historical')then begin
  nt = 115
  yearrange = [1900, 2014]
  y0 = 1900
 ENDIF ELSE BEGIN ;ssp
  nt = 86
  yearrange = [2015,2100]
  y0 = 2015
 ENDELSE

 spawn,'ls '+dirw + var+'*'+ expid+'*.nc', files
 models = get_lastv2_inlist(files,'_','.')
 nmodel = n_elements(models)

 ps  = fltarr(nx,ny,nt)
 pss = fltarr(nx,ny,nt)
 nh = 12 ; consistent with slr_bin
 mps_bin = fltarr(nh, nt,nmodel)
 mps_bin2 = fltarr(nh, nt,nmodel)

 mcount = pss
 ;y0  = 1900
 for im = 0, nmodel-1 do begin
  nc_file = files[im]
  print,im, nc_file

  ncdf_vars, nc_file,varsj, no_print=1
  if(blt('lat',varsj))then begin
    yy3 = get_fld(nc_file, 'lat')
    xx3 = get_fld(nc_file, 'lon')
  endif else begin
    yy3 = get_fld(nc_file, 'latitude')
    xx3 = get_fld(nc_file, 'longitude')
  endelse

  ps0= get_fld(nc_file,var)
  time = get_fld(nc_file,'time')
  years = fix(time/365.+0.99)
  yindx = years - y0

  ps[*,*,yindx] = my_interpol2(ps0[*,*,yindx],xx3,yy3,xx,yy,tdim=1)
  
;stop
  pss[*,*,yindx] = ps[*,*,yindx] + pss[*,*,yindx]
  mcount[*,*,yindx] = 1 + mcount[*,*,yindx]

  mps_bin[*,yindx,im]  = slr_bin(ps[*,*,yindx], d22 = dj) 
  mps_bin2[*,yindx,im] = dj[*,yindx]

  print,max(pss)
  print,models[im], min(ps0), max(ps0), min(years),max(years)
;stop
 endfor
 jj = where(mcount gt 0,cnt)

;jump:
 if(cnt gt 0)then $
 pss[jj] = pss[jj]/mcount[jj]/100.  ; ensemble average

 mps_bin = mps_bin/100.
 mps_bin2 = mps_bin2/100.
 ps_bin = slr_bin(pss, d22 = ps_bin2) 
 ps_ens = pss
 filesav = dir0+'ps_ens_'+expid+'.sav0'
 save,file = filesav, ps_ens , ps_bin, ps_bin2, mps_bin, mps_bin2, models
 print,'file saved', filesav
 help,ps_ens,ps_bin,mps_bin

ENDFOR ; iexp


stop


zz = 0
dj2 = dd
iave=3
lev1 = cal_lev([-5.,5],20)
view3d, 'test', dj2, dd2=dj2, xx,yy,zz, iave,lev1,lev1*10


end

