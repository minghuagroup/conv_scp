
file = '../data/lens_ANN_climo.nc'

 lon = get_fld(file,'lon')
 lat = get_fld(file,'lat')
 lev = get_fld(file,'lev')
 ilev = get_fld(file,'ilev')
 PS = get_fld(file,'PS')

 hyam= get_fld(file,'hyam')
 hybm= get_fld(file,'hybm')
 hyai= get_fld(file,'hyai')
 hybi= get_fld(file,'hybi')

 nlon = n_elements(lon)
 nlat = n_elements(lat)
 np   = n_elements(lev)
 P0 = 100000. ;get_fld(file,'P0')

 p  = fltarr(nlon,nlat,np)
 pi = fltarr(nlon,nlat,np+1)
 for k=0,np-1 do begin
   p[*,*,k] = hyam[k]*p0+hybm[k]*ps[*,*]
 endfor
 for k=0,np do begin
   pi[*,*,k] = hyai[k]*p0+hybi[k]*ps[*,*]
 endfor

 T  = get_fld(file,'T')
 Q  = get_fld(file,'Q')
 U  = get_fld(file,'U')
 V  = get_fld(file,'V')
 QRL = get_fld(file,'QRL')
 QRS = get_fld(file,'QRS')
 FLNT = get_fld(file,'FLNT')
 FSNT = get_fld(file,'FSNT')
 FLNS = get_fld(file,'FLNS')
 FSNS = get_fld(file,'FSNS')
 Z = get_fld(file,'Z3')
 DTCOND = get_fld(file,'DTCOND')
 DCQ = get_fld(file,'DCQ')
 DTV = get_fld(file,'DTV')
 LHFLX = get_fld(file,'LHFLX')
 SHFLX = get_fld(file,'SHFLX')
 OMEGA = get_fld(file,'OMEGA')
 PHIS= get_fld(file,'PHIS')
 PRECC= get_fld(file,'PRECC')
 PRECL= get_fld(file,'PRECL')
 PRECSC = get_fld(file,'PRECSC')
 PRECSL = get_fld(file,'PRECSL')
 RELHUM = get_fld(file,'RELHUM')
 TS = get_fld(file,'TS')
 Z3 = get_fld(file,'Z3')
 TREFHT= get_fld(file,'TREFHT')

 g = 9.8
 Cp = 1004.
 Lv = 2.5e6
 Rd = 287.
 RV = 461.
 Radius = 6400e3

 S = T + g/Cp*Z3
 h = S + Lv/Cp*Q
 theta = T*(P0/P)^(Rd/Cp)
 thetae = theta*Relhum^(-Rv/Cp*Q) * exp(Lv*Q/Cp/T)
 es = f_eswi(T-273.16)*100.
 qs = 0.622*es/(p-es)

 grh = rh*alog(rh)

 gamma = dfdp3(T,Z3)*1000.

 x2 = lon
 y2 = lat
 z2 = lev
 iave = 1
 dd = h
 nlev = 50

 dd = dd[*,*,12:*]
 z2 = lev[12:*]
 lev1 = cal_lev([-12,0],24)
 lev1 = cal_lev([0,10],24)
 lev2 = lev1
 view3d,var,dd,x2,y2,z2,iave,lev1,lev2

 dd = Rd^2 * T/(p^2*g)*(g/Cp + gamma)/1.0e4  ; meter  ; 100-200 range
 dd = dd[*,*,12:*]

 view3d,var,dd,x2,y2,z2,iave,cal_lev(dd,nlev),cal_lev(dd,nlev)

end


