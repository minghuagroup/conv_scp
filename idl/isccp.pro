
filein = '../obs/isccp/ISCCP.HGM.v01r00.GLOBAL.2009.12.99.9999.GPC.10KM.CS00.EQ1.00.nc'

levrh = get_fld(filein,'levrh')
rh    = get_fld(filein,'rh_profile')
eqlon = get_fld(filein,'eqlon')
eqlat = get_fld(filein,'eqlat')
lon = get_fld(filein,'lon')
lat = get_fld(filein,'lat')
humtab = get_fld(filein,'humtab')

for k=5,5 do begin
 print,levrh[k]
 drh = reform(rh[*,k])
 daa  = my_interpol_isccp(drh,eqlon,eqlat,humtab,lon,lat)
endfor



;========
aa = daa
xx = lon
yy = lat
lev1 = cal_lev([10.,110],20)
var = 'RH'
igif=0

end
