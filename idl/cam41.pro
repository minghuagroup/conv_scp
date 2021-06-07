 print, 'to run iap0 first, here to prepare for aa zz,xx etc first'

 
 print,'filenumber prec???? '
 caseid='' 
 read,caseid
 ;file='../obs/IAP/zyx'+c+'.cam2.h0.1979-07-28-00000.nc'
 ;file='../obs/IAP/zyx'+c+'.cam2.h0.1979-07-29-00000.nc'
 file='../obs/IAP/prec'+caseid+'.nc'

 ;var = 'PRECC'
 ;var = 'PRECT'
 ;aa  = get_fld(file,var)*8.64e7
lev1 = cal_lev([0.,20],20)
 ;var = 'RELHUM'
 ;aa  = get_fld(file,var)
 ;var = 'CLOUD'
 ;aa  = get_fld(file,var)*100.
lev1 = cal_lev([0.,100],20)
lev2 = [2000.,4000]


 xx = get_fld(file,'lon')
 yy = get_fld(file,'lat')
 ;zz = get_fld(file,'lev')
 

; aa  = ave3(dd3[*,*,9:16],three=3) *100.


print,'run iap42.pro to plot'

end
