
igif=1
iix =0

caseiap = 'core1'
print,'twp?? core?? caseiap # =twp??'
cn=''
read,cn
caseiap = 'core'+cn
caseiap = cn
;====================


;====================
fdir    = '../obs/IAP/'

id = strmid(caseiap,0,3)
case id of
'cor':filein1 =fdir+ caseiap+'.cam2.h0.1992-12-19-19800.nc'
'zmc':filein1 =fdir+ caseiap+'.cam2.h0.1992-12-19-19800.nc'
'twp':filein1 =fdir+ caseiap+'.cam2.h0.2006-01-22-19800.nc'
'zmt':filein1 =fdir+ caseiap+'.cam2.h0.2006-01-22-19800.nc'
 else: ;stop
endcase

;caseiap='junk'
;filein1 =fj
print,filein1

filein2 = filein1
y1 = get_fld(filein1,'lev')
y2 = get_fld(filein1,'ilev')
 x = get_fld(filein2,'time')
 y = get_fld(filein2,'lev')
 xrange=[min(x),max(x)]
 yrange = [1000.,100]
;====================

filein2 = filein1
gif_folder = 'scm_gifs/'+caseiap
if(igif)then begin
  spawn,'mkdir '+gif_folder
  print,'gif_folder:' ,gif_folder
endif

file= filein2
end
