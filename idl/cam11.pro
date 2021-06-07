
igif=0
iix =0

caseiap = 'core1'
print,'twp as t# ?? core as c#?? caseiap # =twp??'
cn=''
read,cn
caseiap = cn
;====================


;====================
fdir    = '../obs/IAP/'

id = strmid(caseiap,0,1)
case id of
'c':filein1 =fdir+ 'core0.cam2.h0.1992-12-19-19800'+caseiap+'.nc'
'zm':filein1 =fdir+ 'zmc0.cam2.h0.1992-12-19-19800'+caseiap+'.nc'
't':filein1 =fdir+ 'twp0.cam2.h0.2006-01-22-19800'+caseiap+'.nc'
'zmt':filein1 =fdir+ 'zmt0.cam2.h0.2006-01-22-19800'+caseiap+'.nc'
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
file = filein2

filein2 = filein1
gif_folder = 'scm_gifs/'+caseiap
if(igif)then begin
  spawn,'mkdir '+gif_folder
  print,'gif_folder:' ,gif_folder
endif

print,'can manually change xrange=[x,xx] now'
print,'to be followed by run cam12.pro 
print,'                   or scm1 then scm2'
print,''

end
