
function get_fldvisstmn, yyyymm, var, scale1, days=days, m12=m12, ann=ann,file=filesav

; these 2 options not supported yer
 m12=0
 ann=0 

 date0 = yyyymm+'0100'
 var0 = visst_vars(var, scale1=scale1)

 mmdays = [31,28,31,30,31,30,31,31,30,31,30,31]
 ndays = mmdays[fix(strmid(yyyymm,4,2))-1]
 days  = n_elements(ndays)

 yyyy =strmid(yyyymm,0,4)
 datadir = 'vissta/'+yyyy
 if(not file_exists(datadir)) then spawn, 'mkdir '+datadir

 filesav = datadir+'/'+yyyymm+'_'+var0+'.sav' 

 if(not file_exists(filesav))then begin

    visstsavmn,yyyymm,restoreonly=1

 endif

 restore,filesav

 return,aa
end

;vars = ['CLDTOT','CLDLOW','CLDMED','CLDHGH','CLDLWP','CLDIWP','CLDTWP',$
; 'ALBEDO','FLNT','FSUSC','FSDSC','FLUSC','FLDSC',$
; 'FSUS','FLUS','FSDS','FLDS','FSNS','FLNS','COSZ','TS']

