igif=0

;goto,jump2
;run 00_range
;get aa,xx,yy,lev1


var='TAS'
files = 'data/tas2.files'
lev1 = cal_lev([-5,5],20)


;;goto,jumpo
file0 =''
close,1
openr,1,files

obsfile = '../obs/era_tas.nc'
 olon = get_fld(obsfile,'LON')
 olat = get_fld(obsfile,'LAT')
 otas = get_fld(obsfile,'TAS') - 273.16

restore, 'data/tas_mode.sav'
 data = tas_model 
restore,'data/pr.sav' ; gpcp
;restore,'data/pr_model.sav' ; pr_model
 gpcp = gpcp

   d1 = ave3(data[*,*,6:8],three=3)
   d2 = ave3(otas[*,*,6:8],three=3)
   d3 = ave3(gpcp[*,*,0:11],three=3)
   aa = d1-d2
   jj=where(d3 lt 0.0,cnt)

lev2 = [1000.,2000]
lev1 = cal_lev([-3,3.],12)
;lev1 = cal_lev([-3,3.],12)
   jj1 = where(aa ge max(lev1),cnt)
   if(cnt gt 0)then aa[jj1] = max(lev1)-0.01
   jj1 = where((aa le min(lev1)) and (aa ge -1000.),cnt)
   if(cnt gt 0)then aa[jj1] = min(lev1)
   aa[jj] = -9999.
 window,/free,xsize=420.,ysize=350.
 plot_map4,aa,aa,xx,yy,lev1,lev2,title='' ,$
     xrange=[240.,300],yrange=[10.,50],latdel=10.,londel=20  ;,mapfill=3  ;, $
 mygif,'data/tas_diff.gif'

stop





k=0
im = 6
ix=0
cfile0=''
 prec_model = fltarr(360,180,12)
 while (not eof(1)) do begin
;==============================
  k=k+1
  readf,1,cfile0

  file='data/g+'+cfile0  
   xx = get_fld(file,'LON')
   yy = get_fld(file,'LAT')
   Data = get_fld(file,var) - 273.16
   d1 = ave3(data[*,*,5:7],three=3)
   d2 = ave3(otas[*,*,5:7],three=3)
   aa = d1-d2
; plot_map4,aa,aa,xx,yy,lev1,lev1*10,title=cfile0,$
;     xrange=[240.,300],yrange=[20.,60]

print,file
;read, ix

 prec_model = prec_model+data
 print,k

endwhile


prec_model = prec_model/(1.0*k) ;- 273.16

tas_model=prec_model
save,file='data/tas_mode.sav',xx,yy,tas_model

jumpo:



 nyears=20
 n1 = 85*12
 n2 = n1+nyears*12
; otas = otas[*,*,n1:n2-1]

 gpcp  = fltarr(360,180,12) - 9999.
 gpcp  = otas
jump2:
  
 mm = 1
 aa = reform(gpcp[*,*,mm])
;window,/free,title='gridded'
;plot_map4,aa,aa,xx,yy,lev1,lev1*100

 aa = reform(prec_model[*,*,mm])
;window,/free,title='model'
;plot_map4,aa,aa,xx,yy,lev1,lev1*100

 lev2 = cal_lev([-5,5],20)
 aa = reform(prec_model[*,*,mm]-gpcp[*,*,mm])
 jj = where(aa gt 9000.)
 aa[jj] = -9999.
window,/free,title='diff'
plot_map4,aa,aa,xx,yy,lev2,lev2*100

stop

 lev3 = cal_lev([-50,50],20)
 aa = aa/gpcp[*,*,mm]*100.
; jj = where(aa gt 9000.)
; aa[jj] = -9999.
window,/free,title='diff%'
plot_map4,aa,aa,xx,yy,lev3,lev3*100

 lev2 = cal_lev([-5,5],20)
end
