igif=0
;run 00_range
;get aa,xx,yy,lev1

;goto,jumpo

restore,'data/pr.sav' ; gpcp
restore,'data/pr_model.sav' ; pr_model
 gpcp = gpcp
 data = pr_model

filels = '../obs/lsmask.nc'
 mask = get_fld(filels,'mask')
 mask2 = mask
 for i=0,359 do begin
  dj = reform(mask[i,*])
  dj = reverse(dj)
  mask2[i,*] = dj
 endfor

lev1 = cal_lev([-5,5.],20)
lev2 = [10000.,20000]

goto,jump_diff

VAR='PR'
files = 'data/pr.files'

file0 =''
close,1
openr,1,files

k=0
im = 6
ix=0
cfile0=''
 prec_model = fltarr(360,180,12)
 while (not eof(1)) do begin
;==============================
  k=k+1
  readf,1,cfile0

  file='data/m+'+cfile0  

  file='data/g+'+cfile0  
   xx = get_fld(file,'LON')
   yy = get_fld(file,'LAT')
   Data = get_fld(file,var)*86400.

;   d1 = ave3(data[*,*,3:5],three=3)
;   d2 = ave3(gpcp[*,*,3:5],three=3)

   aa = d1-d2
   jj=where(aa gt 1.0e3,cnt)
   aa[jj] = -9999.
; plot_map4,aa,aa,xx,yy,lev1,lev2*10,title=cfile0 ,$
;     xrange=[80.,140],yrange=[10.,50]

print,file
;read, ix

 prec_model = prec_model+data
 print,k

endwhile

   
   data = prec_model/(k*1.0)
   pr_model = data
   save,file='data/pr_mode.sav',xx,yy,pr_model

jump_diff:

   d1 = ave3(data[*,*,3:5],three=3)
   d2 = ave3(gpcp[*,*,3:5],three=3)
   aa = d1-d2
   jj=where(mask2 gt 0.0,cnt)
   jj=where(d2 lt 0.0,cnt)

lev1 = cal_lev([-5,5.],20)
lev1 = cal_lev([-3,3.],12)
   jj1 = where(aa ge max(lev1),cnt)
   if(cnt gt 0)then aa[jj1] = max(lev1)
   jj1 = where((aa le min(lev1)) and (aa ge -1000.),cnt)
   if(cnt gt 0)then aa[jj1] = min(lev1)
   aa[jj] = -9999.

window,/free,xsize=420.,ysize=350.
 plot_map4,aa,aa,xx,yy,lev1,lev2*10,title='' ,$
     xrange=[80.,140],yrange=[10.,50],latdel=10.,londel=20  ;,mapfill=3  ;, $

stop
 mygif,'data/pr_diff.gif'

  lev1 = cal_lev([-50.,100],15)
   aa = (d1-d2)/d2 *100.

   jj1 = where(aa ge max(lev1),cnt)
   if(cnt gt 0)then aa[jj1] = max(lev1)-0.001
   jj1 = where((aa le min(lev1)) and (aa ge -1000.),cnt)
   if(cnt gt 0)then aa[jj1] = min(lev1)
   aa[jj] = -9999.

window,/free,xsize=420.,ysize=350.
 plot_map4,aa,aa,xx,yy,lev1,lev2*10,title='' ,$
     xrange=[80.,140],yrange=[10.,50],latdel=10.,londel=20
 mygif,'data/pr_diff2.gif'

stop

prec_model = prec_model/(1.0*k)*86400.

jumpo:

obsfile = '../obs/gpcc_full_data_v7_10.nc'
 olon = get_fld(obsfile,'lon')
 olat = get_fld(obsfile,'lat')
 oprec = get_fld(obsfile,'p')

 nyears=20
 n1 = 85*12
 n2 = n1+nyears*12
; oprec = oprec[*,*,n1:n2-1]

 gpcp  = fltarr(360,180,12) - 9999.
  
  mns = indgen(nyears)*12
  for i1=0,359 do begin
  for j1=0,179 do begin
   i = i1 - 180
   if(i1 le 179)then i = i1+180
   j = 179 - j1   

   for im=0,11 do begin
   mn = im+mns
   dj = reform(oprec[i1,j1,mn])
   jj = where(dj ge 0.0,cnt)
   if(cnt gt 0)then begin
    dj2 = dj[jj]
    gpcp[i,j,im] = mean(dj2)*12./365
   endif
   endfor
 endfor
 endfor

 save,file='data/pr.sav',olon,olat,gpcp

stop

 mm = 6
 aa = reform(gpcp[*,*,mm])
window,/free,title='gridded'
plot_map4,aa,aa,xx,yy,lev1,lev1*100

 aa = reform(prec_model[*,*,mm])
window,/free,title='model'
plot_map4,aa,aa,xx,yy,lev1,lev1*100

 lev2 = cal_lev([-5,5],20)
 aa = reform(prec_model[*,*,mm]-gpcp[*,*,mm])
 jj = where(aa gt 9000.)
 aa[jj] = -9999.
window,/free,title='diff'
plot_map4,aa,aa,xx,yy,lev2,lev2*100

 lev3 = cal_lev([-50,50],20)
 aa = aa/gpcp[*,*,mm]*100.
; jj = where(aa gt 9000.)
; aa[jj] = -9999.
window,/free,title='diff%'
plot_map4,aa,aa,xx,yy,lev3,lev3*100

 lev2 = cal_lev([-5,5],20)
end
