igif=0
;run 00_range
;get aa,xx,yy,lev1

VAR='PR'
files = 'data/pr.files'
lev1 = cal_lev([0,20],20)

;;var='TAS'
;;files = 'data/tas.files'
;;lev1 = cal_lev([-10,30],20)


file0 =''
close,1
openr,1,files

 k=0
im = 6
ix=0
cfile0=''
 while (not eof(1)) do begin
;==============================
  k=k+1
  readf,1,cfile0

  file='data/m+'+cfile0  
   xx = get_fld(file,'LON')
   yy = get_fld(file,'LAT')
   Data = get_fld(file,var)
 if(var eq 'TAS')then aa = reform(data[*,*,im])-273.16
 if(var eq 'PR')then aa = reform(data[*,*,im])*86400.
;window,/free
plot_map4,aa,aa,xx,yy,lev1,lev1*10
print,file

  file='data/g+'+cfile0  
   xx = get_fld(file,'LON')
   yy = get_fld(file,'LAT')
   Data = get_fld(file,var)
 if(var eq 'TAS')then aa = reform(data[*,*,im])-273.16
 if(var eq 'PR')then aa = reform(data[*,*,im])*86400.
window,/free,title='gridded'
plot_map4,aa,aa,xx,yy,lev1,lev1*10

print,file
read, ix


endwhile


end
