;!! PHYSICS 2D

;040644
;for nstep = 40644,40645 do begin
for nstep = 40644 ,40644 do begin
folder = 'fout'
var = 'q' 
var = 'T' 
var = 'ts' 
;======================

file = 'fout/'+var+'_'+strdigit(nstep,0)+'.sav'

;goto,jump1

dd3 = get_phy3(nstep,var,folder=folder,x=xx,y=yy)

save,file=file,dd3,xx,yy
print,'file saved in ',file, ' for processing on mac workgcm/iap_phys3.pro'

endfor
;stop

get_lev,aa,'T',lev,scale



aa  = dd3
siz = size(aa)
if(siz[0] eq 3)then aa = reform(aa[*,*,k])
dd = aa*scale

;lev1 = lev
lev1 = cal_lev([190.,340.],15)
lev2 = lev1 ;*1000.
;plot_map4_new,dd,dd,xx,yy,lev1,lev1*100,title=var
plot_map4,dd,dd,xx,yy,lev1,lev2,title=var
;print,range
end

