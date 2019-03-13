;!! PHYSICS 2D


for nstep = 11486,11488 do begin
folder = 'fout'
var = 'q' 
var = 'T' 
var = 'PS' 
;======================

file = 'fout2/'+var+'_'+strdigit(nstep,0)+'.sav'

;goto,jump1

dd3 = get_phy3(nstep,var,folder=folder,x=xx,y=yy)

save,file=file,dd3,xx,yy
print,'file saved in ',file, ' for processing on mac workgcm/iap_phys3.pro'

endfor
stop

get_lev,aa,'Q',lev,scale



aa  = dd3
siz = size(aa)
if(siz[0] eq 3)then aa = reform(aa[*,*,k])
dd = aa*scale

lev1 = lev
lev2 = lev1*1000.
plot_map4_new,dd,dd,xx,yy,lev1,lev1*100,title=var
print,range
end

