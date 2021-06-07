;!!! DYNAMICS
rd = 287.6
b0 = 87.8

nstep0 = 10603
nstep1 = 10605

vars2d = ['psa','pt']
vars3d = ['ply','tt','ut','vt','deltac']


txt = 'txt' ;txt1,txt2...
folderout = 'fout'

dlat = 180./128
xx = indgen(256)*dlat
yy = 90.-indgen(128)*dlat

close,2
openw,2,'aa.out'

for nstep = nstep0,nstep1 do begin
;===================================
 for iv2 = 0,n_elements(vars2d)-1 do begin
   var = vars2d[iv2]  
   dd3 = get_dyn2(nstep,txt,var,folder=folderout)
   file = 'fout2d/'+var+'_'+strdigit(nstep,0)+'.sav'
   save,file=file,dd3,xx,yy
   print,'file saved in ',file, ' for processing on mac workgcm/iap_dyn3.pro'
 endfor

 for iv3 = 0,n_elements(vars3d)-1 do begin
   var = vars3d[iv3]  
   dd2 = get_dyn3(nstep,txt,var,folder=folderout)
   dd3 = transpose(dd2,[0,2,1])
   file = 'fout2d/'+var+'_'+strdigit(nstep,0)+'.sav'
   save,file=file,dd3,xx,yy
   print,'file saved in ',file, ' for processing on mac workgcm/iap_dyn3.pro'
 endfor

ix=1
if(ix eq 0)then begin
lev1 = cal_lev([500.,1050],11)
lev2=lev1+1.0e5
title = var+' n= '+strdigit(nstep,0)+' min= '+strdigit(min(dd),3)+' min= '+strdigit(max(dd),3)+$
     ' scale='+strdigit(scale,3)
plot_map4_new,dd,dd,xx,yy,lev1,lev2,title=title
;mygif,'gifs/'+var+'.gif'
read,ix
endif

endfor ;nstep

close,2

end
