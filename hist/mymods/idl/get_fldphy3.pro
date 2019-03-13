
function get_fldphy3,nstep,var,folder=folder,x=x,y=y  ; read from fout3d in mzfunctions
;===================================
;ftype = txt or txt1

sstep = strtrim(10000+nstep,2)
sstep = strmid(sstep,1,4)
if(not keyword_Set(folder))then folder = 'fout'

files = findfile(folder+'/'+var+'_'+sstep+'_*.txt')
help,files
;stop
nf = n_elements(files)
aa = fltarr(256,30,128)
aa2 = fltarr(256,128)

dy = 180./127.
dx = 180./128.
x  = indgen(256)*dx
y = 90.-indgen(128)*dy

close,2
openw,2,'aa.out'
for i=0,nf-1 do begin
 close,1
 openr,1,files(i)
 jj = intarr(4)
 readf,1,jj
   nstep= jj[0]
   lchnk = jj[1]
   nnx  = jj[2]
   nnz  =  jj[3]
 
   bb  = fltarr(nnx,nnz)
   bb2 = fltarr(nnx)
   lat = fltarr(nnx)
   lon = fltarr(nnx)

   c = ''
       readf,1,c
       readf,1,format='(1000(5F15.3/))',lat
       readf,1,c
       readf,1,format='(1000(5F15.3/))',lon

  if(nnz eq 1)then begin
       readf,1,format='(1000(5E15.7/))',bb2
       endif else begin
   for K=1,nnz do begin
       readf,1,format='(1000(5E15.7/))',bb2
       bb[*,k-1] = bb2
   endfor
  endelse
   close,1

lat = 90.-lat   
if(nnz eq 1)then begin
for ij = 0,nnx-1 do begin
   ii = fix(lon(ij)/dx+0.01) 
   jj = fix(lat(ij)/dy+0.01) 
   aa2[ii,jj] = bb2[ij]  ;only for 2d decomposition 
endfor
       endif else begin
for ij = 0,nnx-1 do begin
   ii = fix(lon(ij)/dx+0.01) 
   jj = fix(lat(ij)/dy+0.01) 
   aa[ii,*,jj] = bb[ij,*]  ;only for 2d decomposition 
endfor
endelse

; print,i,' ',files[i],min(bb),max(bb)
 printf,2,i,files[i],min(bb),max(bb)
endfor  ; files
close,2

if(nnz eq 1)then aa=aa2
help,aa,x,y
print,var,' ',min(aa),max(aa)
return,aa

end
