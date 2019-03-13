
function get_fldcas3,nstep,ftype,var,folder=folder  ; read from fout3d in mzfunctions
;===================================
;ftype = txt or txt1

sstep = strtrim(10000+nstep,2)
sstep = strmid(sstep,1,4)
if(keyword_Set(folder))then folder = 'fout'

files = findfile(folder+'/'+var+'_'+sstep+'_*.'+ftype)
help,files

nf = n_elements(files)
aa = fltarr(256,31,128)

close,2
openw,2,'aa.out'
for i=0,nf-1 do begin
 close,1
 openr,1,files(i)
 jj = intarr(7)
 readf,1,jj
   nstep= jj[0]
   nnx  = jj[1]
   nnz  =  jj[2]
   nny  =  jj[3]
   nx0  =   jj[4]
   nz0  =   jj[5]
   ny0  =   jj[6]
 
   bb  = fltarr(nnx,nnz,nny)
   bb2 = fltarr(nnx)
   for J=1,nny do begin
   for K=1,nnz do begin
       readf,1,format='(1000(5E15.7/))',bb2
;       readf,1,bb2
;       stop
       bb[*,k-1,j-1] = bb2
   endfor
   endfor
   close,1

 aa[*,nz0-1:nz0+nnz-2,ny0-1:ny0+nny-2 ] = bb  ;only for 2d decomposition 

; print,i,files[i],min(bb),max(bb)
 printf,2,i,files[i],min(bb),max(bb)
endfor
 print,var,' ',min(aa),max(aa)
close,2

return,aa

end
