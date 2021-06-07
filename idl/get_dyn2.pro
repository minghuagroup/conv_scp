
function get_dyn2,nstep,ftype,var,folder=folder  ; read from fout3d in mzfunctions
;===================================
;ftype = txt or txt1

if(not keyword_Set(folder))then folder = 'fout'

sstep = strtrim(1000000+nstep,2)
sstep = strmid(sstep,1,6)

files = findfile(folder+'/'+var+'_'+sstep+'_*.'+ftype)
help,files

nf = n_elements(files)
aa = fltarr(256,128)

;close,2
;openw,2,'aa.out'
 printf,2,''
for i=0,nf-1 do begin
 close,1
 openr,1,files(i)
 jj = intarr(5)
 readf,1,jj
   nstep= jj[0]
   nnx  = jj[1]
   nny  =  jj[2]
   nx0  =   jj[3]
   ny0  =   jj[4]
 
   bb  = fltarr(nnx,nny)
   bb2 = fltarr(nnx)
   for J=1,nny do begin
       readf,1,format='(1000(5E15.7/))',bb2
       bb[*,j-1] = bb2
   endfor
   close,1

 aa[*,ny0-1:ny0+nny-2 ] = bb  ;only for 2d decomposition 

endfor
 print,var,' ',min(aa),' ',max(aa)
;close,2

return,aa

end
