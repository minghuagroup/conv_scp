
file2 = 'junk'
close,2
openr,2,file2
c=''
 for i=1,17 do readf,2,c

 dd = fltarr(256, 128, 35, 2)
 aa = fltarr(256)
 for m=0,1 do begin
 for k=0,34 do begin
 for j=0,127 do begin
  readf,2,aa
  dd[*,j,k,m] = aa
 endfor
 endfor
 endfor
close,2
 end
