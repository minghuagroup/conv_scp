
function strdigit,a,n
 b=long(a*10^n)
 b=strtrim(b,2)
 len1=strlen(b)
 sz=size(b)

 ndot = '.'
 if(n eq 0)then ndot=''

 if(sz[0] eq 0)then begin
 c=strmid(b,0,len1-n)+ndot+strmid(b,len1-n,n)
 return,c
 endif

 if(sz[0] eq 1)then begin
  c=strarr(sz[1])
 for i=0,sz[1]-1 do begin
  c[i] =strmid(b[i],0,len1[i]-n)+ndot+strmid(b[i],len1[i]-n,n)
 endfor
 return,c
 endif

 if(sz[0] eq 2)then begin
  c=strarr(sz[1],sz[2])
 for i=0,sz[1]-1 do begin
 for j=0,sz[2]-1 do begin
  c[i,j] =strmid(b[i,j],0,len1[i,j]-n)+ndot+strmid(b[i,j],len1[i,j]-n,n)
 endfor
 endfor
 return,c
 endif

 if(sz[0] eq 3)then begin
  c=strarr(sz[1],sz[2],sz[3])
 for i=0,sz[1]-1 do begin
 for j=0,sz[2]-1 do begin
 for j=0,sz[3]-1 do begin
 c[i,j,k] =strmid(b[i,j,k],0,len1[i,j,k]-n)+ndot+strmid(b[i,j,k],len1[i,j,k]-n,n)
 endfor
 endfor
 endfor
 return,c
 endif
 
end


