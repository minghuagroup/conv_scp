
function mean_g,aa,lat,missing,amin=amin
   ny = n_elements(aa[0,*])
   nx = n_elements(aa[*,0])
   yy = aa*0.0
   for i=1,ny-1 do yy[*,i] = lat[i]*3.1416/180.
   
   cosy = cos(yy)
   jj = where(aa gt missing,cnt)
   if(cnt gt 0)then begin
     d = total(aa[jj]*cosy[jj])/total(cosy[jj])
   endif
   amin = min(aa[jj])
return,d
end


