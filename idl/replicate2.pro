
 function replicate2,d1,nt
   np = n_elements(d1)
   dd=fltarr(np,nt)
   for k=0,np-1 do dd[k,*]=replicate(d1[k],nt)
   return,dd
 end

