     function diur_ave2,a,n1
       np = n_elements(a[*,0])
       n  = n_elements(a[0,*])/n1

       ave=fltarr(np,n)
       for i=0,np-1 do begin
        ave[i,*]=diur_ave(reform(a[i,*]),n1)
       endfor
       return,ave
     end         


