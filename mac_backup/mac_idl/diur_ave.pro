
     function diur_ave,a,n1
       n = n_elements(a)/n1
       b=fltarr(n)
       for i=0,n-1 do begin
         i1=i*n1
         i2=i1+n1-1
         b[i]=total(a[i1:i2])/n1
       endfor
       return,b
      end



