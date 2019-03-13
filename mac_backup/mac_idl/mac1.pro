        pro mac1,outputfile,c,d
;                                         d(nv,nt)
          cc = strtrim(c,2)
          cc=strsplit(cc,' ')
          nv = n_elements(cc)
          n = n_elements(d)
          nt = n/nv

          dd = reform(d,nt,nv)
          dd=transpose(dd)

         close,99
         ni = 99
         openw,ni,outputfile,width=nv*13
;          dd=d
;          if(n_elements(d[*,0]) ne nv) then dd=transpose(dd)

;          nt = n_elements(dd(0,*))
          print,'output in 1mac: ', outputfile,nv,nt

          printf,ni,c
          d1=fltarr(nv)
          for j=0,nt-1 do begin
            d1(*)=reform(dd(*,j))
            printf,ni,d1,format='(100E13.5)'
          endfor
          close,ni
         return
       end


