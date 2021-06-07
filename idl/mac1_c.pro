        pro mac1_c,outputfile,c,id_out,d
;                                         id_out[i],d(nv,nt)
          cc = strtrim(c,2)
          cc=strsplit(cc,' ')
          nv = n_elements(cc)-1
          n = n_elements(d)
          nt = n/nv

          dd = reform(d,nt,nv)
;;          dd=transpose(dd)

         close,99
         ni = 99
         openw,ni,outputfile,width=nv*40
;          dd=d
;          if(n_elements(d[*,0]) ne nv) then dd=transpose(dd)

;          nt = n_elements(dd(0,*))
          print,'output in 1mac: ', outputfile,nv,nt

          printf,ni,c
          d1=fltarr(nv)
          for j=0,nt-1 do begin
            ;d1(*)=reform(dd(*,j))
            d1(*)=reform(d(*,j))
            printf,ni,id_out[j],d1,format='(1x,a10,40E13.5)'
          endfor
          close,ni
         return
       end


