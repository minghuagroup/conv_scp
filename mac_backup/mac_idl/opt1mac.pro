        pro opt1mac,outputfile,c,nv,nt,d
;                                         d(nv,nt)

          openw,ni,outputfile,/get_lun,width=nv*13
          print,'output in 1mac: ', outputfile,nv,nt
          dd=d
          if(n_elements(d[*,0]) ne nv) then dd=transpose(dd)

          printf,ni,c
          d1=fltarr(nv)
          for j=0,nt-1 do begin
            d1(*)=d(*,j)
            printf,ni,d1,format='(100E13.5)'
          endfor
          close,ni
         return
       end


