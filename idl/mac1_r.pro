        function mac1_r,outputfile, c=c

         close,99
         ni = 99

         c=''
         openr,ni,outputfile
         readf,ni,c
          cc = strtrim(c,2)
          cc=strsplit(cc,' ')

          nv = n_elements(cc)
          d1 = fltarr(nv)

         dd = fltarr(1000,nv)
         k = 0
         while not eof(ni) do begin
          readf,ni,d1
          dd[k,*] = d1
          k = k+1
         endwhile
         dd = dd[0:k-1,*]

          n = n_elements(d)
          nt = n/nv

          close,ni
         return,dd
       end


