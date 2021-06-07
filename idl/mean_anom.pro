pro mean_anom, dd1, nann, dd1m, dd1a, dd1p
  ; mean, anomaly, percentage

 sz = size(dd1)

 case sz[0] of
 1: begin  ;CO2
   nt = n_elements(dd1)
   nyrs  = fix( nt/nann)
   jj1   = indgen(nyrs) * nann

  ; nann is the lenghth of the cycle to be averaged
    
   dd1m = fltarr(nann)  ; annual cycle
   for i = 0,nann-1 do begin
    jj = jj1+i    ;dimension nyrs
    dd1m[i] = mean(dd1[*,*,jj])
   endfor
    end
 2: begin
     dd1m = dd1
     dd1a = dd1
     dd1p = dd1
    end
 3: begin
   nx = n_elements(dd1[*,0,0])
   ny = n_elements(dd1[0,*,0])
   nt = n_elements(dd1[0,0,*])
   nyrs  = fix( nt/nann)
   jj1   = indgen(nyrs) * nann

  ; nann is the lenghth of the cycle to be averaged
    
   dd1m = fltarr(nx,ny,nann)  ; annual cycle
   nyrs  = fix( nt/nann)
   for i = 0,nann-1 do begin
    jj = jj1+i    ;dimension nyrs
    dd1m[*,*,i] = ave3(dd1[*,*,jj],three=3)
   endfor

   dd1a = dd1*0.  ; anomaly
   for i=0,nt-1 do begin
     i1 = i mod nann
     dd1a[*,*,i] = dd1[*,*,i] - dd1m[*,*,i1]
   endfor

   dd1p  = dd1a*0.0
   
   for i1 = 0,nann-1 do begin
         djm = reform(dd1m[*,*,i1])
         jj = where(abs(djm) gt 1.0e-10,cnt)
    if(cnt gt 0)then begin
        
     for i=i1,nt-1,nann do begin
         i1 = i mod nann
         djp = reform(dd1a[*,*,i])*0.0
         dja = reform(dd1a[*,*,i])
           djp[jj] = dja[jj]/djm[jj]*100.
         dd1p[*,*,i] = djp
     endfor

     endif
   endfor
 end

 4: begin
    nz = n_elements(dd1[0,0,*,0])
    nx = n_elements(dd1[*,0,0,0])
    ny = n_elements(dd1[0,*,0,0])
    dd2m = fltarr(nx,ny,nz,nann)
    dd2a = dd1*0
    dd2p = dd1*0
    for k=0,nz-1 do begin
      dd3 = reform(dd1[*,*,k,*])
      mean_anom, dd3, nann, dd3m, dd3a, dd3p 
      dd2m[*,*,k,*] = dd3m
      dd2a[*,*,k,*] = dd3a
      dd2p[*,*,k,*] = dd3p
    end
    dd1m = dd2m
    dd1a = dd2a
    dd1p = dd2p
    end
  endcase

  return
  end
 
