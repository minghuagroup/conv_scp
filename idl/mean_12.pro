pro mean_12, dd1, nann, dd1m
  ; mean, anomaly, percentage

 large_number = 999999.
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
    ddj = reform(dd1[jj])
    jj2 = where(abs(ddj) lt large_number, cnt)
    if(cnt gt 0) then begin
      dd1m[i] = mean(ddj[jj2])
     endif else begin
      dd1m[i] = large_number
     endelse
   endfor
    end
 2: begin
     dd1m = dd1
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
    end
    dd1m = dd2m
    end
  endcase

  return
  end
 
