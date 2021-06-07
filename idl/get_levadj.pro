
;===============================================================
function get_levadj , dd, LEV
;===============================================================
; adjust intervals based on dd

  large_number = 9999.

 lev0 = lev
   

     jj = where(abs(dd) le large_number ,cnt)
     if(cnt eq 0)then return, lev0

    lev0_int0 = (lev0[1] -lev0[0])

    dmin = min(dd[jj]) 
    dmax = max(dd[jj]) 

    lev0_int2 = (dmax-dmin)/20.

    lev0_int = lev0_int2

    if(lev0_int2 lt lev0_int0/2.0) then begin 
        lev0_int = lev0_int0/2. 
    endif else begin 
    if(lev0_int2 lt lev0_int0/2.0) then begin
       lev0_int = lev0_int0/2.
    endif else begin
     if (lev0_int2 lt lev0_int0/4.0) then begin
       lev0_int = lev0_int0/4.
    endif else begin
      if(lev0_int2 lt lev0_int0/10.0) then begin
       lev0_int = lev0_int0/10.
    endif else begin
      if (lev0_int2 lt lev0_int0/100.0) then begin
       lev0_int = lev0_int0/100.
    endif else begin 
       if(lev0_int2 gt lev0_int0*2) then begin
       lev0_int = lev0_int0*2
       endif
    endelse 
    endelse 
    endelse 
    endelse 
    endelse 

    levj = (indgen(200)-100)*lev0_int  + lev[0]

    jj = where(levj le dmax and (levj ge dmin), cnt)
    if(cnt le 1 or cnt gt 40)then begin
      levj = cal_lev([dmin,dmax],20) 
    endif else begin
      levj = levj[jj]
    endelse
    
  return, [levj[0] - lev0_int, levj, levj[n_elements(levj)-1] + lev0_int]

end
 

