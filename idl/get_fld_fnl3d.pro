 function get_fld_fnl3d,filein,var,plev
;combine different fields together

  np =n_elements(plev)
  for k=0,np-1 do begin
    var1 = var+'_'+strtrim(fix(plev[k]),2)+'mb'
    dk = get_fld(filein,var1)
    if(k eq 0)then begin
      dd = dk
    endif else begin
      dd = [[dd],[dk]]
    endelse
   endfor
   nx=n_elements(dk[*,0])
   ny=n_elements(dk[0,*])
   dd  = reform(dd,nx,ny,np)

 jj=where(dd ge 10.e15,cnt)
 if(cnt gt 0)then dd[jj] = -9999.

 return,dd
 end


