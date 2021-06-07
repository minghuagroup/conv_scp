function replicate3, dd, lev, nc
; d is 1D

  np = n_elements(lev)
  sz = size(dd)

  dd2 = dd * 0.0

case sz[0] of
2: begin
  case nc of
  0:  for k=0,np-1 do dd2[k,*] = lev[k] 
  1:  for k=0,np-1 do dd2[*,k] = lev[k] 
  else:
  endcase
   end

3: begin
  case nc of
  0:  for k=0,np-1 do dd2[k,*,*] = lev[k] 
  1:  for k=0,np-1 do dd2[*,k,*] = lev[k] 
  2:  for k=0,np-1 do dd2[*,*,k] = lev[k] 
  else:
  endcase
   end

3: begin
  case nc of
  0:  for k=0,np-1 do dd2[k,*,*,*] = lev[k] 
  1:  for k=0,np-1 do dd2[*,k,*,*] = lev[k] 
  2:  for k=0,np-1 do dd2[*,*,k,*] = lev[k] 
  3:  for k=0,np-1 do dd2[*,*,*,k] = lev[k] 
  else:
  endcase
   end

else:
endcase

return,dd2

end
 

