
 function belongsto,var,vars
 if(n_elements(vars) le 0)then return, 0
   jj=where(vars eq var,cnt)
   d=0
   if(cnt gt 0)then d=1
  return,d
 end
