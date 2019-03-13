
 function belongsto,var,vars
   jj=where(vars eq var,cnt)
   d=0
   if(cnt gt 0)then d=1
  return,d
 end
