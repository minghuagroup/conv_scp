function ymd_now

  c=systime(0)
  b=bin_date(c)+100
  d=strtrim(b,2)
  yr=strmid(d[0],2,2)
  mn=strmid(d[1],1,2)
  dd = strmid(d[2],1,2)
  st=yr+'_'+mn+'_'+dd

; stop
 return,st
end 

