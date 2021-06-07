
function f_qs,T,p

 T1 = T
 if(max(T) gt 100.)then T1=T-273.16
 es = f_es(T1)
 qs = 0.622*es/p
 return, qs
end

