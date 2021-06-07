function f_qswi, T, p
 es = f_eswi(T)*100.
 qs = 0.622*es/(p-es)
 return,qs
end

