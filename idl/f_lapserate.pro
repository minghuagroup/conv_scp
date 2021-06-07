
function f_lapserate, T, p

 Cp = 1004.
 Rd = 287.
 Rv = 462.
 Lv = 2.5e6
 Gd = 9.8/Cp

 w = Lv*f_qs(T,p)/(Rd*T)
 D = Lv/(Cp*T)*(Rd/Rv)

 Gm = Gd*(1+w)/(1+D*w)

return,Gm

end

