pro s_qt_p_2_t_qv_ql, s, qt,p, t, qv, ql

 cp = 1004.6
 Lv = 2.5e6

 T0 = (s - Lv*qt)/Cp
 qs = f_qswi(T0 - 273.16, p)
 
 if(qs gt qt)then begin
  T = T0
  qv = qt
  ql = 0.

;stop
  return
 endif

;stop
 ;T needs to be higher
 T1 = T0
 s1 = f_qswi(T1 - 273.16,p)*Lv + Cp*T1 

 T2 = T0+1.
 s2 = f_qswi(T2 - 273.16,p)*Lv + Cp*T2 

; (s-s1)/(T - T0) = (s2-s1)/(T2-T1)
 T = T0 + (s-s1)/((s2-s1)/(T2 - T1) )

 qv = f_qswi(T-273.16,p)
 ql = qt - qv

;stop
 return
 end
 

