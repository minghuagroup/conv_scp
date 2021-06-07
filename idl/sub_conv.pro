;================================================================
pro get_eps_prof,z,dz,zb,zt,dz1,dz2, jb2, jt2, jd2 ,eps,del,ratio
;================================================================
; input z(np),dz(np), zb,zt,dz1,dz2 to get 
; jb2, jt2, jd2 and eps(np),del(np),ratio
;================================================================

 zz = (z-zb)/(zt-zb)
 eps = 0.0
 del = 0.0
 
 zj  = abs(z-zb)
 jj = where(zj eq min(zj))
 jb = jj[0]

 zj  = abs(z-zt)
 jj = where(zj eq min(zj))
 jt = jj[0]

 zb2 = zb + dz1*(zt-zb)
 zj  = abs(z-zb2)
 jj = where(zj eq min(zj))
 jb2 = jj[0]

 zt2 = zt - dz2*(zt-zb)
 zj  = abs(z-zt2)
 jj = where(zj eq min(zj))
 jt2 = jj[0]

 zd2 = zt - 0.5*(zt-zb)

 zj  = abs(z-zd2)
 jj = where(zj eq min(zj))
 jd2 = jj[0]
 print,jt2,jb2,jd2

 
 eps = z*0.0
 del = z*0.0

 jj=where((zz ge 0.0) and (zz le dz1))
  eps[jj] = (1. - zz[jj]/dz1)

 jj=where((zz ge 1.-dz2) and (zz le 1.))
  del[jj] = (zz[jj] -(1.-dz2)) /dz2

 teps = total(eps[jb2-1:jb]*dz[jb2-1:jb] )
 tdel = total(del[jt:jt2+1]*dz[jt:jt2+1] )


 ratio = teps/tdel

 eps = eps
 del = ratio*del
 del[jt] = del[jt] + 1./dz[jt]

;stop

 return
end


;================================================================
pro get_parcel2, t0, q0, jb, jt, tmn, qmn, z,p, eps, tp, qvp, qlp,buoy,jt2
;================================================================
; input h0,q0, tmn, qmn, z,p, eps; p in mb!
; output tp, qvp, qlp, buoy, jt2
;================================================================

cp = 1004.63
Rv = 287.4
g = 9.8
Lv = 2.5e6


 hmn = Cp*tmn + g*z + Lv*qmn
 tp = tmn
 qvp = qmn
 qlp = qmn*0.0
 qtp = qmn
 hp = hmn

 tp[jb]  = t0
 qtp[jb] = q0
 hp[jb]  = Cp*tp[jb] + g*z[jb] + Lv*qtp[jb]

 for k= jb-1, jt, -1 do begin
  hp[k] = hp[k+1] + eps[k]*(hmn[k] - hp[k+1])*(z[k]-z[k+1])
  qtp[k] = qtp[k+1] + eps[k]*(qmn[k] - qtp[k+1])*(z[k]-z[k+1])
 endfor

 sp = (hp - g*z)

 for k=jb, jt, -1 do begin
     s_qt_p_2_t_qv_ql, sp[k],qtp[k],p[k]*100.,tj,qvj,qlj
  ;   print,p[k],tmn[k],sp[k]/cp,qtp[k],tj,qvj,qlj
     
     tp[k] = tj
     qvp[k] = qvj
     qlp[k] = qlj
 endfor

 jt2 = -1

 buoy = g*(tp - tmn)/tmn - qlj*g

 jj = where(buoy gt 0.0,cnt)
  if(cnt gt 0.)then begin
   jt2 = min(jj)
  endif

; stop
 return
end
 
  
