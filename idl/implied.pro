function implied, S, K, t, C

 N = 1000.
 CC = fltarr(N)
 sig0 = 0.01
 delta=1./N


 st = sqrt(t)
 for i=0,N-1 do begin
  sig = sig0 + delta*i*2. 
  CG  = option(S,K,t,sig)
  CC[i] = CG
 endfor

 diff = ABS(CC-C)
 jj   = where(diff eq min(diff))
 CC0  = CC[jj[0]]
 sig0 = sig0 + delta*jj[0]*2.

; stop
 return,sig0 
end


