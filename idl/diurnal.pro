pro diurnal,dd,amm, amp,ampp,phase,lon,nw = nw

  if(not keyword_Set(nw))then nw = 2
  am = fft(dd,-1)
  amm = real_part(am[0])
  bm = am
  bm[0] = complex(0.0,0.0)
  for j=nw+1,23-nw do begin
    bm[j] = bm[0]
  endfor
  ;bm[nw+1:23-nw] = bm[0]
  
  dd2 = fft(bm,1)

  amp = 0.0
  for i = 1,nw do begin 
   a = real_part(bm(i))
   b = imaginary(bm(i))
   amp = amp + a*a + b*b
  endfor
  amp  = sqrt(amp)

  ampp = 0.0
  if(amm gt 1.0e-20)then begin
    ampp = amp/amm*100.
  endif

  jj = where(dd2 eq max(dd2))
  jj = jj[0]
  phase = lon/15. + jj[0]
  if(phase gt 24.)then phase = phase-24.

  return
end   

 

