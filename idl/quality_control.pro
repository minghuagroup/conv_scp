
function quality_control, dj, range=range, std_limit=std_limit, range2_limit=range2_limit, $
     nstd = nstd
; time series

 if(not keyword_Set(range))then range=[400.,800]
 if(not keyword_Set(std_limit))then std_limit = 20.
 if(not keyword_Set(range2_limit))then range2_limit=100.
 if(not keyword_Set(nstd))then nstd = 1

 dj2 = dj*0 - 999999.

  jj = where(dj le max(range) and (dj gt min(range)), cnt)
  if(cnt gt 0)then begin
   std = stddev(dj[jj])
   dev = dj - mean(dj[jj])
   range2 = max(dj[jj]) - min(dj[jj])
   jj2 = where(abs(dev) le nstd*std, cnt2)  ; 2 std
   if(cnt2 gt 0 and (std le std_limit) and (range2 le range2_limit))then begin
     dj2[jj2] = dj[jj2]
   endif
  endif

 return, dj2

end


