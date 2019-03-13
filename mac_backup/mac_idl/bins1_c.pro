pro bins1_c,nbin,windex,crf,crfbin
 ;output crfbin
 crfbin=fltarr(nbin)-9999.0
 for ii=0,nbin-1 do begin
  jj=where(windex eq ii,cnt)
  if(cnt gt 0)then crfbin[ii] = total(crf[jj])/n_elements(jj)
 endfor

 return
 end 


