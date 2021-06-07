pro bins2_c,nbin1,windex1,nbin2,windex2,crf,crfbin
 ;output crfbin
 crfbin=fltarr(nbin1,nbin2)-9999.0
 for ii=0,nbin1-1 do begin
  for jj=0,nbin2-1 do begin

  jjk=where((windex1 eq ii) and (windex2 eq jj),cnt)
  if(cnt gt 0)then crfbin[ii,jj] = total(crf[jjk])/n_elements(jjk)

 endfor
 endfor

 return
 end 


