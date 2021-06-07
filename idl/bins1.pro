pro bins1,binmin,binmax,binint,w,nbin,binmid,binends,windex,ex=ex
 ;output binmid,windex ,ex=1, exclude the end points beyond
 ;if crf, bin 20 then crfbin(20) ;jj1=where(windex eq j) and sum 

 nbin=(binmax-binmin)/binint
 binends = fltarr(nbin+1)
 binmid  =fltarr(nbin)

 for i=0,nbin do binends[i]=binmin+i*binint
 binmid = binends[0:nbin-1]+binint/2.0

 windex=indgen(n_elements(w))*0-1

if(keyword_set(ex))then begin

 for ii = 0,nbin-1 do begin
  jj=where((w ge binends[ii]) and (w lt binends[ii+1]),cnt)
  if(cnt gt 0)then windex[jj]=ii
 endfor

endif else begin

 for ii = 1,nbin-2 do begin
  jj=where((w ge binends[ii]) and (w lt binends[ii+1]),cnt)
  if(cnt gt 0)then windex[jj]=ii
 endfor

  jj=where(w lt binends[1],cnt)
  if(cnt gt 0)then windex[jj]=0

  jj=where(w ge binends[nbin-1],cnt)
  if(cnt gt 0)then windex[jj]=nbin-1

endelse

return
end 


