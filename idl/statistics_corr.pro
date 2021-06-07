
function statistics_corr, y1,y2, number=number, $
   sstd=sstd, smean = smean, confidence = confidence, percent=percent
 
 ; using bootstraping method to get the correlation between y1 and y2, and get the 
 ; standard deviation, y1 and y2 need to be same lenghth
 ; number is the number of bootstrap samples

 ; output: returns correlation between y2 and y1, the std and mean values of the samples

 nt = n_elements(y1)

 if(not keyword_Set(number))then number = 10000

 c = correlate(y1,y2)

corr = fltarr(number)

for ib = 0, number-1 do begin
 rn = nt*randomu(seed,nt)
 indx = fix(rn)
 jj = where(indx eq nt,cnt)
 if(cnt gt 0)then indx[jj] = nt-1
 yr1 = y1[indx]

 rn = nt*randomu(seed,nt)
 indx = fix(rn)
 jj = where(indx eq nt,cnt)
 if(cnt gt 0)then indx[jj] = nt-1
 yr2 = y1[indx]

 corr[ib] = correlate(yr1,yr2)
endfor

smean = mean(corr)
sstd = stddev(corr) 

if(keyword_Set(percent))then begin
 jj = where(corr le c,cnt)
 percent = cnt*1.0/number      
endif


return, c

end

