
function statistics_ttest, y1,y2, number=number, $
   sstd=sstd, smean = smean, precentage = percentage
 
 ; using bootstraping method to get the mean difference between y1 and y2, and get the 
 ; standard deviation, y1 and y2 need not to be same lenghth
 ; number is the number of bootstrap samples

 ; output: returns difference between y2 and y1, the std and mean values of the samples

 nt1 = n_elements(y1)
 nt2 = n_elements(y2)

 nt = nt1 + nt2

 if(not keyword_Set(number))then number = 10000
 
 y1m = mean(y1)
 y2m = mean(y2)
 ydiff = y2m - y1m

 ysample = [y1,y2-ydiff]

 diff = fltarr(number)

for ib = 0, number-1 do begin
 rn = nt*randomu(seed,nt1)
 indx = fix(rn)
 jj = where(indx eq nt,cnt)
 if(cnt gt 0)then indx[jj] = nt-1
 m1 = mean(ysample[indx])

 rn = nt*randomu(seed,nt2)
 indx = fix(rn)
 jj = where(indx eq nt,cnt)
 if(cnt gt 0)then indx[jj] = nt-1
 m2 = mean(ysample[indx])

 diff[ib] = m2 - m1
endfor

sstd = stddev(diff) 
smean = mean(diff)

if(keyword_Set(percent))then begin
 jj = where(diff le ydiff,cnt)
 percent = cnt*1.0/number 
endif

return, ydiff

end

