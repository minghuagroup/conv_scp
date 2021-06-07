function my_regress, x, y,yfit = yfit, sigma=sigma, ftest=ftest, chisq = chisq, ave = ave,$
    yy= yy , double=doublei, min_val = min_Val,$
   const = const, correlation=correlation

 if(not keyword_set(min_Val))then min_val = -999999.
 jj = where(x gt min_val and (y gt min_val),cnt) 

 if(cnt lt 5)then return,0

 x2 = x[jj]
 y2 = y[jj]

 yy = x*0.0

 result = regress(x2,y2,yfit=yfit, FTEST=FTEST, CHISQ=CHISQ, SIGMA=SIGMA, double=double,$
   const = const, correlation=correlation)
 yfit = reform(yfit)

 ave = mean(y2)

 yy[jj] = y2 - yfit + ave
; print,sigma[0],ftest,chisq
;;help,x2,y2,x,yy
 
 return, result

end
