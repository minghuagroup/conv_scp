function statistics_confidence, ym, std, level=level, distribution = distribution, $
        precent=precent
; ym agaist a mean zero

 ; returns whether ym is significant
 
 if(not keyword_Set(distribution))then distribution = 'normal'
 if(not keyword_Set(level))then level = 0.95

 test = ym/std/sqrt(2.0)
 cdf = 0.5*(1+erf(test))

 if(keyword_Set(percent))then begin
  p = percent
  percent = sigma*sqrt(2.0)/erf(2*p-1.0)
 endif 

 if(cdf gt level) then return, 1 else return,0

 

end
  
 

