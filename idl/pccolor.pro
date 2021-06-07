 pro pccolor
 restore,'~mzhang/idl/my_rain18.color'
 nlv=16
   r3 = congrid(r2[1:*],nlv-1,/interp,/minus_one) ; nlv intervals
   g3 = congrid(g2[1:*],nlv-1,/interp,/minus_one)
   b3 = congrid(b2[1:*],nlv-1,/interp,/minus_one)

 tvlct,[255,r3,0],[255,g3,0],[255,b3,0]   ; 0 white, nm black
return
end
