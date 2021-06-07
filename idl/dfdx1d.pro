function dfdx1d,d,x
; to do differencing with d/dx ; 

 n1=n_elements(d) 

 dw = (shift(d,-1)-shift(d,1))/(shift(x,-1)-shift(x,1))
 dw[0] = dw[1]
 dw[n1-1] = dw[n1-2]

  return,dw
 end
