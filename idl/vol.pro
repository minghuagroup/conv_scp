
PRO VOL,open0,close0,v1,v2,day=day

if(keyword_set(day))then dclose = close-open

close = close0
open  = open0

;close = close[150:250]
;close = close[0:90]

N  = n_elements(close)
N1 = N-1

dclose = close[1:N-1] - close[0:N-2] 


dclose1  = (close[1:N-1]-close[0:N1-1])/close[0:N1-1]  
dclose2 = alog(close[1:N-1]/close[0:N1-1])
 
M = 253.

v1      = STDDEV(dclose1)*sqrt(M)
v2      = STDDEV(dclose2)*sqrt(M)

print,'v1,v2'
print,v1,v2

;stop
return

end

