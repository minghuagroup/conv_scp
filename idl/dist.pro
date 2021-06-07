
;pro vol,open0,close0,v2,v2d,day=day

;close = close0
;open = open0

;close = close[150:250]
;close = close[0:90]

N  = n_elements(close)
N1 = N-1

dclose = shift(close,-1)-close

if(keyword_set(day))then dclose = close-open

dclose  = dclose[0:N1-1]/close[0:N1-1]  ; normalized
dclose2 = alog(close[1:N1]/close[0:N1-1])
std1    = stddev(dclose) *sqrt(253.)
std2    = stddev(dclose2)*sqrt(253.)

print,std1,std2
stop

mclose = mean(close[0:N1-1])
mdclose = mean(dclose[0:N1-1])

N2  = 365.
v0  = stddev(close[0:N1-1])/mclose
v0  = sqrt(2.0)*v0
v2  = sqrt(mean( dclose^2 )*N2)

ddclose = dclose-mdclose
dsmut   = ddclose 

v2d  = sqrt(mean(ddclose*ddclose)*N2 )

;print,'v0,v2,v2d, mut'
;print,v0,v2,v2d,mdclose*365.*253./N1

;stop
;return

end

