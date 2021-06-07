
;PRO VOL,open0,close0,v1,v2,day=day

if(keyword_set(day))then dclose = close-open

;close0 = close
;open  = open0

close = close[160:*]
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

dclose1 = dclose1*100.
dclose2 = dclose2*100.

m0 = mean(dclose)
h0 = histogram(dclose,NBIN=30,locations=b0)
b0 = b0 + (b0[1]-b0[0])

m1 = mean(dclose1)
h1 = histogram(dclose1,binsize=0.5,locations=b1)
b1 = b1 + (b1[1]-b1[0])

m2 = mean(dclose2)
h2 = histogram(dclose2,binsize=0.5,locations=b2)
b2 = b2 + (b2[1]-b2[0])

db2 = b2[1]-b2[0]
bb2 = [b2[0]-db2,b2,max(b2)+db2]

nb2 = n_elements(b2)
sh2 = h2/total(h2)/db2
hh2 = [0.,sh2,0]

win64
plot,b0,h0,title='Ticker '+file+' daily DS histogram',color=colors.black
oplot,[m0,m0],[0.,max(h0)]*2


win64
plot,b1,h1,title='Ticker '+file+' daily % return histogram',color=colors.blue
oplot,[m1,m1],[0.,max(h1)]*2

win64
plot,b2,sh2,title='Ticker '+file+' daily % log return histogram',color=colors.red
oplot,[m2,m2],[0.,max(h2)]*2

yfit2 = GAUSSFIT(bb2, hh2,coeff, NTERMS=3)
print,coeff

v3=coeff[2]*sqrt(253.)
print,'v3 =',v3

syfit2 = yfit2/total(yfit2)/db2

oplot,bb2,syfit2,color=colors.blue


;yfit3 = coeff[0]*exp(- ((bb2-coeff[1])/coeff[2])^2/2.0)

x0    = (indgen(201)-100)*0.1
v20   = v2*100./sqrt(253.)
xim   = (x0 - mean(dclose2))/v20
yfitim = 1./sqrt(2.*3.1416)/v20*exp(-xim^2/2.0)
yfitim = yfitim/total(yfitim)/0.1
oplot,x0,yfitim


stop
;return

end

