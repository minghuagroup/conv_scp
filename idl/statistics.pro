
y = reform(d11_era5[*,10]) ;12
y = reform(d11_era5[*,12]) ;12
x = indgen(69)*1.0

result = my_regress( x, y,yfit = yfit, sigma=sigma, ftest=ftest, chisq = chisq, ave = ave,$
    yy= yy , double=double,const=const, correlation=correlation)

help,result,sigma,ftest,chisq,const, correlation
print,result, sigma,ftest,chisq,const, correlation

plot,x,y,ynoz=1
oplot,x,y,psym=2
oplot,x,yfit,color=colors.red

auto = a_correlate(y,indgen(10))

efold = exp(-1.)
jj = where(auto lt efold)
jm  = min(jj)
Te  = max([jm-1,0])

EOD = n_elements(y)/2./Te
print,'Te, EOD',te,eod

y1 = y[0:9]
y2 = y[59:68]

ttest = tm_test(y2,y1)
help,ttest
print,ttest


diff = statistics_ttest(y1,y2, number=10000, sstd = sstd, smean=smean)
print,diff, smean, sstd

corr = statistics_corr(x,y, number=10000, sstd = sstd, smean=smean)
print,corr, smean, sstd
end

