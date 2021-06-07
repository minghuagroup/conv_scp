iplot=0
file = '../obs/IAP/core365.cam2.h0.1992-12-19-19800.nc'
file = '../obs/IAP/twp365.cam2.h0.2006-01-22-19800.nc'
file = '../obs/IAP/twp371.cam2.h0.2006-01-22-19800.nc'

cp = 1004.63
Rv = 287.4
g = 9.8
Lv = 2.5e6


x = time
y = lev

var='eps0'
dd = eu0/1000.
dd = eu2

dd = du0/1000.
dd = du2


var='buoy_up'
dd = buoy_up
dd = bb3

var = 'w_up'
dd = ww2

var = 'mu0'
dd = mu0
dd = mu2


get_lev,dd,var,lev1,scale
lev1 = lev1/2. ;buoy
;lev1 = lev1*2.

 aa=scale*dd
 bb=aa
 lev2 = lev1+9999.
 title = var
 xrange=[min(x),max(x)]
 yrange=[max(y),min(y)]

  window,/free,xsize=560.,ysize=500,title=title
  if(min(aa) ne max(aa))then $
   plot_4dhtml,aa,bb,x,y,lev1,lev2,xrange=xrange,$
      yrange=yrange,xtitle='time',tran=1,$
      title= title+' min:'+strtrim(min(aa),2)+'  max:'+strtrim(max(aa),2)+ $
      ' scaled by '+strtrim(scale,2)



end
