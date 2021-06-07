k=25
title = 'mass flux'
window,/free,title=title
plot,mcrup[k,*],yrange=[-0.5,0.5]/10.,title=title
oplot,mcrdnu[k,*],color=colors.red   
oplot,mcrdns[k,*],color=colors.green
oplot,mcr[k,*],color=colors.blue    
mj = mcrup + mcrdnu + mcrdns
oplot,mj[k,*],color=colors.red  
oplot,mfcor[k,*]*100.,color=colors.red


title = 'cld'
window,/free,title=title
 plot,cld[k,*]   ,title=title
oplot,aup[k,*],color=colors.(0)
oplot,cor[k,*],color=colors.(3)
oplot,cdn[k,*],color=colors.(6)
oplot,sup[k,*],color=colors.(9) 
oplot,sdn[k,*],color=colors.(12)
oplot,env[k,*],color=colors.(15)
oplot,corecl[k,*],color=colors.(18)
oplot,mcrup[k,*]*5.,color=colors.red 
oplot,mcr[k,*]*10.,color=colors.blue  

title = 'w'
window,/free,title=title

 plot, wcor[k,*],title=title
 oplot,wsup[k,*],color=colors.(1)
 oplot,wcld[k,*],color=colors.(4)
 oplot,wsupa[k,*],color=colors.(7)
 oplot,-wcdn[k,*],color=colors.(10)

;stop
buoy_up = (dsecor-dse)*tabs/9.8

end
