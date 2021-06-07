ip= 1 ;nplume-1
diag='tend'

case diag of
'tend': begin

varp =icase2 + '_qtend_ip=' + strtrim(ip,2)
window,/free,title=varp
dd=qtend*86400*1000                                                        
plot,reform(dd[ip,*,it]),y2,yrange=[1000.,100],title=varp,xrange=[-5.,5]
dd=qtendcond*86400*1000                                                    
oplot,reform(dd[ip,*,it]),y2,color=colors.red                              
dd=qtendevap*86400*1000                      
oplot,reform(dd[ip,*,it]),y2,color=colors.blue
dd=qtendtranup*86400*1000                     
oplot,reform(dd[ip,*,it]),y2,color=colors.green,linest=2
dd=qtendtrandn*86400*1000                      
oplot,reform(dd[ip,*,it]),y2,color=colors.cyan 
dd=diffq_dn*1000      
oplot,reform(dd[ip,*,it]),y2,color=colors.brown
dd=diffq_up*1000      
oplot,reform(dd[ip,*,it]),y2,color=colors.brown,linest=2
oplot,y2*0,y2

varp = icase2+'_stend_ip=' + strtrim(ip,2)
window,/free,title=varp
dd=stend*86400/1004                                                        
plot,reform(dd[ip,*,it]),y2,yrange=[1000.,100],title=varp,xrange=[-5.,5]
dd=stendcond*86400/1004                                                    
oplot,reform(dd[ip,*,it]),y2,color=colors.red                              
dd=stendevap*86400/1004                      
oplot,reform(dd[ip,*,it]),y2,color=colors.blue
dd=stendtranup*86400/1004                     
oplot,reform(dd[ip,*,it]),y2,color=colors.green,linest=2
dd=stendtrandn*86400/1004                      
oplot,reform(dd[ip,*,it]),y2,color=colors.cyan 
dd=diffdse_dn/1004                         
oplot,reform(dd[ip,*,it]),y2,color=colors.brown
dd=diffdse_up/1004                         
oplot,reform(dd[ip,*,it]),y2,color=colors.brown,linest=2
oplot,y2*0,y2

varp =icase2 + '_normassflx_ent_det_ip=' + strtrim(ip,2)
window,/free,title=varp
dd=normassflx_up
plot,reform(dd[ip,*,it]),y2,yrange=[1000.,100],title=varp,xrange=[-1.,3]
dd=normassflx_dn
oplot,reform(dd[ip,*,it]),y2,color=colors.cyan 
dd=ent_rate*1000.
oplot,reform(dd[ip,*,it]),y2,color=colors.red                              
dd=ent_rate_sh*1000.
oplot,reform(dd[ip,*,it]),y2,color=colors.brown,linest=2
dd=det_rate*1000.
oplot,reform(dd[ip,*,it]),y2,color=colors.green
dd=det_rate_sh*1000.
oplot,reform(dd[ip,*,it]),y2,color=colors.blue,linest=2
dd=w_up_mid/10.
oplot,reform(dd[ip,*,it]),y,color=colors.red,thick=2
dd=radius_up/10000.
oplot,reform(dd[ip,*,it]),y,color=colors.red,thick=2,linest=2
dd=buoy_mid*10.
oplot,reform(dd[ip,*,it]),y,color=colors.pink,thick=2,linest=2
oplot,y2*0,y2


varp =icase2 + 'sum_massflx_tend='+ strtrim(ip,2) 
window,/free,title=varp
dd=massflxsum*1.e3
plot,reform(dd[0,*,it]),y2,yrange=[1000.,100],title=varp,xrange=[-10.,10]*2.0
dd=stendsum/1004.*86400
oplot,reform(dd[0,*,it]),y2,color=colors.red
dd=qtendsum*1000.*86400
oplot,reform(dd[0,*,it]),y2,color=colors.green

dd=massflxbase*1000.
print,'massflxbase',reform(dd[*,it])

varp =icase2 + 'normassflx_up=all'
window,/free,title=varp
dd=normassflx_up ;massflxsum*1.e3
plot,reform(dd[0,*,it]),y2,yrange=[1000.,100],title=varp,xrange=[0.,max(dd)/2.]
 for ip=1,nplume-1 do oplot,reform(dd[ip,*,it]),y2
 oplot,reform(dd[nplume-1,*,it]),y2,color=colors.red


end  ;'tend' 
else:

endcase

end


