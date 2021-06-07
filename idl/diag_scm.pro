


x = reform( get_fld(filein2,'time') )
p = reform( get_fld(filein2,'lev') )
q = reform( get_fld(filein2,'Q') )*1000.
p2 = replicate2(p,n_elements(q[0,*]))
q1 = reform( get_fld(filein2,'DTCOND') )*86400.
q2 = reform( get_fld(filein2,'DCQ') )*86400.
q3 = q1 + 2.5/1.004*q2*1000.
t = reform( get_fld(filein2,'T')   )    
Z3 = reform( get_fld(filein2,'Z3') )
mc2 = reform( get_fld(filein2,'CMFMC')) 
mc  = t*0
for i=0,29 do mc[i,*] = 0.5*(mc2[i+1,*]+mc2[i,*])

rho = P2/287./t*100.

hdiff = reform( get_fld(filein2,'TDIFF')+get_fld(filein2,'QDIFF' )*2.5*1000/1.004)

levdt = cal_lev([-10.,10],20)  

s = t+9.8/1004*Z3
s=reform(s)
q=reform(q)
h = s + 2.5/1.004*q
sa = ave2(s)
za = ave2(z3)
;plot,sa,y,yrange=[1000.,100]
;plot,ave2(Q),y,yrange=[1000.,100]
;plot,ave2(mc),y,yrange=[1000.,100]

jump1:
mcdsdz = s*0
mcdqdz = s*0
dmcdz = s*0
for k=1,28 do begin
a = s
dmcdz[k,*] = (mc[k+1,*]-mc[k-1,*])/(z3[k+1,*]-z3[k-1,*])
Mcdsdz[k,*] = mc[k,*]*(a[k+1,*]-a[k-1,*])/(z3[k+1,*]-z3[k-1,*])*86400.
a = q
Mcdqdz[k,*] = mc[k,*]*(a[k+1,*]-a[k-1,*])/(z3[k+1,*]-z3[k-1,*])*86400.
endfor
Mcdsdz = Mcdsdz/rho
Mcdqdz = Mcdqdz/rho
Mcdhdz = mcdsdz+2.5/1.004*mcdqdz ; -Det
det    = -dmcdz  ; 
deth = q3 - Mcdhdz  ;D(hc-hbar)
dhc = q3*0   ; hc-hbar
jj=where(det ge 1.0e-6)
dhc[jj] = deth[jj]/det[jj]/86400.


vars3 = ['CMFMC','DTCOND','MC_DSDZ','D_Sc_CE','DCQ','MC_DQDZ','D_Qc_CE',$
     'Q3','MC_DHDZ','D_Hc','H_DIFF']

FOR ii=0,n_elements(vars3)-1 DO BEGIN
var = vars3[ii]
CASE var of 
 'CMFMC': begin
   get_lev,aa,'CMFMC',levc,scale
   aa = mc*scale
           end
 'DTCOND': begin
   aa = q1
   get_lev,aa,var,levc,scale
           end

 'MC_DSDZ': begin
   aa = mcdsdz
   get_lev,aa,'DTCOND',levc,scale
           end

 'D_Sc_CE': begin
   aa = q1 - mcdsdz
   get_lev,aa,'DTCOND',levc,scale
           end

 'DCQ': begin
   aa = q2*1000.
   get_lev,aa,'DCQ',levc,scale
           end

 'MC_DQDZ': begin
   aa = mcdqdz
   get_lev,aa,'DCQ',levc,scale
           end

 'D_Qc_CE': begin
   aa = q2*1000. - mcdqdz
   get_lev,aa,'DCQ',levc,scale
           end

 'Q3': begin
   aa = q3
   get_lev,aa,'DTCOND',levc,scale
           end

 'MC_DHDZ': begin
   aa = mcdhdz
   get_lev,aa,'DTCOND',levc,scale
           end

 'D_Hc': begin
   aa = q3 - mcdhdz
   get_lev,aa,'DTCOND',levc,scale
           end

 'H_DIFF': begin
   aa = hdiff
   get_lev,aa,'TDIFF',levc,scale
           end
  else:
   endcase

  bb=aa
 lev1 = levc
 lev2 = lev1*100. +10000.
 title=strtrim(ii)+':'+var
 xrange=[0.,max(x)]
  window,/free,xsize=480,ysize=400,title=title
  plot_4dhtml,aa,bb,x,y,lev1,lev2,xrange=xrange,$
      yrange=yrange,xtitle='time',tran=1,$
      title= title+' min:'+strtrim(min(aa),2)+'  max:'+strtrim(max(aa),2)+ $
      ' scaled by '+strtrim(scale,2)
;stop

ENDFOR ; ii

stop

aa=mc & xrange=[-0.2,0.2]
aa = h & xrange=[330.,400]
plot,reform(aa[*,i]),y,yrange=[1000.,100],xrange=xrange
oplot,y*0,y,color=colors.red

i1=0
i2=130
for i=i1,i2 do  oplot,reform(mc[*,i]),y
 


end

