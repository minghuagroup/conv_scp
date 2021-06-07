;after run era_3cm and 00_era_dump

; need gifolder


dlat=5.

NLVE = n_elements(pp)

COE = fltarr(NY,NLEV,4,3)  ; 4 coefficients land, ocean, all

for lat = 0,ny-1  do begin

; window,/free,title = 'Lat=:'+strdigit(yy[lat],0)
 kk=0
for K=5, n_elements(pp)-1,4 do begin
;===============================

;goto,jump_plot
; print,'lat plev',yy(lat),pp[k]

 DB1 = reform(CLOUD4[*,  lat,k])
 DB2 = reform(RH2[   *,  lat,k])
 DB3 = reform(NN2[   *,  lat,k])
 DB4 = reform(D2RH_DZ2[*,lat,k])
 DB5 = LANDFRAC2[   *,   lat]

     jj2 = where( (DB1 gt -9990)  and (DB2 gt -9990.) and $
                  (DB3 gt -9990.) and (DB4 gt -9990.) and $
                  (DB5 le 1000.1), cnt1)    ;ocean
     jj1 = where( (DB1 gt -9990)  and (DB2 gt -9990.) and $
                  (DB3 gt -9990.) and (DB4 gt -9990.) and $
                  (DB5 gt 0.1), cnt2)    ;land 

     if(cnt1 gt 2)then begin
        yyt = (DB1[jj2])
        xx2 = (DB2[jj2])
        xx3 = (DB3[jj2])
        xx4 = (DB4[jj2])
        MDB5 = (DB4[jj2])

        xx3 = 1.0*RANKS2(xx3,levxx3)/cnt1 
        xx4 = 1.0*RANKS2(xx4,levxx3)/cnt1 

        ;xx1 = (xx2-xxc)
; start from the simplest case
        
         zz1 = xx2*xx2
         zz2 = xx2*xx3
         zz3 = xx2*xx4
         zz4 = xx3*xx4 

         xxt = [transpose(xx2),transpose(xx3),transpose(xx4) ] ;$
;               transpose(zz1),transpose(zz2),transpose(zz3), transpose(zz4)]

         Result = regress(xxt,yyt,sigma=sigma,yfit = yfit,status=status,const=const)

       xxx = xx2 
       xxx = yfit
       if(kk eq 0)then $
         plot,xxx,yyt,psym=1,xrange=[0.2,1.2],yrange=[0.,1.0],xtitle='RH' $
       else oplot,xxx,yyt,color=colors.(k),psym=1
       kk=kk+1 

         print,format='(3A4,4F8.3)',strdigit(yy[lat],0),' ',strdigit(pp[k],0),reform(result),const

     endif ; bin data valid 

 endfor  ;lev
         read,ix
 endfor  ;lat


end


