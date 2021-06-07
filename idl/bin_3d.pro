;after run era_3cm and 00_era_dump

; need gifolder


 k=35

for lat = 30,90,30 do begin
 jjlat = where(abs(yy) lt lat and (abs(yy) ge lat-30.))
for K=5,40,4 do begin   ; vertical levels
;===============================

;goto,jump_plot
 print,'lat plev',lat,pp[k]

 DB1 = reform(CLOUD4[*,  jjlat,k])
 DB2 = reform(RH2[   *,  jjlat,k])
 DB3 = reform(NN2[   *,  jjlat,k])*1.0e5
 DB4 = reform(D2RH_DZ2[*,jjlat,k])*1000.
 DB5 = LANDFRAC2[   *,   jjlat]

 nbin1 = 21  ;CLD
 nbin2 = 25  ;RH
 nbin3 = 5   ;NN
 nbin4 = 5   ;d2RH/dz
 nbin5 = 2   ;LND
 
 Bin1 = indgen(nbin1)*0.05
 Bin2 = indgen(nbin2)*0.025+0.4
 Bin3 = indgen(nbin3)*2 ; +5
 Bin4 = indgen(nbin4)*10-20.
 Bin5 = indgen(nbin5)*2-0.5

 dbin1 = bin1[1]-bin1[0]
 dbin2 = bin2[1]-bin2[0]
 dbin3 = bin3[1]-bin3[0]
 dbin4 = bin4[1]-bin4[0]
 dbin5 = bin5[1]-bin5[0]

 bin1[0] = bin1[0]-100*dbin1  & bin1[nbin1-1] = bin1[nbin1-1]+100*dbin1
 bin2[0] = bin2[0]-100*dbin2  & bin2[nbin2-1] = bin2[nbin2-1]+100*dbin2
 bin3[0] = bin3[0]-100*dbin3  & bin3[nbin3-1] = bin3[nbin3-1]+100*dbin3
 bin4[0] = bin4[0]-100*dbin4  & bin4[nbin4-1] = bin4[nbin4-1]+100*dbin4
 bin5[0] = bin5[0]-100*dbin5  & bin5[nbin5-1] = bin5[nbin5-1]+100*dbin5

 MDB1 = fltarr(nbin1,nbin2,nbin3,nbin4,nbin5) - 9999.
 MDB2 = fltarr(nbin1,nbin2,nbin3,nbin4,nbin5) - 9999.
 MDB3 = fltarr(nbin1,nbin2,nbin3,nbin4,nbin5) - 9999.
 MDB4 = fltarr(nbin1,nbin2,nbin3,nbin4,nbin5) - 9999.
 MDB5 = fltarr(nbin1,nbin2,nbin3,nbin4,nbin5) - 9999.
 NHIST = MDB1

 for i1 = 0,nbin1-2 do begin
 for i2 = 0,nbin2-2 do begin
 for i3 = 0,nbin3-2 do begin
 for i4 = 0,nbin4-2 do begin
 for i5 = 0,nbin5-2 do begin

  jj=where ( (DB1 gt Bin1[i1]) and (DB1 le Bin1[i1+1]) and $
             (DB2 gt Bin2[i2]) and (DB2 le Bin2[i2+1]) and $
             (DB3 gt Bin3[i3]) and (DB3 le Bin3[i3+1]) and $
             (DB4 gt Bin4[i4]) and (DB4 le Bin4[i4+1]) and $
             (DB5 gt Bin5[i5]) and (DB5 le Bin5[i5+1]),cnt)

  if(cnt gt 0)then begin
     WDB1 = DB1[jj]
     WDB2 = DB2[jj]
     WDB3 = DB3[jj]
     WDB4 = DB4[jj]
     WDB5 = DB5[jj]

     jj2 = where( (WDB1 gt -9990)  and (WDB2 gt -9990.) and $
                  (WDB3 gt -9990.) and (WDB4 gt -9990.) and $
                  (WDB5 gt -9990.), cnt2)    

     if(cnt2 gt 0)then begin
        MDB1[i1,i2,i3,i4,i5] = mean(WDB1[jj2])
        MDB2[i1,i2,i3,i4,i5] = mean(WDB2[jj2])
        MDB3[i1,i2,i3,i4,i5] = mean(WDB3[jj2])
        MDB4[i1,i2,i3,i4,i5] = mean(WDB4[jj2])
        MDB5[i1,i2,i3,i4,i5] = mean(WDB4[jj2])
        NHIST[i1,i2,i3,i4,i5]= cnt2
     endif ; bin data valid 
   endif   ; bin data exist
 endfor
 endfor
 endfor
 endfor
 endfor
 


 NHIST = 1.0*NHIST/max(NHIST)  ; normalization

 xm = bin2[1]+(indgen(nbin2)-0.5)*dbin2 
 ym = bin1[1]+(indgen(nbin1)-0.5)*dbin1 


for i3=0,nbin3-2 do begin
for i4=0,nbin4-2 do begin
for i5=0,nbin5-2 do begin

 print,''
 print,'i3,i4,i5',i3,i4,i5
 print,'        ',bin3[i3],bin4[i4],bin5[i5]
 
 aa = reform(NHIST[*,*,i3,i4,i5])
 lev1 = cal_lev([0,0.6],20)
if(max(aa) ne min(aa))then begin
 
aa = transpose(aa)

 xtitle = 'RH'
 ytitle = 'CLD'
 plot_4dhtml,aa,aa,xm,ym,lev1,lev1,xtitle = xtitle,ytitle=ytitle,xrange=[min(xm),max(xm)]

 mygif,gifolder+'/'+xtitle+'_'+ytitle+'_Lat'+ strdigit(lat,0)+'_'+  $
     strdigit(pp[k],0)+'mb_NN'+$
     strdigit(i3,0)+'_2RH'+strdigit(i4,0)+'_LND'+strdigit(i5,0)+'.gif'

 ix=1
; read,ix
endif
endfor
endfor
endfor
;============
jump_plot:

 strid = gifolder+'_hist_cld_Lat'+ strdigit(lat,0)+'_'+strdigit(pp[k],0)+'mb'
 gifgroups = file_search(gifolder+'/'+xtitle+'_'+ytitle+'*Lat'+ strdigit(lat,0)+'_'+$
     strdigit(pp[k],0)+'mb*.gif')

 htmlfile ='html_hist/'+strid+'.html'
 dir='../'
 gifgroups = dir+gifgroups
 web_view,gifgroups,htmlfile,title=strid,ncolumns=2
 print,'webfile:',htmlfile

endfor ; K
endfor ; lat


end


