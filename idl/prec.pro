igif=0
iix = 1
ix=0
files=['test2.cam.h0.1978-11.nc', $
       'test2.cam.h1.1978-11-01-00000.nc',$
       'test2.cam.h2.1978-11-01-00000.nc',$
       'test2.cam.h3.1978-11-01-00000.nc']


for nf=3,3 do begin  ;for precip

 filein = 'data/'+files[nf]
 print,''
 print,filein 
 ncdf_vars,filein,vars

 x=get_fld(filein,'lon')
 y=get_fld(filein,'lat')
 lev=get_fld(filein,'lev')
 time = get_fld(filein,'time')    ;
 nt=n_elements(Time) 
read,ix
endfor



 PRECT=get_fld(filein,'PRECT')
 PRECDP=get_fld(filein,'PRECDP')
 PRECSH=get_fld(filein,'PRECSH')
 help,prect
 prect= prect*3600.*1000. ; m/s to mm/hr
 precdp= precdp*3600.*1000. ; m/s to mm/hr
 precsh= precsh*3600.*1000. ; m/s to mm/hr
 precl = prect-precdp-precsh

if(ix lt 0)then begin
 TMQ = get_fld(filein,'TMQ')
 CLDTOT = get_fld(filein,'CLDTOT')
 CLDHGH = get_fld(filein,'CLDHGH')
 CLDMED = get_fld(filein,'CLDMED')
 CLDLOW = get_fld(filein,'CLDLOW')
 LWCF = get_fld(filein,'LWCF')
 SWCF = get_fld(filein,'SWCF')
 CRF=LWCF+SWCF
 TGCLDLWP = get_fld(filein,'TGCLDLWP')
 TGCLDIWP = get_fld(filein,'TGCLDIWP')
 TS = get_fld(filein,'TS')-273.16
 PS = get_fld(filein,'PS')/100.
 QREFHT = get_fld(filein,'QREFHT')*1000.
endif

 lev_crf= cal_lev([-400,200],20)
 lev_lwcf= cal_lev([0.,100],20)
 lev_swcf= cal_lev([-300.,0],20)
 lev_cld = cal_lev([0.,1],20)
 lev_prect= cal_lev([0.,10],20) 
 lev_precc= cal_lev([0.,10],20) 
 lev_precl= cal_lev([0.,10],20) 
 lev_tmq = cal_lev([10.,50],20)  
 lev_totcldlwp = cal_lev([0,1.],20)  
 lev_ts= cal_lev([12,32],20)
 lev_ps= cal_lev([800.,1020.],20)
 lev_qrefht= cal_lev([0.,20.],20)

; print,time[0],time[1],(time[1]-time[0])*24
 ix=0


 ii=10
 if(ii eq 0)then begin
 xrange=[254.,268.]
 yrange=[33.,39]
 londel=1.
 latdel=1.
 siz='Small'
 endif else begin
 xrange=[240.,285.]
 yrange=[25.,50]
 londel=10.
 latdel=5. 
 siz = 'Large'
 endelse

 xrange=[0.,360]
 yrange= [-90.,90]
 londel = 30.
 latdel = 30.
 siz='global'
 for it=0,nt-1 do begin
;=========
 title= 'PRECT at '+ strtrim(time[it],2)

;--------
 aa=reform(prect[*,*,it])
 
 jj =where(aa ge 0.0,cnt)
 print,'mean_model',total(aa[jj])/cnt

 var='prect'
 lev1 = lev_prect/5
 lev2 = lev1*4
; jj=where(aa le 1.0e-3,cnt) & if(cnt gt 0)then aa[jj]=lev1[0]-1.
 bb=aa
 plot_map4,aa,bb,x,y,lev1,lev2,xrange=xrange,$
      yrange=yrange,londel=londel,latdel=latdel,isetz=1,$
          title=title,xtitle='longitude',ytitle=ytitle
 gifname= var+'_'+siz+'.gif'
 if(igif)then mygif,'gif_maps/'+gifname
 if(iix)then read,ix
;--------

 goto,jump1

 aa=reform(precc[*,*,it])
 var='precdp'
 lev1 = lev_precc
 lev2 = lev1*2
 title=model+': '+xtitle+' '+var+' (mm/hr)'
 jj=where(aa le 1.0e-3,cnt) & if(cnt gt 0)then aa[jj]=lev1[0]-1.
 bb=aa
 plot_map4,aa,bb,x,y,lev1,lev2,xrange=xrange,$
      yrange=yrange,londel=londel,latdel=latdel,isetz=1,$
          title=title,xtitle='longitude',ytitle=ytitle
 gifname= xtitle+'_'+var+'_'+siz+'.gif'
 if(igif)then mygif,'gif_maps/'+gifname
 if(iix)then read,ix

;--------
 aa=reform(precsh[*,*,it])
 var='precsh'
 lev1 = lev_precl
 lev2 = lev1*2
 title=model+': '+xtitle+' '+var+' (mm/hr)'
 jj=where(aa le 1.0e-3,cnt) & if(cnt gt 0)then aa[jj]=lev1[0]-1.
 bb=aa
 plot_map4,aa,bb,x,y,lev1,lev2,xrange=xrange,$
      yrange=yrange,londel=londel,latdel=latdel,isetz=1,$
          title=title,xtitle='longitude',ytitle=ytitle
 gifname= xtitle+'_'+var+'_'+siz+'.gif'
 if(igif)then mygif,'gif_maps/'+gifname
 if(iix)then read,ix

jump1:

endfor ; it

end
