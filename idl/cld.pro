igif=0
iix = 1
ix=0
files=['test2.cam.h0.1978-11.nc', $
       'test2.cam.h1.1978-11-01-00000.nc',$
       'test2.cam.h2.1978-11-01-00000.nc',$
       'test2.cam.h3.1978-11-01-00000.nc']


for nf=0,0 do begin  ;for precip

 filein = 'data/'+files[nf]
 print,''
 print,filein 
 ncdf_vars,filein,vars

 x=get_fld(filein,'lon')
 y=get_fld(filein,'lat')
 lev=get_fld(filein,'lev')
; time = get_fld(filein,'time')    ;
; nt=n_elements(Time) 
read,ix
endfor

hyam = get_fld(filein,'hyam')
hybm = get_fld(filein,'hybm')
P0 = 100000.
PS = get_fld(filein,'PS')
PSL= get_fld(filein,'PSL')
Z3 = get_fld(filein,'Z3')

 CLOUD =get_fld(filein,'CLOUD')

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


 title= 'CLOUD '

;--------
 titles = ['cldtot','cldlow','cldmed','cldhgh']
 for id=0,3 do begin
  case id of
   0: aa=reform(cldtot)
   1: aa=reform(cldlow)
   2: aa=reform(cldmed)
   3: aa=reform(cldhgh)
   else:
  endcase
 title=titles[id]

 jj =where(aa ge 0.0,cnt)
 print,'mean_model',total(aa[jj])/cnt
 lev1 = lev_cld
 lev2 = lev1*4
; jj=where(aa le 1.0e-3,cnt) & if(cnt gt 0)then aa[jj]=lev1[0]-1.
 bb=aa
 plot_map4,aa,bb,x,y,lev1,lev2,xrange=xrange,$
      yrange=yrange,londel=londel,latdel=latdel,isetz=1,$
          title=title,xtitle='longitude',ytitle=ytitle
 gifname= var+'_'+siz+'.gif'
 if(igif)then mygif,'gif_maps/'+gifname

 read,ix
endfor  ; 4 types


 print,'do layered clouds?'
 if(iix)then read,ix
;--------
 
 for k=10,29 do begin  

;--------
 aa=reform(cloud[*,*,k])
 lev1 = lev_prect/5
 title = 'cld at k='+strtrim(k,2)+' lev= '+strtrim(lev[k])
 jj =where(aa ge 0.0,cnt)
 print,'mean_model',total(aa[jj])/cnt
 lev1 = lev_cld
 lev2 = lev1*4
; jj=where(aa le 1.0e-3,cnt) & if(cnt gt 0)then aa[jj]=lev1[0]-1.
 bb=aa
 plot_map4,aa,bb,x,y,lev1,lev2,xrange=xrange,$
      yrange=yrange,londel=londel,latdel=latdel,isetz=1,$
          title=title,xtitle='longitude',ytitle=ytitle
 gifname= var+'_'+siz+'.gif'
 if(igif)then mygif,'gif_maps/'+gifname
 read,ix
  
endfor ;level
jump1:




end
