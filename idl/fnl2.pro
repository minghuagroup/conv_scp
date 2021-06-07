igif=0
iix = 1
ix=0
icoarse=1

;goto,jump1
 filein = '../obs/FNL/'+'fnl_20170727_00_00.nc'
 print,''
 print,filein 
 ncdf_vars,filein,vars
 x=get_fld(filein,'longitude')
 y=get_fld(filein,'latitude')

 varlev =['HGT','TMP','RH','VVEL','UGRD','VGRD','ABSV','CLWMR']
 print,varlev

 plev  =[indgen(18)*50+100,1000]
 vars1=strarr(8,19)
 for iv=0,7 do for ip=0,18 do $
 vars1[iv,ip] = varlev[iv]+'_'+strtrim(plev[ip],2)+'mb'

 vars2a =['PRES_surface','HGT_surface TMP_surface',    $
   'PWAT_entireatmosphere_consideredasasinglelayer_', $
   'CWAT_entireatmosphere_consideredasasinglelayer_','MSLET_meansealevel']

 vars2 =['Ps','Ts','PREW','TGCLW','SLP']

 PRECT=get_fld(filein,vars2a[2])  ;prew

 prect= prect ; /24.*10 ; m/s to mm/hr  
; =============!!!!?? verified against the globe animation
 

 lev_crf= cal_lev([-400,200],20)
 lev_lwcf= cal_lev([0.,100],20)
 lev_swcf= cal_lev([-300.,0],20)
 lev_cldw = cal_lev([0.,1],20)
 lev_cld = cal_lev([0.,1],20)
 lev_prect= cal_lev([0.,10],20) 
 lev_precc= cal_lev([0.,10],20) 
 lev_precl= cal_lev([0.,10],20) 
 lev_tmq = cal_lev([0.,70],14)  
 lev_totcldlwp = cal_lev([0,1.],20)  
 lev_ts= cal_lev([12,32],20)
 lev_ps= cal_lev([800.,1020.],20)
 lev_qrefht= cal_lev([0.,20.],20)

; print,time[0],time[1],(time[1]-time[0])*24
 ix=0

 xrange=[-180.,180]
 yrange= [-90.,90]
 londel = 30.
 latdel = 30.
 siz='global'
;=========
 title= 'PREW' ;+ strtrim(time[it],2)

;--------
 aa= prect
 jj =where(aa ge 0.0,cnt)
 print,'mean3',total(aa[jj])/cnt

 var='prew'
 lev1 = lev_tmq
 lev2 = [ 1000.,2000] ;lev1*100
; jj=where(aa le 1.0e-1,cnt) & if(cnt gt 0)then aa[jj]=lev1[0]-1.
 bb=aa
 plot_map4,aa,bb,x,y,lev1,lev2,xrange=xrange,$
      yrange=yrange,londel=londel,latdel=latdel,isetz=1,$
          title=title,xtitle='longitude',ytitle=ytitle
 gifname= var+'_'+siz+'.gif'
 if(igif)then mygif,'gif_maps/'+gifname

;--------
print,'layered?'
read,ix

jump1:

vv=[2,7]

iv=2
for k=0,18,2 do begin
;--------
 if(iv eq 2)then aa= get_fld(filein,vars1[2,k])/100  ;RH
 if(iv eq 7)then aa= get_fld(filein,vars1[7,k])*1000  ;CLWR

 title = vars1[iv,k]
 jj =where(aa ge 0.0,cnt)
 print,'mean_model',total(aa[jj])/cnt,min(aa),max(aa)
 lev1 = lev_cld
 lev2 = [1000.,2000]; lev1
 jj=where(aa le 1.0e-3,cnt) & if(cnt gt 0)then aa[jj]=lev1[0]-1.
 bb=aa
 plot_map4,aa,bb,x,y,lev1,lev2,xrange=xrange,$
      yrange=yrange,londel=londel,latdel=latdel,isetz=1,$
          title=title,xtitle='longitude',ytitle=ytitle
 gifname= var+'_'+siz+'.gif'
 if(igif)then mygif,'gif_maps/'+gifname
 read,ix

endfor ;level
;--------



print,'globe?'
 if(iix)then read,ix

 lon0 = [0.,90.,180.,-90]
 for i=0,3 do begin
   window,/free
   plot_map4g,aa, -90.,90., 0.,360, 0, lon0[i]
 endfor

end
