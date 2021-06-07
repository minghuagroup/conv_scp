

caseid = '647b5'
var    = 'PRECT'

; 1. =========================================

nann = 73 ;pentad
filein   = caseid+'_pentad_'+var+'.nc'

 ;2 .======================================  ;region and filein needs to match

 region = 'SGP'
 region = 'Tropical INDO Pacific'
 region = 'China'
 region = 'East Asia'
 print,var, ' ',region

 ;3.=========================================

file = '../obs/IAP/'+filein
;4 =========================================

 print,file
  dd1= indgen(10)+1.
  get_lev,dd1,var,lev1,scale

 if(strpos(filein,'diurnal') ge 0)then var=var+'_'+month

 dd1 = get_fld(file,var)   ;!!!!!!!!!!!!!! data
 dd1 = dd1*scale

;5 =========================================
  time_scale =1.
  if(strpos(filein,'pentad') ge 0)then time_scale = 5.0

tt = indgen(n_elements(time))*1.0 
xx = get_fld(file,'lon')
yy = get_fld(file,'lat')
time = get_fld(file,'time')
tt = indgen(n_elements(time))*1.0 
nx = n_elements(xx)
ny = n_elements(yy)
nt = n_elements(time)

;6 =========================================
; specify plot x,y,t ranges
 case region of
'SGP': begin  ; =====================
     yy3 = [35.,45]
     xx3 = [-110.,-85]
     tt3 = [0.,23]
       end
'China':begin 
     yy3 = [28.,35]
     xx3 = [90.,130.]
     tt3 = [0.,23]
        end
'East Asia': begin ;pentad
     xx3 = [115.,125.]
     yy3 = [20.,44]
     tt3 = [21.,54] ;[0.,72]
         end 
'Tropical INDO Pacific': begin ;pentad
     xx3 = [40.,180.]
     yy3 = [-15.,15]
     yth = 1
     t1 = (yth-1.)*nann
     t2 = t1+nann-1.
     tt3 = [t1,t2] ;[0.,3*72]

         end 
 else: begin
    print,'selection region!'
    stop
        end
 endcase

 tt3 = [0.,nt-1.]  ;MAXIMUM time length

; 6a ==  extract data and calc climatology & anomaly ==========

     jj1 = where(xx3 lt 0.,cnt)
     if(cnt gt 0)then xx3[jj1] = 360. + xx3[jj1]

     jjx = where( xx ge xx3[0] and xx le xx3[1],cntx)
     jjy = where( yy ge yy3[0] and yy le yy3[1],cnty)
     jjt = where( tt ge tt3[0] and tt le tt3[1], cntt)

     nxh = n_elements(jjx)
     nyh = n_elements(jjy)
      
     dd11 = fltarr(cntx,cnty,cntt)

     for k = 0,cntt-1 do begin
      for j = 0,cnty-1 do begin
        j1 = jjy[j]
        k1 = jjt[k]
        dj  = reform(dd1[*,j1,k1])
        dj2 = dj[jjx]
        dd11[*,j,k] = dj2
       endfor 
       endfor 

     mean_anom, dd11,nann, dd1m, dd1a, dd1p ;clim, anom, percentage
     ;!!!

;7 specify what field types to plot and lev1 ===========================

 fields = ['original','anomaly','normalized anomaly','climatology']
 for iv = 0,n_elements(fields)-1 do print,iv+1,': ',fields[iv]

 print,'which variable to plot?'
 read,iv
 iv=iv-1
 
 case iv of
 0: begin
      dd = dd11
      lev1 = cal_lev([0.,5],20)    
    end
 1: begin
      dd = dd1a
      lev1 = cal_lev([-5.,5],20)    
    end
 2: begin
      dd = dd1p
      if(strpos(filein,'diurnal') ge 0)then lev1 = cal_lev([-50.,50],20)
      if(strpos(filein,'pentad') ge 0)then lev1 = cal_lev([-100.,100],20)    
    end
 3: begin
      dd = dd1m
      lev1 = cal_lev([-5.,5],20)    
      tt = indgen(nann)  ;new time dimension
      tt3 = [min(tt),max(tt)]
      jjt = tt

     if(strpos(region, 'Pacific') ge 0)then lev1 = cal_lev([0.,10],20)
    end

 else:
 endcase


     print,'region size is:'
     print,'lat: ',yy3
     print,'lon: ',xx3
     print,'tt3: ',tt3, tt3/nann

      ddj = dd
 
; 8. == position array for homography =======================
     if(nxh gt nyh)then begin  ; longitude and time
       print,'longitude and time'

       dd = ave3(ddj,three=2)
       xx = xx[jjx]
       yy = tt[jjt]*time_scale
       yrange = [min(tt3),max(tt3)]*time_Scale
       xrange = xx3
       ytitle = 'Time'
       xtitle = 'Longitude'
       latdel = 1.
       londel = 5.

      endif else begin         ; latitude and time

       print,' latitude and time'
       dd = ave3(ddj,three=1)

       yy = yy[jjy]
       xx = tt[jjt]*time_scale
       dd = transpose(dd) 
       xtitle = 'time'
       ytitle = 'Latitude'
       xrange = [min(tt3),max(tt3)]*time_scale
       yrange = yy3
       londel = 1.
       latdel = 5.
      endelse

      print,'need to re-run region for other types of plots'
   
      aa = dd

; 9. === make plots ===
  
;run 00_range
;get aa,xx,yy,lev1 

titlename = caseid+'_'

  lev2 = [2000.,4000]

  bb = aa

;stop

; ny=n_elements(yy)  ; do weighted average
; aaw=aa*0
; bbw=aaw
; jj=where(aa gt -999.)
; aaw[jj] = aa[jj]
; bbw[jj] = 1.
; pi2=3.1416/180.
; for j=0,cnty-1 do begin
;  cosz = cos(yy[j]*pi2)
;  aaw[*,j] = cosz*aaw[*,j]
;  bbw[*,j] = cosz*bbw[*,j]
; endfor

 value = ' ('+strtrim(min(aa),2)+', '+strtrim(max(aa),2)+')'
; value = ' ('+strtrim(min(aa[jj]),2)+', '+strtrim(max(aa[jj]),2)+', '$
;    +strtrim(mean(aaw)/mean(bbw),2)+')'

 gifname= titlename+var+'_'+fields[iv]+'_'+month
 title= gifname+value


 if(max(aa) ne min(aa)) then begin
 window,/free,xsize=600,ysize=460,title=title
  plot_4dhtml,aa,bb,xx,yy,lev1,lev2,xrange=xrange,$
      yrange=yrange, title=title,xtitle=xtitle,ytitle=ytitle

   if(igif)then begin
;       mygif,'gifs/'+gifname+'.gif'
   endif
;  if(iix)then  read,ix

  endif

print,min(aa),max(aa)
; print,'More plots (1: variable;  2: months)?'
 ivv = 1
; read,ivv

end




