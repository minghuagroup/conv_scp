print,'first run region.pro to specify. A company program is iap_homo to do cross section'

caseid = '647b5' ;d4'
var    = 'PRECT'
filein   = caseid+'_'+var+'_diurnal.nc'
;==========================================

file = '../obs/IAP/'+filein

 fields = ['mean','amplitude','percentage_amplitude','phase']
ivv = 0
jump_im:  ;to redo
print,'jan(1) or jul(7)?
month='1'
read,month

print,file
print,var+'_'+month

xx = get_fld(file,'lon')
yy = get_fld(file,'lat')
time = get_fld(file,'time')
nx = n_elements(xx)
ny = n_elements(yy)


dd1 = get_fld(file,var+'_'+month)
get_lev,dd1,'PRECT',levj,scale
dd1 = dd1*scale ;mm/day

dd1m = ave3(dd1,three=3)
anom1 = dd1*0.

for ih = 0,23 do begin
 anom1[*,*,ih] = dd1[*,*,ih] - dd1m[*,*]
endfor


; Calculate diurnal cycle
dd = dd1

ddm = fltarr(nx,ny)  ;mean
ddamp = ddm          ;amplitude
ddampp = ddm         ;amp in percentage
ddph = ddm           ;phase
;------------------------------------------------------
for j=0,ny-1 do begin
for i=0,nx-1 do begin
 dj = reform(dd[i,j,*])
 diurnal,dj,amm,amp,ampp,phase,xx(i),nw=2 ;one harmonic  
                                   ; convert to local time and do amplitude
                                   ; & percentage amp ampp
 ddm[i,j]   = amm
 ddamp[i,j] = amp
 ddampp[i,j]= ampp
 ddph[i,j]  = phase
endfor 
endfor 
;------------------------------------------------------

 jump_iv:

; help,ddm,ddamp,ddampp,ddph
 for iv = 0,n_elements(fields)-1 do print,iv+1,': ',fields[iv]

 print,'which variable to plot?'
 read,iv
 iv=iv-1
 
 case iv of
 0: begin
      dd = ddm
      lev1 = cal_lev([0.,10],20)    
      
    end
 1: begin
      dd = ddamp
      lev1 = cal_lev([0.,2],20)    
    end
 2: begin
      dd = ddampp
      lev1 = cal_lev([0.,60],20)    
    end
 3: begin
      dd = ddph
      jj = where(ddampp le 10.0,cnt)
      if(cnt gt 0)then dd[jj] = -1.
      lev1 = cal_lev([0.,24],24)    
    end
 else:
 endcase
 
 aa = dd


; the followed code is copied from plot_2d

  
;run 00_range
;get aa,xx,yy,lev1 

titlename = caseid+'_'

  lev2 = [2000.,4000]

  bb = aa

 ny=n_elements(yy)  ; do weighted average
 aaw=aa*0
 bbw=aaw
 jj=where(aa gt -999.)
 aaw[jj] = aa[jj]
 bbw[jj] = 1.
 pi2=3.1416/180.
 for j=0,ny-1 do begin
  cosz = cos(yy[j]*pi2)
  aaw[*,j] = cosz*aaw[*,j]
  bbw[*,j] = cosz*bbw[*,j]
 endfor

 value = ' ('+strtrim(min(aa[jj]),2)+', '+strtrim(max(aa[jj]),2)+', '$
    +strtrim(mean(aaw)/mean(bbw),2)+')'

 gifname= titlename+var+'_'+fields[iv]+'_'+month
 title= gifname+value

 if(max(aa) ne min(aa)) then begin
 window,/free,xsize=600,ysize=460,title=title
  plot_map4,aa,bb,xx,yy,lev1,lev2,xrange=xrange,$
      yrange=yrange,londel=londel,latdel=latdel,isetz=1,$
          title=title,xtitle=xtitle,ytitle=ytitle

   if(igif)then begin
;       mygif,'gifs/'+gifname+'.gif'
   endif
;  if(iix)then  read,ix

  endif

 print,'More plots (1: variable;  2: months)?'
 ivv = 1
 read,ivv
 if(ivv eq 1)then goto, jump_iv
 if(ivv eq 2)then goto, jump_im

end




