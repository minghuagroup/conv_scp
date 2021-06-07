
igif=0
ixx=0

case_test = 'beta05p2k'
case_ctl = 'beta05ctl'

dir_test = '../diag/'+case_test+'/'
dir_cntl = '../diag/'+case_cntl+'/'

file_test = dir_test+case_test+'_ANN_climo.nc'
file_cntl = dir_cntl+case_cntl+'_ANN_climo.nc'

var='ANRAIN'

aa_test = get_fld(file_test,var)
aa_cntl = get_fld(file_cntl,var)

aa = 0.0*aa_test-aa_cntl

xx = get_fld(file_cntl,'lon')
yy = get_fld(file_cntl,'lat')

stop
  
;run 00_range
;get aa,xx,yy,lev1 

titlename = var
;var=casename+'_DCRF'

  lev2 = [2000.,4000]

  bb = aa
 londel = fix((lon_range[1] - lon_range[0])/12. )
 latdel = fix((lat_range[1] - lat_range[0])/6. )

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

 gifname= titlename+var
 title= gifname+value

 if(max(aa) ne min(aa)) then begin
  plot_map4,aa,bb,xx,yy,lev1,lev2,xrange=xrange,$
      yrange=yrange,londel=londel,latdel=latdel,isetz=1,$
          title=title,xtitle='longitude',ytitle=ytitle

   if(igif)then begin
       mygif,'gifs/'+gifname+'.gif'
   endif
  if(iix)then  read,ix

  endif


end
