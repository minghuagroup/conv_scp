
 print,''
 print,' run plot_scm_headi.pro first! to plot scm diagnostic fields scmdiag-output.nc'
 print,'input file:     ',filein2
 print,'gif_folder:',gif_folder
 print,'icase     :',icase
 print, 'Ready for time ix?'

 read,ix



 ncdf_vars,filein2,vars2


 for iv = 1,n_elements(vars2)-1 do begin
;================
  var = vars2[iv]
  title   = icase2+'_'+var  ;+'_'+strtrim(it,2)
  aa = get_fld(filein2,var)
  aa = reform(aa)
  siz = size(aa)
  siz0 = siz[0]
  if(siz0 eq 2 )then begin

   print,iv, ' ',var,'  ',siz

 jj=where(abs(aa) gt 1.0e10,cnt)
 if(cnt gt 0)then aa[jj]=0
  get_levi,aa,var,levc,scale

; levc = cal_lev([min(aa),max(aa)],20)
; if(min(aa) eq max(aa))then levc = cal_lev([min(aa),max(aa)+20],20)
; 
; scale=1

;------------------------
  aa   = scale*aa
  data_range = [min(levc),max(levc)]

  gif_file= gif_folder+'/1_'+title+'.gif'

  lev1 = levc
  bb = aa
  lev2 = levc
  lev2 = [10000.,20000]

  x = xp
  y = yp

xrange=[min(x),max(x)]
yrange=[min(y)-1,max(y)+1]

;window,/free

   plot_4dhtml,aa,bb,x,y,lev1,lev2,xrange=xrange,$
      yrange=yrange,xtitle='time',ytitle='plume number',tran=1,$
      title= title+' min:'+strtrim(min(aa),2)+'  max:'+strtrim(max(aa),2)+ $
      ' scaled by '+strtrim(scale,2)

 if(igif)then mygif,gif_file
 if(iix)then read,ix


  print,var, ' ', gif_file
 endif ; do the plot 

 erase  ;!!!
 endfor ; variable
;==================

print, 'f_folder:',gif_folder

end
