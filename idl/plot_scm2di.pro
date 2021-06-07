
 print,''
 print,' run plot_scm_headi.pro first! to plot scm diagnostic fields scmdiag-output.nc'
 print,' run plot_scm_head.pro first!'
 print,'input file:     ',filein2
 print,'gif_folder:',gif_folder
 print,'icase     :',icase
 print, 'Ready?, which time mark?'

 ix=1
 read,ix



 ncdf_vars,filein2,vars2

 for iv = 1,n_elements(vars2)-1 do begin
;================
  var = vars2[iv]
  title   = icase2+'_'+var;  +'_'+strtrim(it,2)
  aa = get_fld(filein2,var)
  aa = reform(aa)
  siz = size(aa)
  siz0 = siz[0]
;  if(siz0 ne 3)then goto,jump_var
  if(siz0 ne 3)then goto,jump_var
 aa = reform(aa[*,*,it])
 aa = transpose(aa) 

; print,var
; if(var eq 'CLOUD')then stop
 y = y1
 if(n_elements(aa[*,0]) eq 31)then y=y2
 x = yp

 jj=where(aa gt 1.0e10,cnt)
  if(cnt gt 0)then aa[jj]=0

  get_levi,aa,var,levc,scale
;------------------------
  aa   = scale*aa

  
;  if(max( abs(aa)) gt 9000.)then aa = aa/1000.
;  levc = cal_lev([min(aa),max(aa)],20)
;  if(min(aa) eq max(aa))then levc = cal_lev([min(aa),max(aa)+20],20)


  data_range = [min(levc),max(levc)]
  gif_file= gif_folder+'/2_'+title+'.gif'
  lev1 = levc
  bb = aa
  lev2 = levc
  lev2 = [10000.,20000]

xrange=[min(x),max(x)]
yrange=[1000.,100]

;if(var eq 'radius_up')then stop

 
;;  jj=where(aa eq 0,cnt) & if(cnt gt 0)then aa[jj] = lev1[0]-1

;    if(min(aa) ne max(aa))then begin
   plot_4dhtml,aa,bb,x,y,lev1,lev2,xrange=xrange,$
      yrange=yrange,xtitle='time',tran=1,$
      title= title+' min:'+strtrim(min(aa),2)+'  max:'+strtrim(max(aa),2)+ $
      ' scaled by '+strtrim(scale,2)
;stop

 if(igif)then mygif,gif_file
 if(iix)then read,ix

  print,var, ' ', gif_file

 jump_var:
 endfor ; variable
;==================

jump2:

print, 'f_folder:',gif_folder

end
