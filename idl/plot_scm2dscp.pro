
 print,''
 print,' run plot_scm_head.pro first!'
 print,'input file:     ',filein2
 print,'gif_folder:',gif_folder
 print,'icase     :',icase
 print, 'Ready?'

 ix=1
 read,ix


 ncdf_vars,filein2,vars2
 x = get_fld(filein2,'time')
 y = get_fld(filein2,'lev')
; xrange=[min(x),max(x)]

 y2 = get_fld(filein2,'ilev')
; y2 = get_fld(filein2,'LEV')
 ny2 = n_elements(y2)

 for iv = 24,n_elements(vars2)-1 do begin
;================
 y = get_fld(filein2,'lev')
  var = vars2[iv]
  title   = icase+'_'+var
  aa = get_fld(filein2,var)
  aa = reform(aa)
  siz = size(aa)
  siz0 = siz[0]
  if(siz0 ne 2)then goto,jump_var

  get_lev,aa,var,levc,scale
;------------------------
  aa   = scale*aa
  data_range = [min(levc),max(levc)]
  gif_file= gif_folder+'/2_'+title+'.gif'
  lev1 = levc
  bb = aa
  lev2 = levc
  lev2 = [10000.,20000]

  y= get_fld(filein2,'lev')
  if(n_elements(aa[*,0]) eq ny2) then y=y2 ; get_fld(filein2,'LEV')  
 
  jj=where(aa eq 0,cnt) & if(cnt gt 0)then aa[jj] = lev1[0]-1
    if(min(aa) ne max(aa))then $
   plot_4dhtml,aa,bb,x,y,lev1,lev2,xrange=xrange,$
      yrange=yrange,xtitle='time',tran=1,$
      title= title+' min:'+strtrim(min(aa),2)+'  max:'+strtrim(max(aa),2)+ $
      ' scaled by '+strtrim(scale,2)

 if(igif)then mygif,gif_file
 if(iix)then read,ix

  print,var, ' ', gif_file

 jump_var:
 endfor ; variable
;==================

jump2:

print, 'f_folder:',gif_folder

end
