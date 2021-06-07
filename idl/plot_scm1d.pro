
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

 for iv = 24,n_elements(vars2)-1 do begin
;================
  var = vars2[iv]
  title   = icase2+'_'+var
  aa = get_fld(filein2,var)
  aa = reform(aa)
  siz = size(aa)
  siz0 = siz[0]

   if(iix)then print,iv, ' ',var,'  ',siz
  if(siz0 eq 1 )then begin

  get_lev,aa,var,levc,scale
;------------------------
  aa   = scale*aa
  data_range = [min(levc),max(levc)]

  gif_file= gif_folder+'/1_'+title+'.gif'

;window,/free

   plot,x,aa,xtitle='time',ytitle=var,xrange=xrange,$
     title = title+' min:'+strtrim(min(aa),2)+'  max:'+strtrim(max(aa),2) + $
       ' scaled by '+strtrim(scale,2),$
     max_v = data_range[1],min_v=data_Range[0]


 if(igif)then mygif,gif_file
 if(iix)then read,ix


  print,var, ' ', gif_file
 endif ; do the plot 

 erase  ;!!!
 endfor ; variable
;==================

print, 'f_folder:',gif_folder

end
