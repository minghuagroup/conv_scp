;igif=1
;this 


var='CLOUD'
 print,'file=',file
 print,'for lev purpose var=?'
; read,var

 help,aa,x,y,var

; ncdf_vars,filein2,vars2
; x = get_fld(filein2,'time')
; y = get_fld(filein2,'lev')
; xrange=[min(x),max(x)]

; y2 = get_fld(filein2,'ilev')
; y2 = get_fld(filein2,'LEV')
; ny2 = n_elements(y2)

;for iv = 37,n_elements(vars2)-1 do begin
; for iv = 210,240 do begin
;================
title=file+'_'+var
window,/free,xsize=600,ysize=460,title = title

; y = get_fld(filein2,'lev')
;  var = vars2[iv]
;  title   = caseiap+'_'+var
;  aa = get_fld(filein2,var)
  aa = reform(aa)
  siz = size(aa)
  siz0 = siz[0]

  get_lev,aa,var,levc,scale
  aa   = scale*aa
  data_range = [min(levc),max(levc)]
;------------------------
if(siz0 eq 1 )then begin
  gif_file= gif_folder+'/1_'+title+'.gif'

   plot,x,aa,xtitle='time',ytitle=var,xrange=xrange,$
     title = title+' min:'+strtrim(min(aa),2)+'  max:'+strtrim(max(aa),2) + $
       ' scaled by '+strtrim(scale,2),$
    max_v = data_range[1],min_v=data_Range[0]


  endif else begin 
   if(siz0 eq  2)then begin

;; if(var eq 'TDIFF')then stop
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
  endif
 endelse

; if(igif)then begin
;  mygif,gif_file
;  wdelete
;endif 
;if(iix)then read,ix

;  print,var, ' ', gif_file

; endfor ; variable
;==================

;dir ='/Users/minghuazhang/unix/workgcm/'
;if(igif)then begin
; htmlfile = 'html/'+caseiap+'.html'
; gifgroups = file_search(dir+gif_folder+'/*.gif')
; web_view,gifgroups,htmlfile,title=caseiap,ncolumns=2
; print,'webfile:',htmlfile
;endif

end
