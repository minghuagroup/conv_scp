
print,' get aa, var, xx, yy ready!'
help,aa,var,xx,yy

gif_folder=''

 xrange = minmax(xx)
 yrange = reverse(minmax(yy))
 xtitle = 'time'
 ytitle = 'pressure'
 title=var

window,/free,xsize=600,ysize=460,title = title

  aa = reform(aa)
  siz = size(aa)
  siz0 = siz[0]

  get_lev,aa,var,lev1,scale
;--------------------------- lev1 overwrite here 
;  lev1 = cal_lev([-1.,1.],20)
;--------------------------- lev1 overwrite here 

  aa   = scale*aa
  data_range = minmax(lev1)
;------------------------
if(siz0 eq 1 )then begin
  gif_file= gif_folder+'/1_'+title+'.gif'

   plot,x,aa,xtitle='time',ytitle=var,xrange=xrange,$
     title = title+' min:'+strtrim(min(aa),2)+'  max:'+strtrim(max(aa),2) + $
       ' scaled by '+strtrim(scale,2),$
    max_v = data_range[1],min_v=data_Range[0]


  endif else begin 
   if(siz0 eq  2)then begin

  bb = aa
  lev2 = lev1
  lev2 = [10000.,20000]

  title2=title+' min:'+strtrim(min(aa),2)+'  max:'+strtrim(max(aa),2)+ $
      ' scaled by '+strtrim(scale,2)
  jj=where(aa eq 0,cnt) & if(cnt gt 0)then aa[jj] = lev1[0]-1
    if(min(aa) ne max(aa))then $
   plot_4dhtml,aa,bb,xx,yy,lev1,lev2,xrange=xrange,$
      yrange=yrange,xtitle='time',tran=1,$
      title= title2
  endif
 endelse


end
