
function get_lev_era5, dd, var,  scale, diff=diff, cam = cam , $
            missing = missing, nn=nn, level=level, contrast = contrast
; =======================================================================
; input, dd, var,missing, nn 
; output lev, diff

if(not keyword_set(cam))then begin
  era5_vars = era5_vars()
  restore,'scale_era5.sav0'   ; scale_Era5 and era5_lev structure  ; after era5_scaling2.pro
endif else begin
  era5_vars = cam_vars()
  restore,'scale_cam.sav0'   ; scale_cam and cam_lev structure   ; after cam_scaling2.pro
  scale_era5 = scale_cam
  era5_lev   = cam_lev
endelse

if(not keyword_Set(missing))then missing  = 1.0e15
if(not keyword_Set(nn))then nn = 20                     ; forced lines

  jj = where(abs(dd) lt abs(missing), cnt)
       if(cnt gt 0)then begin
         dmin = min(dd[jj])
         dmax = max(dd[jj])
         LEV00 =  [dmin,dmax, 1]
         delta = (dmax-dmin)/nn
         DIFF0 = [-nn*delta/2 ,nn*delta/2 ,nn]
        endif else begin
         lev = [0,0,1]
         diff0 = [0,0,1]
        endelse

  names = tag_names(era5_lev)
  jj = where(strupcase(var) eq names, cnt)
  if(cnt gt 0) then begin
    levj = get_stru(era5_lev, names[jj[0]])  ; [0., 1., 1.0e5] e.g.
    scale = levj[2]
  endif else begin
    levj = lev00
    scale = 1
  endelse

  levj = levj*1.0
   
  lev   = cal_lev(levj[0:1], nn)
    
  ddj  = dd
  diff0 = get_lev_adjust( ddj, lev, nn=nn, lev_max=lev_max_diff, lev_min = lev_min_diff, missing = missing)

  if(keyword_set(level))then begin
   lev = get_lev_adjust( ddj, lev)
  endif

if(not keyword_Set( contrast))then contrast= 1.


 if(not keyword_set(diff))then begin 
  return,lev
 endif else begin
  levj = get_lev_diff(diff0, nn, contrast = contrast)
  return, levj
 endelse
  
end
