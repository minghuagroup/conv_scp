
function get_lev_cam, dd, var,  scale, diff=diff,  $
            missing = missing, nn=nn,  contrast = contrast, level=level
; =======================================================================
; input, dd, var,missing, nn 
; output lev, diff

  cam_vars = cam_vars()
  restore,'scale_cam.sav0'   ; scale_cam and cam_lev structure   ; after cam_scaling2.pro
  ;scale_cam = scale_cam
  ;cam_lev   = cam_lev

if(not keyword_Set(nn))then nn = 20                     ; forced lines

  names = tag_names(cam_lev)
  varj = strupcase(var)
  if(belongsto(varj, names)) then begin
    levj = get_stru(cam_lev, varj)
    lev = cal_lev(levj[0:1],nn)
    scale = levj[2]
  endif else begin
    lev = get_lev(dd, var)  
  endelse

  ddj  = dd
 if(keyword_set(diff))then begin
  diff0 = get_lev_adjust( ddj, lev, nn=nn, lev_max=lev_max_diff, lev_min = lev_min_diff, missing = missing)
  if(not keyword_Set( contrast))then contrast= 1.
  lev = get_lev_diff(diff0, nn, contrast = contrast)
 endif


  if(keyword_set(level))then begin
   lev = get_lev_adjust( ddj, lev)
  endif

  return, lev

; --- end
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

end
