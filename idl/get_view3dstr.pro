;==================================== =====================================
function get_view3dstr, dd, var, var2=var2, dd2=dd2, lev_adjust=lev_adjust
;==========================================================================

 ; work together with get_lev_str and get_lev2_nn for view3d

 if(not keyword_set(var2))then var2 = var
 if(not keyword_set(dd2))then  dd2  = dd
 
 var_cam  = cmip6_2cesm_var(var, c=1)
 var_cam2 = cmip6_2cesm_var(var2, c=1)


; default values

    lev2_nn = 2  ; default

    lev1_max = 0
    lev1_min = 0   ; default values
    lev2_max = 0
    lev2_min = 0   ; default values

    colorbar_nn = 20
    vect = 0
    usteps = 10 
    vsteps = 10
    vlen = 3

    vcolor = 0
    
   levstr = get_levstr( dd, var_cam)
   lev1   = levstr.lev
   scale1 = levstr.scale

   levstr = get_levstr( dd2, var_cam2)
   lev2   = levstr.lev
   scale2 = levstr.scale

 if(not keyword_Set(lev_adjust)) then begin  ; old way of lev1 and lev2
;=========================================
   lev_adjust = 0

 endif else begin ; new way of adjust contour and vector levels
;=========================================

    case var_cam of
 'H': begin
      lev1_max = 450.
          end
 'THETA': begin
      lev1_max = 450.
          end
 'Z3': begin
      lev1_min = -0.1
          end
 else:
     endcase

  ; lev2_nn
  ; determine how many line intervals and whether to do lines
  ; lev2_nn < 0,  nn=2 every two lines for the contour overplot
  ;  lev2_nn = 2  ; default

 if((strpos(var_cam, 'PREC') eq 0 ) or (strpos(var_cam, 'CLD') ge 0) or (strpos(var_cam,'CRF') eq 0) $
       or (strpos(var,'sn') eq 0) or (strpos(var, 'sbl') ge 0)    $
       or (strpos(var, 'mr') eq 0)   $
       or (belongsto(var, ['LHFLX','SHFLX', 'LANDFRAC','OMEGA','PME','QFLX']))  $
       or ( belongsto(var, ['ATMNET','CI','CLOUD', 'LWCF', 'SWCF']))  $
           ) $
        then lev2_nn = -1  ; no lines added

 endelse  ; lev_adjust
;=========================================

 view3dstr = create_struct('var1',var, 'var2', var2, 'var1_cam', var_cam, 'var2_cam', var_cam2, $
        'lev_adjust', lev_adjust, 'lev2_nn', lev2_nn, $
        'lev1_max', lev1_max, 'lev1_min', lev1_min, 'lev2_max',lev2_max, 'lev2_min',lev2_min, $
        'colorbar_nn', colorbar_nn, 'usteps', usteps, 'vsteps',vsteps, 'vlen', vlen,    $
         'vcolor',vcolor,$
         'lev1', lev1, 'lev2', lev2, 'scale1', scale1, 'scale2', scale2)

 ;----------------------------
;if(var eq 'ta' and var2 eq 'zg')then stop

 return, view3dstr

end

