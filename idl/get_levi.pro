
;==========================
Pro get_levi,dd, VAR,LEV,SCALE

SCALE=1.
SCALE_PRECIP = 1000*86400.
SCALE_Q      = 1000.
scale_omega = 3600 /100.  ;mb/hr
scale_dqdt=   1.0e3*86400 ; g/kg/day
scale_dtdt =  86400. ; T/day
scale_liq =   1.0e4  ; 10^-2 g/kg
scale_cldnl=  1.0e-6 ; cm-3
scale_cldni=  1.0e-6 ;
scale_tgliq = 1.0e6   ;mm
scale_p =     1.0e-2  ;mb


  CASE VAR OF

 'accuprec': BEGIN LEV =  CAL_LEV([0.,100],20)    & SCALE=86400. & END
 'bs_xc':    BEGIN LEV =  CAL_LEV([0.,11],11)    & SCALE=10.0 & END
 'buoy':    BEGIN LEV =  CAL_LEV([-10,10],20)    & SCALE=100 & END
 'buoy_mid':    BEGIN LEV =  CAL_LEV([-10,10],20)    & SCALE=100 & END
 'condrate': BEGIN LEV =  CAL_LEV([0.,30],20)    & SCALE=scale_precip & END
 'det_rate': BEGIN LEV =  CAL_LEV([0.,2],20)    & SCALE=1000.0 & END
 'det_rate_sh': BEGIN LEV =  CAL_LEV([0.,2],20)    & SCALE=1000.0 & END
 'diffdse_dn': BEGIN LEV =  CAL_LEV([-8,8],20)    & SCALE=1./1004. & END
 'diffdse_up': BEGIN LEV =  CAL_LEV([-8.,8],20)    & SCALE=1./1004. & END
 'diffq_dn': BEGIN LEV =  CAL_LEV([-4.,4],20)    & SCALE=1000. & END
 'diffq_up': BEGIN LEV =  CAL_LEV([-4.,4],20)    & SCALE=1000. & END
 'ent_rate': BEGIN LEV =  CAL_LEV([0.,4],20)    & SCALE=1000.0 & END
 'ent_rate_sh': BEGIN LEV =  CAL_LEV([0.,4],20)    & SCALE=1000.0 & END
 'evaprate': BEGIN LEV =  CAL_LEV([0.,10],20)    & SCALE=scale_precip & END
 'massflx': BEGIN LEV =  CAL_LEV([0.,10],20)    & SCALE=1.0e2 & END
 'normassflx_dn':   BEGIN LEV =  CAL_LEV([0.,10],20)    & SCALE=-1.0 & END
 'normassflx_up':   BEGIN LEV =  CAL_LEV([0.,10],20)    & SCALE=1.0 & END
 'precrate': BEGIN LEV =  CAL_LEV([0.,10],20)    & SCALE=scale_precip & END
 'qice_up': BEGIN LEV =  CAL_LEV([0.,20],20)    & SCALE=1.0e5 & END
 'qliq_up': BEGIN LEV =  CAL_LEV([0.,40],20)    & SCALE=1.0e4 & END
 'qliqtenddet': BEGIN LEV =  CAL_LEV([0.,10],20)    & SCALE=1.0e8 & END
 'qtend': BEGIN LEV =  CAL_LEV([-20.,20],20)/2.    & SCALE=scale_precip & END
 'qtendcond': BEGIN LEV =  CAL_LEV([-20.,20],20/2.)    & SCALE=scale_precip & END
 'qtendevap': BEGIN LEV =  CAL_LEV([-20.,20],20)/2.    & SCALE=scale_precip & END
 'qtendtrandn': BEGIN LEV =  CAL_LEV([-20.,20],20)/2.    & SCALE=scale_precip & END
 'qtendtranup': BEGIN LEV =  CAL_LEV([-20.,20],20) /2.   & SCALE=scale_precip & END
 'radius_up': BEGIN LEV =  CAL_LEV([0.,50],20)    & SCALE=1.0e-3 & END
 'rainrate': BEGIN LEV =  CAL_LEV([0.,10],20)    & SCALE=scale_precip & END
 'snowrate': BEGIN LEV =  CAL_LEV([0.,10],20)    & SCALE=scale_precip & END
 'stend': BEGIN LEV =  CAL_LEV([-10.,10],20)    & SCALE=1/1004.*86400 & END
 'stendcond': BEGIN LEV =  CAL_LEV([-10.,10],20)    & SCALE=1.0/1004*86400 & END
 'stendevap': BEGIN LEV =  CAL_LEV([-10.,10],20)    & SCALE=1.0/1004*86400 & END
 'stendtranup': BEGIN LEV =  CAL_LEV([-10.,10],20)    & SCALE=1.0/1004*86400 & END
 'stendtrandn': BEGIN LEV =  CAL_LEV([-10.,10],20)    & SCALE=1.0/1004*86400 & END
 'w_up': BEGIN LEV =  CAL_LEV([0.,10],20)    & SCALE=1.0 & END
 'w_up_mid': BEGIN LEV =  CAL_LEV([0.,10],20)    & SCALE=1.0 & END
 'dilucape': BEGIN LEV =  CAL_LEV([0.,10],20)    & SCALE=1.0e-2 & END
  ELSE: begin 
       jj = where(dd gt -9990.,cnt)
       if(cnt gt 0)then lev = cal_lev([min(dd[jj]),max(dd[jj])],20)     
       if(max(lev) eq min(lev))then lev = indgen(10)*1.0 + min(lev)
      
        end
  ENDCASE

  return

end
 

