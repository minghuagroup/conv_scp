 function less_100, dd, var
; ------------------------------
; check whether should be multiplied by 100, and set the limit to 100

 cam_var1 = strupcase(era5_2cesm_var(var))
 cam_var2 = strupcase(cmip6_2cesm_var(var))

 dd0 = dd

 vars_cld =  ['CC','CLOUD','CLDTOT','CLDLOW','CLDMED','CLDHGH','RELHUM','RELHUMS','AST','CLDST','LCLOUD','CUFRAC_CU','LIQCLD', 'LCLOUD']

  vars_prec = 'PRECC, PRECL , PRECLS, PRECCS, PRECT'
  vars_prec = str_sep(strcompress(vars_prec),',')

  vars1 = 'CL CRWC CSWC CSF CSFR CSWC CVH CVL ISOR LAIHV LAILV LICD LSF LANDFRAC RO RSN SD SDFOR SDOR SF'
  vars2 = 'SLT SMLT SRC SRO SSRO SWVL4 TCRW TVH TVL LSRR LSSFR SF'
  vars_ec = vars1 +' ' + vars2
  vars_ec = str_sep(strcompress(vars_ec),' ')

  vars_all = [vars_prec, vars_cld, vars_ec]

  varsj = vars_all
  if(belongsto(var, varsj) or belongsto(cam_var1, varsj) or belongsto(cam_var2, varsj)) then begin
    jj = where(dd0 lt 0, cnt)
    if(cnt gt 0)then dd0[jj] = 0.0
  endif
;if(var eq 'CSF')then stop

  varsj = vars_cld
  if(belongsto(var, varsj) or belongsto(cam_var1, varsj) or belongsto(cam_var2, varsj)) then begin
    if(max(dd0) gt 2.)then begin
      jj = where(dd0 gt 100., cnt)    ; set maximum to 100
      if(cnt gt 0)then dd0[jj] = 100. 
    endif else begin
     if(max(dd0) lt 2.)then begin
      jj = where(dd0 gt 1., cnt)    ; set maximum to 100
      if(cnt gt 0)then dd0[jj] = 1.
     endif
    endelse
  end
    

 return,dd0
end


