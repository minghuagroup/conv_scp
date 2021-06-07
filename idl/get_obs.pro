
function get_obs, date, var, src, daily=daily, hr24=hr24, native=native

 if(not keyword_set(daily))then daily=0
 if(not keyword_set(fc))then fc=0

  yyyymmddhh = date+'    '
  yyyy = strmid(yyyymmddhh,0,4)
  mm   = strmid(yyyymmddhh,4,2)
  dd   = strmid(yyyymmddhh,6,2)
  hh   = strmid(yyyymmddhh,8,2)

  if(not keyword_set(daily))then daily = 0
  if(not keyword_set(hr24 ))then hr24  = 0
  if(not keyword_set(native))then native = 0

  restore,'iap.sav0'
  nx = n_elements(lon2)
  ny = n_elements(lat2)
  dd_missing = fltarr(nx,ny)-9999.


 SRC2 = strupcase(src)

;======================================================================================
 CASE SRC2 OF
; erai, era5, gpcp, visst, c3m, , cmorph...
;======================================================================================
'ERAI': begin 
        scale= 1.
        fileinfo = date
  case 1 of
   belongsto(var,['PRECC','PRECL','PRECT']): begin
        dd = get_flderai(fileinfo,var,daily=daily,hr24=hr24,native=native)
          scale = 1./86400.*3.
           dd=dd*scale
           scale = 1.0
         end
   belongsto(var,['LHFLX','SHFLX']): begin
        dd = get_flderai(fileinfo,var,daily=daily,hr24=hr24,native=native)
           scale = 36.
           dd=dd*scale
           scale = 1.0
          end
   belongsto(var,['FLNTC','FLNT','FSNTC',$
         'FSNT','FLNS','FSNSC','FLNS','FLNSC','FLDS','FLDSC']): begin
        dd = get_flderai(fileinfo,var,daily=daily,hr24=hr24,native=native)
           scale = 36.
           dd=dd*scale
           scale = 1.0
             end
    belongsto(var,['LWCF']) :     begin
        dd1 = get_flderai(fileinfo,'FLNTC',daily=daily,hr24=hr24,native=native)
        dd2 = get_flderai(fileinfo,'FLNT',daily=daily,hr24=hr24,native=native)
;stop
        dd  = dd1 - dd2
           scale = 36.
           dd=dd*scale
           scale = 1.0
;stop

               end
    belongsto(var,['SWCF']) :     begin
        dd1 = get_flderai(fileinfo,'FSNTC',daily=daily,hr24=hr24,native=native)
        dd2 = get_flderai(fileinfo,'FSNT',daily=daily,hr24=hr24,native=native)
        dd  = dd2 - dd1
           scale = 36.
           dd=dd*scale
           scale = 1.0
        end
   else: begin
        dd = get_flderai(fileinfo,var,daily=daily,hr24=hr24,native=native)
        end

   endcase

       dd = dd*scale

  end ;ERAI end

;======================================================================================
'ERA5': begin 
  
  scale1 = 1.
  fileinfo = date
  if(strlen(date) eq 6)then begin
    fileinfo = fileinfo+'00'
    daily    = 1
  endif

  case 1 of
    (var eq 'PRECT'): begin
       dd1 = get_fldera5(fileinfo,'PRECL', daily=daily, fc=1, native=native,hr24=hr24)  ; yyyy, 
       dd2 = get_fldera5(fileinfo,'PRECC', daily=daily, fc=1, native=native,hr24=hr24)  ; yyyy, 
       dd  = (dd1+dd2)
       scale1 = 1.0e-3
              end
    belongsto(var, ['PRECL','PRECC']) : begin
       dd = get_fldera5(fileinfo,var, daily=daily, fc=1, native=native,hr24=hr24)  ; yyyy, 
       scale1 = 1.0e-3
             end
    belongsto(var, ['Q', 'T', 'U', 'V', 'OMEGA', 'CLOUD', 'CLDLIQ', 'CLDICE', 'RELHUM']): begin
        dd = get_fldera5(fileinfo,var, ml=1,daily=daily, native=native,hr24=hr24)  ; yyyy, 
         end
    (var eq 'LHFLX') :     begin
        dd = get_fldera5(fileinfo,var, daily=daily, fc=0, native=native,hr24=hr24)  ; yyyy, 
        scale1 = -2.5e6
             end
    belongsto(var,['SHFLX', 'TAUX', 'TAUY']) :     begin
        dd = get_fldera5(fileinfo,var, daily=daily, fc=0, native=native,hr24=hr24)  ; yyyy, 
        scale1 = -1.0
               end
    belongsto(var,['LWCF']) :     begin
        dd1 = get_fldera5(fileinfo,'FLNTC', daily=daily, fc=1, native=native,hr24=hr24)  ; yyyy, 
        dd2 = get_fldera5(fileinfo,'FLNT', daily=daily, fc=1, native=native,hr24=hr24)  ; yyyy, 
        dd  = dd1 - dd2
        ;dd = dd/3600. 
               end
    belongsto(var,['FLNTC','FLNT','FSNT','FSNTC','FLNS','FLNSC','FSNS','FSNSC']) : begin
        dd = get_fldera5(fileinfo,var, daily=daily, fc=1, native=native,hr24=hr24)  ; yyyy, 
        ;dd = dd/3600. 
               end
    belongsto(var,['SWCF']) :     begin
        dd1 = get_fldera5(fileinfo,'FSNTC', daily=daily, fc=1, native=native,hr24=hr24)  ; yyyy, 
        dd2 = get_fldera5(fileinfo,'FSNT', daily=daily, fc=1, native=native,hr24=hr24)  ; yyyy, 
        dd  = dd2 - dd1
        ;dd = dd/3600. 
               end
    belongsto(var,['U10']) :     begin
        du = get_fldera5(fileinfo,'U10', daily=daily, fc=0, native=native,hr24=hr24)  ; yyyy, 
        dv = get_fldera5(fileinfo,'V10', daily=daily, fc=0, native=native,hr24=hr24)  ; yyyy, 
        dd = sqrt(du*du +dv*dv)
                end
    else  :    begin

       dd = get_fldera5(fileinfo, var, daily=daily, ml=0, native=native,hr24=hr24)  ; yyyy, 
               end
  endcase   ; ERA var end

       dd = dd*scale1 

        end ; ERA end
 
;======================================================================================
'GPCP': begin 
; SYNTAX
; dd = get_fldgpcp(yyyymmdd,var,months=months,native=native)
; yyyymmdd determines whether daily, monthly, annual, or selected months used:
; '20170729', '201707', '2017', months = [6,7,8], native is without interpolation
; var is a dummy, it is not used

  
        fileinfo = strmid(date,0,8) 
        if(var eq 'PRECT')then begin
          dd = get_fldgpcp(fileinfo, var, native=native) 
        endif else begin
          print,'GPCP only has precipitation data, not ', var 
          print,'stopped in get_obs with SRC=gpcp'
          stop
        endelse
        return,dd
        end

;======================================================================================
'VISST': begin 

; Syntax:
; dd= get_fldvisst(yyyymmddhh,var, daily=daily,hr24=hr24, scale1,$
;    nearest=nearest,hours=hours, native=native)  
; scale1 to record what scale in the nc field
; h24 to get hrly data, daily to get daily mean
; hours to record what hrs are available

     scale1 = 1.0
     var1   = var
  case 1 of
     
     (belongsto(var1, ['FSNTC','FSNT'])) : begin

          ss0 = 1365.0  ; TO BE REVISED based on season

          dd24 = get_fldvisst(date,'ALBEDO',daily= 0, scale1,hours=hours,$
             hr24=1,restoreonly=1)
          cosz24 = get_fldvisst(date, 'COSZ'  ,daily=0, hr24=1, scale2,restoreonly=1)
          jj = where(cosz24 gt -9980.,cnt)

          if(n_elements(dd24) eq 1)then return, dd_missing

          if(cnt gt 0)then cosz24[jj] = cos(cosz24[jj]*3.1416/180.*scale2)

          jj      = where(dd24 gt 950.,cnt) ; too large
          if(cnt gt 0)then dd24[jj] = 950.  

          ddj     = dd24*0.0 - 9999.
          jj = where(dd24 gt -8990. and cosz24 gt -8990., cnt)
          jj2=where(cosz24 le 0.,cnt)
          if(cnt gt 0)then cosz24[jj2] = 0.0

          if(cnt gt 0)then ddj[jj] = (1.0 - dd24[jj]*scale1)*cosz24[jj]
          
          if(var1 eq 'FSNT' ) then  dd1 = reform(ddj[0,*,*,*])
          if(var1 eq 'FSNTC') then  dd1 = reform(ddj[1,*,*,*])
          
          dhs = abs(hours-fix(hh))
          jj=where(dhs eq min(dhs))
          j1=jj[0]

          dd = reform(dd1[*,*,j1])

          if(hr24 )then dd = dd1
          if(daily)then dd = ave3(dd1,missing=-998.)
;stop
          dd = dd*ss0
          scale1 = 1.0
                         end 

     (belongsto(var1, ['FLNT','FLNTC'])) : begin
          dd1 = get_fldvisst(date,'FLNT',daily= daily, scale1,hours=hours,hr24=hr24,restoreonly=1)
          if(n_elements(dd1) eq 1)then return, dd_missing

         if(hr24 eq 0)then begin
          if(var1 eq 'FLNT') then  dd =  reform(dd1[0,*,*])
          if(var1 eq 'FLNTC')then  dd =  reform(dd1[1,*,*])
         endif else begin
          if(var1 eq 'FLNT') then  dd =  reform(dd1[0,*,*,*])
          if(var1 eq 'FLNTC')then  dd =  reform(dd1[1,*,*,*])
         endelse
                         end 

     (belongsto(var1, ['FLNSC'])) : begin
          dd1 = get_fldvisst(date,'FLUSC',daily= daily, scale1, hours=hours,hr24=hr24,restoreonly=1)
          if(n_elements(dd1) eq 1)then return, dd_missing
          dd2 = get_fldvisst(date,'FLDSC',daily= daily, scale1, hours=hours,hr24=hr24,restoreonly=1)
          dd  = dd1*0.0-9999.
          jj = where(dd1 gt -9989. and dd2 gt -9989.,cnt)
          if(cnt gt 0)then dd[jj]  = dd1[jj] - dd2[jj]
          scale1 = 1.0
                         end 

     (belongsto(var1, ['FSNSC'])) : begin
          dd1 = get_fldvisst(date,'FSUSC',daily= daily, scale1, hours=hours,hr24=hr24,restoreonly=1)
          if(n_elements(dd1) eq 1)then return, dd_missing
          dd2 = get_fldvisst(date,'FSDSC',daily= daily, scale1, hours=hours,hr24=hr24,restoreonly=1)
          dd  = dd1*0.0-9999.
          jj = where(dd1 gt -9989. and dd2 gt -9989.,cnt)
          if(cnt gt 0)then dd[jj]  = dd2[jj] - dd1[jj]
          scale1 = 1.0
                         end 

     (belongsto(var1, ['LWCF'])) : begin
          dd1 = get_obs(date,'FLNTC'  , 'VISST',daily= daily, hr24=hr24)  ;get obs
          dd2 = get_obs(date,'FLNT'   , 'VISST',daily= daily, hr24=hr24)

          dd  = dd1*0.0-9999.
          jj = where(dd1 gt -9989. and dd2 gt -9989.,cnt)
          if(cnt gt 0)then dd[jj]  = dd1[jj] - dd2[jj]
          scale1 = 1.0
                         end 

     (belongsto(var1, ['SWCF'])) : begin
          dd1 = get_obs(date,'FSNTC'    ,'VISST',daily= daily, hr24=hr24)  ;get obs
          dd2 = get_obs(date,'FSNT'     ,'VISST',daily= daily, hr24=hr24)

          dd  = dd1*0.0-9999.
          jj = where(dd1 gt -9989. and dd2 gt -9989.,cnt)
          if(cnt gt 0)then dd[jj]  = dd2[jj] - dd1[jj]
          scale1 = 1.0
                         end 


     (belongsto(var1, ['CLDTOT','CLDLOW','CLDMED','CLDHGH'])) : begin
          
          dd1 = get_fldvisst(date,'CLDTOT',hr24 = hr24,daily= daily, scale1, hours=hours, rest=1)
        
         if(hr24 eq 0)then begin
          if(var1 eq 'CLDTOT') then  dd = reform(dd1[0,*,*])
          if(var1 eq 'CLDLOW') then  dd = reform(dd1[1,*,*])
          if(var1 eq 'CLDMED') then  dd = reform(dd1[2,*,*])
          if(var1 eq 'CLDHGH') then  dd = reform(dd1[3,*,*])
         endif else begin
          if(var1 eq 'CLDTOT') then  dd = reform(dd1[0,*,*,*])
          if(var1 eq 'CLDLOW') then  dd = reform(dd1[1,*,*,*])
          if(var1 eq 'CLDMED') then  dd = reform(dd1[2,*,*,*])
          if(var1 eq 'CLDHGH') then  dd = reform(dd1[3,*,*,*])
         endelse
                         end 

     (belongsto(var1, ['TGCLDLWP','TGCLDIWP','TGCLDCWP'])) : begin
          
          dd1 = get_fldvisst(date,'TGCLDLWP',daily= daily, scale1,hours=hours,hr24=hr24,rest=1)
        
         if(hr24 eq 0)then begin
          if(var1 eq 'TGCLDIWP') then  dd = reform(dd1[0,*,*])  * scale1
          if(var1 eq 'TGCLDLWP') then  dd = reform(dd1[1,*,*])  * scale1
          if(var1 eq 'TGCLDCWP') then  dd = reform(dd1[0,*,*]+dd1[1,*,*])
         endif else begin
          if(var1 eq 'TGCLDIWP') then  dd = reform(dd1[0,*,*,*])  * scale1
          if(var1 eq 'TGCLDLWP') then  dd = reform(dd1[1,*,*,*])  * scale1
          if(var1 eq 'TGCLDCWP') then  dd = reform(dd1[0,*,*,*]+dd1[1,*,*,*])
         endelse
         scale1 = 1.0
                         end 

     else: begin
           print,'variable not expected by get_fldvisst'
           print,'stopped in get_obs.pro'
           stop 
         ;dd = get_fldvisst(date,var,daily=daily, scale1,hours=hours,hr24=hr24)
           end
     endcase

          dd2 = dd *0.0 - 9999.
          jj = where(dd gt -9989.0,cnt)
          dd2[jj] = dd[jj]*scale1
          return,dd2
        end
 
;======================================================================================
'C3M': begin 

        end
 
;======================================================================================
'OLR': begin 

        end
;======================================================================================
 ELSE:  begin
         print,'no data source is specified in get_obs'
         stop
        end
 ENDCASE 
;======================================================================================

 return,dd

end
