
function get_obsmn, date, var, src, ann=ann, m12=m12, native=native

 if(not keyword_set(ann))then ann=0
 if(not keyword_set(fc))then fc=0

  yyyymmddhh = date+'    '
  yyyy = strmid(yyyymmddhh,0,4)
  mm   = strmid(yyyymmddhh,4,2)
  dd   = strmid(yyyymmddhh,6,2)
  hh   = strmid(yyyymmddhh,8,2)

  if(not keyword_set(ann))then ann = 0
  if(not keyword_set(m12 ))then m12  = 0
  if(not keyword_set(native))then native = 0

  restore,'iap.sav0'
  nx = n_elements(lon2)
  ny = n_elements(lat2)
  dd_missing = fltarr(nx,ny)-9999.


 print,'src=',src
 SRC2 = strupcase(src)

;======================================================================================
 CASE SRC2 OF
; era5, gpcp, visst, c3m, , cmorph...
;======================================================================================
'ERAI': begin 
        scale= 1.
        fileinfo = date
  case 1 of
   belongsto(var,['PRECC','PRECL','PRECT']): begin
        dd = get_flderaimn(fileinfo,var)
          scale = 1./86400
           dd=dd*scale
           scale = 1.0
         end
   belongsto(var,['LHFLX','SHFLX','FLNTC','FLNT','FSNTC',$
         'FSNT','FLNS','FSNSC','FLNS','FLNSC','FLDS','FLDSC']): begin
        dd = get_flderaimn(fileinfo,var)
           scale = -1./86400
           dd=dd*scale
           scale = 1.0
             end 
    belongsto(var,['LWCF']) :     begin
        dd1 = get_flderaimn(fileinfo,'FLNTC')*(-1./86400.)
        dd2 = get_flderaimn(fileinfo,'FLNT')*(-1./86400.)
        dd  = dd1 - dd2
;stop
               end
    belongsto(var,['SWCF']) :     begin
        dd1 = get_flderaimn(fileinfo,'FSNTC')*(-1./86400.)
        dd2 = get_flderaimn(fileinfo,'FSNT')*(-1./86400.)
        dd  = dd2 - dd1 
        dd  = -dd
        end
   else: begin
        dd = get_flderaimn(fileinfo,var)
        end

   endcase 

       dd = dd*scale 
   end ;ERAI end

'ERA5': begin 
  
  scale1 = 1.
  fileinfo = date
  if(strlen(date) eq 6)then begin
    fileinfo = fileinfo+''
  endif

  case 1 of
    (var eq 'PRECT'): begin
       dd1 = get_fldera5mn(fileinfo,'PRECL', ann=ann, fc=1, native=native)  ; yyyy, 
       dd2 = get_fldera5mn(fileinfo,'PRECC', ann=ann, fc=1, native=native)  ; yyyy, 
       dd  = (dd1+dd2)
       scale1 = 1.0e-3
              end
    belongsto(var, ['PRECL','PRECC']) : begin
       dd = get_fldera5mn(fileinfo,var, ann=ann, fc=1, native=native)  ; yyyy, 
       scale1 = 1.0e-3
             end
    belongsto(var, ['Q', 'T', 'U', 'V', 'OMEGA', 'CLOUD', 'CLDLIQ', 'CLDICE', 'RELHUM']): begin
        dd = get_fldera5mn(fileinfo,var, ml=1,ann=1, native=native)  ; yyyy, 
        scale1 = 1.0
         end
    (var eq 'LHFLX') :     begin
        dd = get_fldera5mn(fileinfo,var, ann=ann, fc=0, native=native)  ; yyyy, 
        scale1 = -2.5e6
             end
    belongsto(var,['SHFLX', 'TAUX', 'TAUY']) :     begin
        dd = get_fldera5mn(fileinfo,var, ann=ann, fc=0, native=native)  ; yyyy, 
        scale1 = -1.0
               end
    belongsto(var,['U10']) :     begin
        du = get_fldera5mn(fileinfo,'U10', ann=ann, fc=0, native=native)  ; yyyy, 
        dv = get_fldera5mn(fileinfo,'V10', ann=ann, fc=0, native=native)  ; yyyy, 
        dd = sqrt(du*du +dv*dv)
                end
    belongsto(var,['LWCF']) :     begin
        dd1 = get_fldera5mn(fileinfo,'FLNTC',ann=ann,   fc=0, native=native)  ; yyyy, 
        dd2 = get_fldera5mn(fileinfo,'FLNT',ann=ann,  fc=0, native=native)  ; yyyy, 
        dd  = dd1 - dd2
               end
    belongsto(var,['SWCF']) :     begin
        dd1 = get_fldera5mn(fileinfo,'FSNTC',ann=ann,  fc=0, native=native)  ; yyyy, 
        dd2 = get_fldera5mn(fileinfo,'FSNT',ann=ann,  fc=0, native=native)  ; yyyy, 
        dd  = dd2 - dd1 
               end

    else  :    begin

       dd = get_fldera5mn(fileinfo, var, ann=ann, ml=0, native=native)  ; yyyy, 
               end
  endcase   ; ERA var end

       dd = dd*scale1 

        end ; ERA end
 
;======================================================================================
'GPCP': begin 
; SYNTAX
; dd = get_fldgpcp(yyyymmdd,var,months=months,native=native)
; yyyymmdd determines whether ann, monthly, annual, or selected months used:
; '20170729', '201707', '2017', months = [6,7,8], native is without interpolation
; var is a dummy, it is not used

  
        fileinfo = strmid(date,0,6) 
        if(var eq 'PRECT')then begin
          dd = get_fldgpcp(fileinfo, var, native=native) 
        endif else begin
          print,'GPCP only has precipitation data, not ', var 
          print,'stopped in get_obsmn with SRC=gpcp'
          stop
        endelse
        return,dd
        end

;======================================================================================
'VISST': begin 

; Syntax:
; dd= get_fldvisstmn(yyyymmddhh,var, scale1=scale1,days=days)
; scale1 to record what scale in the nc field
; days to record what days are available

     ann = 0
     m12 = 0

     scale1 = 1.0
     var1   = var
  case 1 of
     
     (belongsto(var1, ['FSNTC','FSNT'])) : begin

       ; get daily 24 hours
       ; average over the hours
       ; average to day

          ss0 = 1365.0  ; TO BE REVISED based on season
       
       j1 = caldate2(yyyy+'010100',0,day=i0)
       
       j1 = caldate2(yyyy+mm+'0100',0,day=i2)
       ithday = i2-i0
        

       ins = cal_ins(ithday, 0.,0., cz, ss0=ss0)

print,ss0

       day0 = yyyy+mm+'0100'
       
       albedo30 = fltarr(nx,ny,24,31) - 9999. 
       cosz30   = fltarr(nx,ny,24,31) - 9999. 
       k2 = 0
       for k = 0,30 do begin
          date2 = caldate2(day0,k)
          if(strmid(date2,4,2) ne  mm) then goto,day_end
   

          dd24 = get_fldvisst(date2,'ALBEDO',daily= 0, scale1,hours=hours,$
             hr24=1,restoreonly=1,f1sav=fsav1,f2sav=fsav2,ff=ff1)
          
          if(n_elements(dd24) eq 1)then goto,day_end
          if(n_elements(hours) le 2)then goto,day_end

          if(var eq 'FSNT') then dd24 = reform(dd24[0,*,*,*])
          if(var eq 'FSNTC')then dd24 = reform(dd24[1,*,*,*])

          for j = 0,n_elements(hours)-1 do  $
          albedo30[*,*,hours[j],k] = dd24[*,*,j]   

          cosz24 = get_fldvisst(date2, 'COSZ'  ,daily=0, hr24=1, scale2,restoreonly=1, $
               hours=hours2,f1sav=fsav1b,f2sav=fsav2b,ff=ff2)
          if(n_elements(dd24) eq 1)then goto, day_end

          for j = 0,n_elements(hours2)-1 do  $
           cosz30[*,*,hours2[j],k] = cosz24[*,*,j]   

           k2 = k2+1
       endfor
       day_end:

       albedo30 = albedo30[*,*,*,k2-1]
       cosz30   = cosz30[*,*,*,k2-1]  ; in 0.01 degree!

       b24 = fltarr(nx,ny,24)
       for k=0,23 do begin       ; 1 hr at a time
         dj1 = reform(albedo30[*,*,k,*])

         jj      = where(dj1*scale1 gt 0.95,cnt) ; too large
         if(cnt gt 0)then dj1[jj] = 0.95/scale1

         cj1 = reform(cosz30[*,*,k,*]) 

         jj = where(cj1 gt -9100.,cnt)
         if(cnt gt 0)then cj1[jj] = cos(cj1[jj]*3.1416/180.*scale2)
         jj = where(cj1 gt -9100. and cj1 le 0.0 ,cnt)
         if(cnt gt 0)then cj1[jj] = 0.0

         dj2 = fltarr(nx,ny)-9999.
         jj = where(cj1 gt -8990. and dj1 gt -8990., cnt)
         if(cnt gt 0)then dj2[jj] = (1.0 - dj1[jj]*scale1)*cj1[jj]
         
         b24[*,*,k] = dj2

        endfor

         dd = ave3(b24)*ss0
         scale1 = 1.0  

;stop
                         end 

     (belongsto(var1, ['FLNT','FLNTC'])) : begin
          dd1 = get_fldvisstmn(date,'FLNT',ann= ann, scale1,days=days )
         if(m12 eq 0)then begin
          if(var1 eq 'FLNT') then  dd =  reform(dd1[0,*,*])
          if(var1 eq 'FLNTC')then  dd =  reform(dd1[1,*,*])
         endif else begin
          if(var1 eq 'FLNT') then  dd =  reform(dd1[0,*,*,*])
          if(var1 eq 'FLNTC')then  dd =  reform(dd1[1,*,*,*])
         endelse
                         end 

     (belongsto(var1, ['FLNSC'])) : begin
          dd1 = get_fldvisstmn(date,'FLUSC',ann= ann, scale1, days=days )
          dd2 = get_fldvisstmn(date,'FLDSC',ann= ann, scale1, days=days )
          dd  = dd1*0.0-9999.
          jj = where(dd1 gt -9989. and dd2 gt -9989.,cnt)
          if(cnt gt 0)then dd[jj]  = dd1[jj] - dd2[jj]
          scale1 = 1.0
                         end 

     (belongsto(var1, ['FSNSC'])) : begin
          dd1 = get_fldvisstmn(date,'FSUSC',ann= ann, scale1, days=days )
          dd2 = get_fldvisstmn(date,'FSDSC',ann= ann, scale1, days=days )
          dd  = dd1*0.0-9999.
          jj = where(dd1 gt -9989. and dd2 gt -9989.,cnt)
          if(cnt gt 0)then dd[jj]  = dd2[jj] - dd1[jj]
          scale1 = 1.0
                         end 

     (belongsto(var1, ['LWCF'])) : begin
          dd1 = get_obsmn(date,'FLNTC'  , 'VISST',ann= ann, m12=m12)  ;get obs
          dd2 = get_obsmn(date,'FLNT'   , 'VISST',ann= ann, m12=m12)

          dd  = dd1*0.0-9999.
          jj = where(dd1 gt -9989. and dd2 gt -9989.,cnt)
          if(cnt gt 0)then dd[jj]  = dd1[jj] - dd2[jj]
          scale1 = 1.0
                         end 

     (belongsto(var1, ['SWCF'])) : begin
          dd1 = get_obsmn(date,'FSNTC'    ,'VISST',ann= ann, m12=m12)  ;get obs
          dd2 = get_obsmn(date,'FSNT'     ,'VISST',ann= ann, m12=m12)

          dd  = dd1*0.0-9999.
          jj = where(dd1 gt -9989. and dd2 gt -9989.,cnt)
          if(cnt gt 0)then dd[jj]  = dd2[jj] - dd1[jj]
          scale1 = 1.0
                         end 


     (belongsto(var1, ['CLDTOT','CLDLOW','CLDMED','CLDHGH'])) : begin
          
          dd1 = get_fldvisstmn(date,'CLDTOT',m12 = m12,ann= ann, scale1, days=days)
        
         if(m12 eq 0)then begin
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
          
          dd1 = get_fldvisstmn(date,'TGCLDLWP',ann= ann, scale1,days=days )
        
         if(m12 eq 0)then begin
          if(var1 eq 'TGCLDIWP') then  dd = reform(dd1[0,*,*]) * scale1
          if(var1 eq 'TGCLDLWP') then  dd = reform(dd1[1,*,*]) * scale1
          if(var1 eq 'TGCLDCWP') then  dd = reform(dd1[0,*,*]+dd1[1,*,*])
         endif else begin
          if(var1 eq 'TGCLDIWP') then  dd = reform(dd1[0,*,*,*]) * scale1
          if(var1 eq 'TGCLDLWP') then  dd = reform(dd1[1,*,*,*]) * scale1
          if(var1 eq 'TGCLDCWP') then  dd = reform(dd1[0,*,*,*]+dd1[1,*,*,*])
         endelse
         scale1 = 1.0
                         end 

     else: begin 
         print,'no data source is specified in get_obsmn for ', SRC
         stop
         end
     endcase

          dd2 = dd *0.0 - 9999.
          jj = where(dd gt -998.0,cnt)
          dd2[jj] = dd[jj]*scale1
          return,dd2
;stop
        end
 
;======================================================================================
'C3M': begin 

        end
 
;======================================================================================
'OLR': begin 

        end
;======================================================================================
 ELSE:  begin
         print,'no data source is specified in get_obsmn'
         stop
        end
 ENDCASE 
;======================================================================================

 return,dd

end
