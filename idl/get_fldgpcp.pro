
;=====================================

function get_fldgpcp,yyyymmdd,var,months=months, native=native

;=====================================
 
; yyyymmdd determines whether daily, monthly, annual, or selected months used:
; '20170729', '201707', '2017', months = [6,7,8]
;
; var is a dummy, it is not used
;=====================================

  dirin0 = '/glade/u/home/mzhang/obs/gpci_v1.3_ds728.5/'
  restore,'~/mymods_hist/proc/iap.sav0'

  yyyy = strmid(yyyymmdd,0,4)
  slen = strlen(yyyymmdd)

   file_str = 'XX'
   if(slen eq 8)then file_str = 'daily'
   if(slen eq 6)then file_str = 'monthly'
   if(slen eq 4)then file_str = 'annual or seasons'
   if(file_str eq 'XX')then begin
    print,'yyyymmdd input is incorrect (min 1996), stopped in get_fldgpcp',yyyymmdd
    stop
   endif

  fprefix =dirin0 + yyyy+'/gpcp_daily_cdr_v13rA1_y'+yyyy+'_'

  if(slen eq 4)then begin
   files = file_search(fprefix+'*.nc')  ; annual
  endif else begin 
     if(slen eq 6)then begin
         mm = strmid(yyyymmdd,4,2) 
         files = file_search(fprefix+'m'+mm+'*.nc')  ; monthly
     endif else begin
         mm = strmid(yyyymmdd,4,2) 
         dd = strmid(yyyymmdd,6,2) 
         files = file_search(fprefix+'m'+mm+'_d'+dd+'.nc')  ; daily
     endelse
  endelse

  if(keyword_set(months))then begin
    strs = strmid(strtrim(months+100,2),1,2)
    if(n_elements(months) le 1 or slen gt 4)then begin
      print,'months keyword>0 should have more than 1 month, stopped in get_fldgpcp',months
      print,'months keyword>0 should yyyy date format only',yyyy
      stop
    endif
   files = file_search(fprefix +'m'+strs[0]+'*.nc')
   for i = 1,n_elements(months)-1 do $ 
    files = [files,file_search(fprefix + 'm'+strs[i]+'*.nc') ]
  endif ;months

;stop
   nf = n_elements(files)
   for k = 0,nf-1 do begin
     file = files[k]
     if(k eq 0)then begin ; first file
       lon  = get_fld(file,'longitude') 
       lat  = get_fld(file,'latitude') 
       d2   = get_fld(file,'precip')
       d3   = replicate2(d2,nf)*0.0   ; define array
     endif
     precip = get_fld(file,'precip')
     d3[*,*,k] = precip
   endfor
   precip = ave3(d3,three=3)
   
   if(keyword_Set(native))then return,precip/8.647e7
   
   data = my_interpol2(precip,lon,lat,lon2,lat2)
   data = data/8.64e7  ; return to m/s/m2

   return,data

end
