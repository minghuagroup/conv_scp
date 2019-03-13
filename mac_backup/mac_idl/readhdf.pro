pro readhdf,f=f,desc=d,init=init,group=g,eta=eta,verby=verby,list=fields,isub=isub,only=only,errorcheck=errchk
;------------------------------------------------------
; $Id: readhdf.pro,v 1.4 2006/12/13 19:40:59 bacmj Exp $
;------------------------------------------------------

common readmngr, sweeping
common datamngr, datamngr

if keyword_set(sweeping) then begin
   chx=hdf_open(f,/read)
   if chx eq -1 then begin 
      print,'!!!**', f ,'  NOT THERE !!!! '
   endif
   RETURN
endif

if keyword_set(errchk) or keyword_set(error_checking) then begin
   chx=hdf_open(f,/read)
   if chx eq -1 then begin 
      print,'!!!**', f ,'  NOT THERE !!!! '
      g={file:f,error:-999}
      RETURN
   endif
endif

if f eq 'n/a' then begin
   g={file:f,error:-999}
   return
endif


sdi=hdf_sd_start(f,/read)
hdf_sd_fileinfo,sdi,nd,na

xshift=0
if tagset( datamngr, 'datelinelons' ) then begin
   i=hdf_sd_nametoindex(sdi,'lon')
   if ( i ge 0 ) then begin
     dd=hdf_sd_select(sdi,i)
     hdf_sd_getdata,dd,data
     if max(data) gt 180. then begin
        xshift=1
        print,' This data set -- '
        print,'      ',f 
        print,' will be shifted from GM to DTLN origin '
     endif
   endif
endif


if keyword_set(only) then begin
   only = ['LON','LAT','LEV',only]
endif
if keyword_set(fields) then begin
   fields = ['LON','LAT','LEV','PS','DELP',fields]
endif

d={file:f}
g={file:f , weight:1.0 }



for i = 0,nd-1 do begin

   dd=hdf_sd_select(sdi,i)
   hdf_sd_getinfo,dd,name=name

   if keyword_set(only) then begin
      oo=where( strupcase(name) eq strupcase(only) )
      if( min(oo) ne -1) then begin  
         read_the_data = 1
      endif else begin
         read_the_data = 0
      endelse
   endif else begin
      read_the_data = 1
   endelse

   if read_the_data eq 1 then begin
   hdf_sd_getdata,dd,data


   if keyword_set(xshift) then fixup_lons,data,name
      

   if keyword_set(verby) then begin
      print," have ", name, " at " , size(data)
   endif

   if not keyword_set(fields) then begin

      dd={var:name,i:i}
      d=create_struct(d,name,dd)

         s=size(data)

         if not keyword_set(isub) then begin

            g=create_struct(g,name,data)
            if keyword_set(verby) then begin
              print," created ", name, " at " , size(data)
            endif 

          endif else begin

            case s(0) of
              1: begin
                 if strupcase(name) eq 'LON' then data=data(isub(0):isub(1))
                 if strupcase(name) eq 'LAT' then data=data(isub(2):isub(3))
                 if strupcase(name) eq 'LEV' and $
                       n_elements(isub) eq 6 then data=data(isub(4):isub(5))
                 end
              2: data=data( isub(0):isub(1), isub(2):isub(3) )
              3: begin 
                 if n_elements(isub) eq 4 then data=data( isub(0):isub(1), isub(2):isub(3) , * )
                 if n_elements(isub) eq 6 then data=data( isub(0):isub(1), isub(2):isub(3) , isub(4):isub(5) )
                 end
              4: begin 
                 if n_elements(isub) eq 4 then data=data( isub(0):isub(1), isub(2):isub(3) , * , * )
                 if n_elements(isub) eq 6 then data=data( isub(0):isub(1), isub(2):isub(3) , isub(4):isub(5), * )
                 end
            endcase
            g=create_struct(g,name,data)
            if keyword_set(verby) then begin
              print," created ", name, " at " , size(data)
            endif 
               
          endelse


   endif else begin

      present = strupcase(name) eq strupcase(fields)
      print,name,present

      if total(present) eq 1 then begin


         dd={var:name,i:i}
         d=create_struct(d,name,dd)
         s=size(data)

         if not keyword_set(isub) then begin

            g=create_struct(g,name,data)
            if keyword_set(verby) then begin
              print," created ", name, " at " , size(data)
            endif 

          endif else begin

            case s(0) of
              1: begin
                 if strupcase(name) eq 'LON' then data=data(isub(0):isub(1))
                 if strupcase(name) eq 'LAT' then data=data(isub(2):isub(3))
                 if strupcase(name) eq 'LEV' and $
                       n_elements(isub) eq 6 then data=data(isub(4):isub(5))
                 end
              2: data=data( isub(0):isub(1), isub(2):isub(3) )
              3: begin 
                 if n_elements(isub) eq 4 then data=data( isub(0):isub(1), isub(2):isub(3) , * )
                 if n_elements(isub) eq 6 then data=data( isub(0):isub(1), isub(2):isub(3) , isub(4):isub(5) )
                 end
            endcase

            g=create_struct(g,name,data)
            if keyword_set(verby) then begin
              print," created ", name, " at " , size(data), " SUBSET ",isub
            endif 
               
          endelse
       endif
    endelse
    endif
endfor

hdf_sd_end,sdi  

if tagset(g,'lon') then d=create_struct(d,'lons',g.lon)
if tagset(g,'lat') then d=create_struct(d,'lats',g.lat)

if keyword_set(eta) then begin
  etapress,g=g
endif



if tagset(g,'lev') then begin
  d=create_struct(d,'levels',g.lev)
endif else begin
  d=create_struct(d,'levels',[1.0] )
endelse
  

end
