
; get ready data aa, xx,yy, var, lev1 and lev2, then run plot_3d

iix = 0
;=====

gifolder = 'temp_CAM_RH'    ;!!!!
aa = rh2  ;xx,yy,zz ;!!!!!
var ='RH'           ;!!!!

;===================

;get_lev,aa,var,lev1,scale
;aa = aa*scale
;lev22 = [1000.,2000]

;==============folder = 'cldfrac3.pro'
;aa = cfrac3*100.  ;xx,yy,zz ;!!!!!
;aa = CLOUD2*100.  ;xx,yy,zz ;!!!!!
;var ='CLOUD'           ;!!!!

;===================


; prepare for aa, lev1, xx,yy,zz
 gf = file_search(gifolder)
 if(gf[0] eq '')then spawn,'mkdir '+ gifolder

 ctypes =['Zonal','Meridional','Vertical','GPCI', $
          'P_mb','Latitude_range','Longitude_Range']

for iave2 = 0,6 do begin ;------- type

 iave = iave2+1

; iave=1 ;zonal average ;!!!!!!!!!!!
; iave=2 ;meridional average
; iave=3 ;vertical average
; iave=4 ;GPCI
; iave=5 ;p level [100,100] mb]
; iave=6 ; zonal cross section
; iave=7 ; latitudional cross section

    lat_width=2.  ; for iave=4 only
    lon_width=2.

ctype1 = ctypes[iave-1]
case iave of
 1:begin
    lon_range = [0.,360]
    lat_range = [-90.,90]
    z_range   = [1000.,0]
   end 

 2:begin
    lon_range = [0.,360]
    lat_range = [-90.,90]
    z_range   = [1000.,0]
   end 

 3:begin
    lon_range = [0.,360]
    lat_range = [-90.,90]
    z_range   = [1000.,100]
   end 

 4:begin
    lon_range = 360-[173.,119]
    lat_range = [0.,40]
    z_range   = [1000.,0]
    lat_width=2.  ; for iave=4 only
    lon_width=2.
   end 

 5:begin
    lon_range = [0.,360]
    lat_range = [-90.,90]
    z_range   = [950.,950]

    iave=3   ; vertically level

    FOR ip = 1000,100,-100 do begin
     z_range   = [ip,ip]*1.0
     ctype = ctype1+'_'+strdigit(ip,0)+'_'+var
     gifname = gifolder+'/'+ctype
     lev2 = lev22
     view3d,var,aa,xx,yy,zz,iave,lev1,lev2,$
        ytitle=ytitle,gifname=gifname, $
        lon_range=lon_range,lat_range=lat_range,z_range=z_range,gifolder=1,window=1
     mygif,gifname
    ENDFOR ; ip
     goto,jump_iave
;    ----------------
   end 

 6:begin
    lon_range = [0.,360]
    lat_range = [-90.,90]
    z_range   = [1000.,0]
    iave=2   ; vertically level

    for ilat = -80,80,10 do begin
     lat_range = [ilat-5,ilat+5.]
     ctype = ctype1+'_'+strdigit(ilat,0)+'_'+var
     gifname = gifolder+'/'+ctype+'.gif'
     lev2 = lev22
     view3d,var,aa,xx,yy,zz,iave,lev1,lev2,$
        ytitle=ytitle,gifname=gifname, $
        lon_range=lon_range,lat_range=lat_range,z_range=z_range,gifolder=1,window=1
     endfor
     goto,jump_iave
;    ----------------
   end 

 7:begin
    lon_range = [0.,360]
    lat_range = [-90.,90]
    z_range   = [1000.,0]
    iave=1   ; vertically level
    for ilon = 10,350,30 do begin
     lon_range = [ilon-10,ilon+10.]
     ctype = ctype1+'_'+strdigit(ilon,0)+'_'+var
     gifname = gifolder+'/'+ctype
     lev2 = lev22

     view3d,var,aa,xx,yy,zz,iave,lev1,lev2,$
        ytitle=ytitle,gifname=gifname, $
        lon_range=lon_range,lat_range=lat_range,z_range=z_range,gifolder=1,window=1

     endfor
     goto,jump_iave
;    ----------------
   end 

 else:
 endcase

;print,ctype
;==================

     ctype = ctype1+'_'+var
     gifname = gifolder+'/'+ctype
     lev2 = lev22

  view3d,var,aa,xx,yy,zz,iave,lev1,lev2,$
        ytitle=ytitle,gifname=gifname, $
        lon_range=lon_range,lat_range=lat_range,z_range=z_range, $
        lon_width=lon_width, lat_width=lat_width,gifolder=1,window=1
  print,''
  print,var, ', iave=',iave,' ',ctypes[iave2]

  if(iix)then read,ix

jump_iave:
ENDFOR ; iave

print,'gffolder',gifolder
 strid = gifolder+'_'+var
 gifgroups = file_search(gifolder+'/*'+var+'*.gif')

 htmlfile ='html_work/'+strid+'.html'
 dir='../'
 gifgroups = dir+gifgroups
 web_view,gifgroups,htmlfile,title=strid,ncolumns=2
 print,'webfile:',htmlfile

end






