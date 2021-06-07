
; get ready data aa, xx,yy, var, lev1 and lev2, then run plot_3d

igif= 0
iix =  1
;=====

;aa = rh2
;var ='RH'
;aa = cloud4
;aa = cfrac3
;var ='CLD'


;get_lev,aa,'CLOUD',lev1,scale

;lev1=lev1/100.


;lev2 = [1000.,2000]

 ctypes =['Zonal','Meridional','Vertical','GPCI', $
          'P_mb','Latitude_range','Longitude_Range']

for k=0,6 do print,k+1,'  ',ctypes[k]
print,'which iave type? then do .r plot_3d '
read,iave

; iave=1 ;zonal average ;!!!!!!!!!!!
; iave=2 ;meridional average
; iave=3 ;vertical average
; iave=4 ;GPCI
; iave=5 ;p level [100,100] mb]
; iave=6 ; zonal cross section
; iave=7 ; latitudional cross section


ctype = ctypes[iave-1]
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
    print,'P or -k range?  negative means k='
;    ----------------
    read,z_Range
    iave=3   ; vertically level
   end 

 6:begin
    lon_range = [0.,360]
    lat_range = [-90.,90]
    z_range   = [1000.,0]
    print,'Latitude range? [-90,90]
;    ----------------
    read,lat_range
    iave=2   ; vertically level
   end 

 7:begin
    lon_range = [0.,360]
    lat_range = [-90.,90]
    z_range   = [1000.,0]
    iave=1   ; vertically level
    print,'Longitude range? [0-360]
;    ----------------
    read,lon_range
   end 

 else:
 endcase

print,ctype
;==================

end
