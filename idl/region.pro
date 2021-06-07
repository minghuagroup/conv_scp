; do 3d plots specifications
; Need to specify: nf, iave,idata


regions = ['Zonal_mean','Meridional_mean','Vertical_mean', $
           'GPCI'      ,'vertical_level ',                 $
           'USA', 'China', 'East Asia','TWP', 'AMAZON']

iave = 1
print,'which type of plots?'
for i=0,n_elements(regions)-1 do print,i+1, ':  ',regions[i]
read,iave



igif= 0
iix = 0
;=====

case iave of
 1:begin
    lon_range = [0.,360]
    lat_range = [-90.,90]
    z_range   = [1000.,0]
    ctype = 'Zonal_mean'
   end 

 2:begin
    lon_range = [0.,360]
    lat_range = [-90.,90]
    z_range   = [1000.,0]
    ctype = 'Meridional_mean'
   end 

 3:begin
    lon_range = [0.,360]
    lat_range = [-90.,90]
    z_range   = [1000.,100]
    ctype = 'Vertical_mean'
   end 

 4:begin
    lon_range = 360-[173.,119]
    lat_range = [0.,40]
    z_range   = [1000.,0]
    lat_width=2.  ; for iave=4 only
    lon_width=2.
    ctype = 'GPCI'
   end 

 5:begin
    lon_range = [0.,360]
    lat_range = [-90.,90]
    z_range   = [950.,950]
    ctype = '950mb'
   end 

 6:begin
    lon_range = [0.,360]
    lat_range = [-90.,90]
    z_range   = [300.,300]
    ctype = '300mb'
   end 

 7:begin
    lon_range = [0.,360]
    lat_range = [-90.,90]
    z_range   = [700.,700]
    ctype = '700mb'
   end 
 else:
 endcase

ctype = regions[iave-1]

;==============================
case ctype of

 'USA': begin
    lon_range = [220.,290.]
    lat_range = [20.,60]
        end 

 'China': begin
    lon_range = [85.,130]
    lat_range = [20.,50]
        end 

 'East Asia': begin
    lon_range = [40.,180]
    lat_range = [-20.,50]
        end 

 'TWP': begin
    lon_range = [80.,160]
    lat_range = [-20.,30.]
        end 

 'AMAZON': begin
    lon_range = [250.,350]
    lat_range = [-60.,20.]
        end 
  else: begin
    lon_range = [0.,360]
    lat_range = [-90.,90]
    z_range   = [1000.,100]
        end
 endcase
;==============================

 xrange=lon_range
 yrange=lat_range
 londel = fix((lon_range[1] - lon_range[0])/12. )
 latdel = fix((lat_range[1] - lat_range[0])/6. )
 xtitle = 'Longitude'
 ytitle= 'Latitude'
if(iave eq 4) then iave=3

end
