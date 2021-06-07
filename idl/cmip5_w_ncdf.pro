
restore,'data/pr_model.sav'

lat = olat
lon = olon
var = 'prec'
aa = pr_model

data3=create_struct(               $
        'fileout','data/prec_mmodels.nc', $
       'time'    ,indgen(12)+1.0,  $
       'lon'     ,lon,             $
       'lat'     ,lat,             $
        'data'       ,aa,             $
        'units'  ,'mm/day',   $
        'attribute','CMIP5 Multi_model Mean Prec')

 w_latlon2d_ncdf,data3

 end
