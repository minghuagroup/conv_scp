
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
;load "$XXUTILS"

begin

;filepath="./scm-twp06"
;filepath="./scm-sgp95"
filepath="./"

;cases=(/"scam-zm"/)
;cases=(/"scam-jp"/)
;cases=(/"scamcapt-gr-w", "scamcapt-nsj-w"/)
;cases=(/"scamcapt-gr-b", "scamcapt-nsj-b"/)
;cases=(/"scamcapt-zm"/)
cases=(/casename/)

;outlev = (/95000, 90000, 85000, 80000, 70000, 60000, 50000, 40000, 30000, 10000/)
outlev = (/11500, 16500, 21500, 26500, 31500, 36500, 41500, 46500, 51500, \
   56500,  61500, 66500, 71500, 76500, 81500, 86500, 91500, 96500 /)
;vars = (/"ZMDT", "ZMDQ", "T", "TDIFF", "Q", "QDIFF", "CLOUD"/)
;vars = (/"CONVDPDT", "CONVDPDQ", "SCPDT", "SCPDQ", "ZMDT", "ZMDQ", \
;vars = (/"CONVDPDS", "CONVDPDQ", \
         ;"BFCONVT", "TDIFF", "BFCONVQ", "QDIFF", "Z3", "CLOUD", \
         ;"CAMSTEND", "CAMQTEND", \
         ;"CAMCONDSTEND" , "CAMCONDQTEND", \
         ;"CAMTRANUPSTEND", "CAMTRANUPQTEND", \
         ;"CAMTRANDNSTEND", "CAMTRANDNQTEND", \
         ;"CAMEVAPSTEND" , "CAMEVAPQTEND", \
         ;"EVAPTZM", "EVAPQZM" /)
;vars = (/"ECPDT", "ECPDQ", "ZMDT", "ZMDQ"/)
;vars = (/"camttend", "ecpttend", "g3ttend", "camqtend", "ecpqtend", "g3qtend"/)

;vars = (/"STENDCONVDP", "QTENDCONVDP"/)
vars = (/"STENDCONVDP", "QTENDCONVDP", "OFFT", "OFFQ", "Z3", "MSE", "MSESAT", "MSEUP", "QDIFF",\
"TDIFF", "Q", "T", "DLF"/)


do j=0,dimsizes(cases)-1

f = addfile(filepath+"/"+cases(j)+".nc", "rw")

f->lon@units = "degree_east"
f->lat@units = "degree_north"
lon = f->lon
lat = f->lat

lev = f->PMID

;system("rm -rf "+filename+"-int.nc")
;fout = addfile(filename+"-int.nc", "crw")
do i=0,dimsizes(vars)-1
   if ( isfilevar(f, vars(i)) ) then
      data = f->$vars(i)$
;      int_data = linint1_n_Wrap( lev, data, False, outlev(::-1), 0, 1)
      int_data = linint1_n_Wrap( lev, data, False, outlev, 0, 1)
      f->$(vars(i)+"plev")$ = int_data
      print( vars(i)+" good...")
      delete(data)
      delete(int_data)
   else
      print( vars(i)+" doesn't exist...")
   end if
end do

end do


end


