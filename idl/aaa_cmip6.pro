
; steps

;===== cheyenne and local

1. cmip6_macro
2. cmip2nc.pro newer than cmip2sav.pro on cheyenne get data on work/C/CMIP/NCAR_CESM2/amip/sav etc.
   cmip2scan to scan all folders and put in data structure
   scan2nc to put these in text file, not yet cdf


5/6/2021   
important subroutines: 
; ====================

 get_dd2m.pro, can do difference and interpolation, taking on one file or a folder!
 nc3fig.pro  ; take on single file or a folder, do basic or difference plot
 nc3figs.pro ; take on 12 month files
 add_field.pro can be called directly as a subroutine
    incorporated cam2add, called era2add
 var_map.pro

 executable calling routines (to be tested, for now, can still use old programs:
; ====================

 sav2nc.pro for original sav and sav2 file conversions

 pnc2fig.pro for base plots calls nc2fig for single file and nc2figs for seasonal files
 ncf2figs.pro take DJF JJA files

 pnc2fig_diff.pro for difference plots, calling the same subroutines
 pnc2figs_diff.pro for difference taking DJF JJA files

 nc2fig_era.pro
 nc2fig_era_diff.pro

 cam_scaling1, era5_...
 cam_scaling2, era5 ...

 padd_field.pro standalone call to add_field.pro

mydiag.pro     to simply put the data folder on the web in cmip6/html
mydiag1c         to put comparion of two folders
mydiag1s         to put seasonal comparison 
mydiag1s_diff    to put diff comparison one season per page



4.nc1fig.pro       to plot data under the above folder with ANN, DJF etc. jpeg files, modify color table

nc1fig_diff.pro to take the difference between twp cases

nc2fig_dump written for one file

NC2FIG.PRO  for a folder or a single file

5. mydiag1s_diff
; ERA5
cheyenne 
1. era5mn_decadem.pro

;local
2. era5_scaling2

   era2add.pro  !
   nc2fig_era.pro             ; color table
   nc2fig_era_diff.pro        ; color table
   mydiags_diff.pro

TODO:
; =====================
; get all model data
cam scaling clean up
insert 2D fields to netcdf

nc2fig_obs_diff for cam and iap
nc2fig_model_diff in general  can this be in one program for all plots, no!

download and auto-processing

to do: get_dd2m interpolate from sigma to pressure level 
       speed up my_interpol or my_interpol3d or 3v

do a CAS-ESM diff
do interpolation for obs and other comparison 
get single variable from all runs and put in structure


1 subs
cmip6_vars.pro   to define cmip6 variables to extract, cesm6 variables including some land, ocn etc.
                 new variables to add, DEFINES mapping and new fields, what models and cases to include
cmip6_2cesm_var.pro to map variables, c option 1 is to map cmip6 variables to cesm variables

get_fld_cmip6.pro is to extract data based on model, mip, expid, datatype

cmip6_fill_lowest.pro to fill missing data below surface, reverse vertical coordinates for all 3d fields

cmip6_add_vars.pro to add new 2d variables

get_cmip6_model_number.pro  all based on names defined in cmip6_macros.pro
get_cmip6_mip_number.pro
get_cmip6_exp_number.pro

get_lastv2_inlist.pro
get_match_list.pro
get_lev_adjust.pro
make_folder.pro
str_replace.pro
get_timestamp_value.pro

sav2gif.pro  ; one experiment, 4 seasons and annual average
sav2gif_diff ; two experiments, and the difference, for 4 seasons  ; setZ controls whether in buffer

gif2web.pro   ; one folder give
gif2webs.pro  ; multiple columns for multiple folders

cmip2add.pro
cmip2fill.pro
array_spaced.pro
array_spaced2.pro

get2d.pro and view2d to separate view3d.pro so that color can be adjusted in get_lev(dd,var...)

2.exe


cmip6_macro.pro (exe file)  to define models, mips, expids, datatypes, variable mapping between cmip6 and cesm

cmip2sav.pro

mydiag.pro  ; one experiment generic

mydiag1.pro  ; diagnostic package one experiment, 4 seasons, put on web
mydiag2.pro  ; two exp and take the differences, all plots are re-done here unless commented out 

mycmip6_add.pro  to call cmip2add and cmip2fill to populate another sav2 file for .sav files

sav2nc.pro . get nc file and seasonal averages, color table needs to be specified or generated, care is needed here









sav2gif.pro    ; directly specify .sav file folders
sav2gif.pro    ; use modelID etc to specify .sav files, for individual month
save2gif2.pro; ; as above, do seasonal averaging and looping
save2gifs.pro; ; taking diff between two exps and make plots

gif2web.pro    ; a single folder display
gif2webs.pro   ; multiple columns, especially model comparisons



cmip6_verify
;--------


cmip6_data.pro  get cmip6 data from the ncar archive ~/cmip or /glade/u/home/mzhang/cmip/CMIP6
                to loop or select models, mips etc. to save data in appropriate folders 
                save for individual variables in cmip variable *.sav format dd2m
                atm, ocn, lnd etc
                check the folder like /glade/u/home/mzhang/work/C/NCAR_CESM2/CMIP/amip_climo/ for .sav data files

cmip6_cdf_template.pro
         create a template cdf file like in /glade/u/home/mzhang/obs/FNL/Cheng/NCAR_CESM2_CMIP_amip_tmp.nc
         out from ~/mzhangs/tmp/NCAR_CESM2_CMIP_amip_tmp.nc and manually copy it to 
              /glade/u/home/mzhang/obs/FNL/Cheng/NCAR_CESM2_CMIP_amip_tmp.nc for use by cmip6_cdf_data.pro
         to run diagnostic package

cmip6_cdf_data.pro to loop or select models, mips etc. to retrive data in appropriate folders
                filling missing data cmip6_fill_lowest.pro
                calculate additional fields cmip6_add_vars.pro
                loop through months to put into monthly cdf files 
                   change attributes of cdf file
                   loop through individual variables in *.sav format dd2m

               check /glade/u/home/mzhang/work/C/NCAR_CESM2/CMIP/amip_climo/ for newly saved ATMNET.sav U.sav files`
               check /glade/u/home/mzhang/work/C/NCAR_CESM2/cdfs/CMIP_amip/CMIP_amip_01_climo.nc for climo files

cmip6_verify.pro to read data from both .sav and climo .nc  for both cmip6_data.pro and cmip6_cdf_data.pro
                do plots to visually check

3. post on storm archive
01_extract_cmip6.sh to process into monthly climo and seasonal data
02_cmip6_obs.sh     to run diagnostics

open Q:
210414
global attribute cannot be changed in cmip6_cdf_data and cmip6_cdf_template, so the case name incorrect
diagnostic package plots are not good for some fields, check units and mapping
direct downloads from ESG

