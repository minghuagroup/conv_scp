ncks -v CLDTOT,CLDLOW,CLDHGH,CLDMED,TGCLDLWP,TGCLDIWP,TGCLDCWP,CLOUD,CLDLIQ,CLDICE,TMQ
ncks -v CLDTOT,CLDLOW,CLDHGH,CLDMED,TGCLDLWP,TGCLDIWP,TGCLDCWP
ncks -v PRECC,PRECL,PRECCDZM,PRECT,PREW
ncks -v CLOUD,CLDLIQ,CLDICE,RELHUM,
ncks -v CONCLD,LCLOUD,ICECLDF,cufrc_Cu      ; these need to be specially saved
can be saved in cloudXXX to be processed in iap32.pro


.r mac region to get regions
;=== cheyenne
; get_daily.pro average prec data to daily for haiyang's plot of MJO
; get_diurnal to pull diurnal data then proc on mac as 

;== mac
   cam_diurnal.pro and plot_2d
   cam_homo for homograph pentad mjo and seasonal
   cam_homodi for diurnal
   cam_homomjo for pentad
   cam_homomon for seasonal

; get_monsoon 

;crm
crm_dump.pro
crm1.pro to read
plot_tp.pro to assign aa var and plot, compare with plot_conv

;========
sub_conv
conv1.pro for convection test
plot_conv

conv2.pro for cld scheme test
plot_2dscm aa,x,y,var t-p cross section


era_c3m_sav
era_c3m_u00
.r cldfunc
cld_new.pro

00_3d.pro
plot_3d.pro

00_2d.pro
plot_2d.pro


================================
./jj ./jj1 on cheyenne
cam0 -> 00_range

cam11 -> scm readin
cam12 -> scm view
cam13 -> scm web dump view

cam21 -> forecast hrly file for 2d fields cldlow etc  zyx???.h0.
cam22 -> view zyx???.h1. cld
cam22c -> view zyx???.h1. cld
cam22p -> view zyx???.h1 precip

mean tape 
cam30 plot type
cam31 -> read h0 file, for 2d fields cloud???.nc
cam32 -> read file, plot or 2d 3d fields may change cloud??? to other file
cam32p
cam32c 3d clouds
cam32cldlow
cam32tgcld

cam3d_v generic dd3 array
cam3phy dd3 from proc_phy3 on storm

cam41 and cam42 prec???

  cam2p hrly single fld, cam2pp daily ,prec
  cam2c, cam2cc clouds, pbl
  cam22, any array

for single 3d arrays
cam30.pro, cam31.pro for file, cam32 for plot

cam3 -> daily
cam4 -> weekly
cam5 -> iweb

scm1
scm2 - web
 scm2s -interactive

================================

00_range.pro
trmm_cam_head.pro to change to L30 and L35 using ij
trmm_cam_hourly
trmm_cam_daily
trmm_cam_weekly
iweb  file:///Users/Minghuazhang/Documents/unix/workgcm/html/



.r myview
myncview for all CAM files
ncdiff 1.nc 2.nc 1_2.nc to get the difference

;amwg package:
00_diag.pro
cam3d_diag
plot_2d to view 2d fields, gif in folders gif_3d and gif_2d


plot parameterization scm results steps
============================
1. after run (turn on SCMDIAG in makefile
 USER_FFLAGS  := -DSCMDIAG
 LYPACK /T1/xxie/minghua/software/lapack-3.8.0.  

mget scmdiag-output.nc scp13_scmdiag-output.nc  ; change for kwajex
mget camrun.cam.h0.1992-12-19-00000.nc  scp12_camrun.cam.h0.1992-12-19-00000.nc
 30: normalized 4 plume
 40: closure 4 plume

2. plot_scm_headi.pro 
set cix and icase2 to decide which type of diagnostics and which case

3. for cix='_ix', scm plume diagnostics
     .r diag_scmi to assign diag variables
     .r diag_tend to do local variable tend manuel diagnostics
     .r plot_scm2di and 
     .r plot_scm1di to plot plume diagnostics
     .r plot_scm_headi again to display on web  such as  ix_case_2d.html
        web: file:///Users/minghuazhang/Documents/UNIX/workgcm/html_scm/
             ix_scp13_1d.html, for all time, all plumes
             ix_scp13_2d.html, for all plumes, but a fixed time ix
          to evaluate structure and compare with other's works
  
   for cix=''     scm time-pressure cross sections
     .r plot_scm2d and 
     .r plot_scm1d to plot standard scm dump fields
     .r plot_scm_headi again to display on web  such as  ix_case_2d.html
    
4 get_lev and get_levi for line lev set


plot GCM scm results steps
============================
a. after run in storm minghua/gcm/test1/run with approproate setting atm_in drv
 USER_FFLAGS  := -DSCMDIAG
 LYPACK /T1/xxie/minghua/software/lapack-3.8.0.

mget .h1. minghua/gcm/test1/run tp scp40_gcmrun.cam.h1.2017-07-27-00000.nc
 scp20: ZM
 scp40: github version uniform mass
 scp41: github version closure mass

 whole sequence should be run, hourly can be skipped, but icase and iweb together
 00_range to change: data file, plot type, case name
 trmm_cam_header to change: readin data, iix, igif, and variable to see
 trmm_cam_hourly
 trmm_cam_hourly <-- re-run 00_range and trmm_cam_head
 trmm_cam_weekly
 iweb  to change: html location and gif filepath  



fnl2cam! worked what's left is lan and ocn initialization!! and SST!!
sst2cam
fnl2clm
 fnl2cam_cheng.pro ; on mac
 fnl2clm_cheng.pro ; on mac
 sst2cam_cheng.pro ; on mac
 
 on yellowstone obs/myfnl.sh ; batch
 ;run the following
 fnl2cam_cheng2.pro ; on yellowstone
 fnl2clm_cheng2.pro ; on 
 sst2cam_cheng2.pro ; on 


check2d.pro
my_inter_clm.pro
trmm_coarse.pro

SST replacement and daily obs data

; to add a field: 
	1. 00_range to add the contour levels
	2. cam3d or cam3d_p or cam2d to read, define the var
	3. iweb to put in html
; to add a 3d dataset 
	1. modify 00_range to define a new idata
	2. copy a cam3d.pro file to xxx3d.pro
; to add a 2d data
	1. copy cam2d.pro to xxx2d.pro
; to add a new cross section
	1. change 00_range to define a new iave
; to display in html
	1. change iweb.pro html filename, ls list, and gif path

;============
ALL GCM and OBS 2D-3D fields starts from 00_range.pro
00_range.pro ; to set the common ranges
;run each time ------
finish with iweb

below are in parallel

1.
c3m3d.pro    ; for 3d cld fields
c3m2d.pro    ; for 2d cld fields

2.
era3d.pro

3.
fnl3d.pro
fnl2d.pro
https://rda.ucar.edu/#!lfd?nb=y&b=proj&v=NCEP Climate Forecast System Reanalysis

4.
fnl2cam.pro
fnl2clm.pro

5.
cam2d.pro
cam3d.pro    ; for model
cam3d_p.pro    ; for model physics

6. for any CAM fields , each time re-run 00_range and trmm_cam_head!!
                                         ---------------------------
trmm_cam_head.pro
trmm_cam_hourly
trmm_cam_daily
trmm_cam_weekly
....
to compare with TRMM
iweb  in html/ and gif_... folder

7.
cam2d_hourly.pro
Langley GOES comparison

8.
cam3di.pro  ;initial condition check to read in both cam and cam state fields
cam3di.pro

ceres.pro
cmap.pro
gpcp.pro
olr.pro 

trmm_head  ; trmm_cam_head ..... in parallel
trmm_hourly  trmm_cam_hourly  specifically to compare with TRMM data 
trmm_daily   trmm_cam_daily
trmm_weekly  trmm_cam_weekly              ; daily data diff resolutions 
trmm_monthly  ;need to multiply 10 for mm/day
; daily data directly to do prec_obs
https://disc2.gesdisc.eosdis.nasa.gov/data/TRMM_RT/

plot_scm_head.pro
plot_scm2d
plot_scm1d

diag_scm
diag_scm2d

9.
iweb

;-----------------------
xin's diagnost plumes
plot_scm_headi.pro
plot_scm2di
plot_scm1di
diag_scmi
plot_scm_headi.pro



 use write_cdf2.pro in cmdv1
# Replace scl in group g2 in out.nc with scl from group g2 from in.nc
ncks -A -g g2 -v scl in.nc out.nc
 to prepare initial condition by using a template!
ncrename -v p,pressure -v .t,temperature in.nc



iweb.pro     
;=============

prec.pro
cld.pro
cld2.pro ; compare obs with c3m data


;==========
generic programs
 view3d.pro is generic  !!
 iweb
 m2p
 m2p_ec
 z2p
 interpol2 , my_interpol, my_interpol2
 ave3
 get_fld_fnl2d
 get_fld_fnl3d
 read_trmm for binary
 read_gpcp for binary


cld3.pro ; compare obs with c3m data 3d fields

;to check model simulations
prec_obs  seems to have a unit problem *10 verified against global movie
               used plot_map4g
fnl.pro
c3m.pro


called by c3m3d.pro

need: 
cld, crf, met (wind,humidity), prec

geov is a downloaded package



00_3d.pro 
00_3d_dump
3d_plot  , can set z_range=[600,600] as long as xx,yy,zz,aa,lev1,lev2 set

00_era_dump

era_c3m.pro collocate

cldfrac1 ; monthly tuning
cldfrac2 ; instantaneous tuning CAM !!!
 00_3d.pro
 plot_3d.pro

