;goto,jump1

dlat = 180./128
xx = indgen(256)*dlat
yy = 90.-indgen(128)*dlat

rd = 287.6
b0 = 87.8

pt = get_fldcas2(0,'txt','pt')
ps = pt*pt*1000.

psa = get_fldcas2(0,'txt','psa')

tt = get_dyn3(0,'txt','tt')
t3 = tt*b0/Rd

ut = get_dyn3(0,'txt','ut')

vt = get_dyn3(0,'txt','vt')

ugt = get_dyn3(0,'txt','ugt')

vgt = get_dyn3(0,'txt','vgt')

vgt = get_dyn3(0,'txt','vgt')

py = get_dyn3(0,'txt','py')

ghi = get_dyn3(0,'txt','ghi')

vor = get_dyn3(0,'txt','zmh_vor')*4.0e4
pv = get_dyn3(0,'txt','zmh_pv')*4.0e4
;pvz = get_dyn3(0,'txt','zmh_pvz0')*4.0e4

gvor = get_dyn3(0,'txt','zmh_gvor')*4.0e4
gpv = get_dyn3(0,'txt','zmh_gpv')*4.0e4
gpvz = get_dyn3(0,'txt','zmh_gpvz')*4.0e4
pvz=gpvz

ghi2 = get_dyn3(0,'txt','zmh_ghi2')*1.0e-6
dir2 = get_dyn3(0,'txt','zmh_dir2')*1.e5

px3 = get_dyn3(0,'txt','zmh_px3')*1.e5
py3 = get_dyn3(0,'txt','zmh_py3')*1.e5
pz3 = get_dyn3(0,'txt','zmh_pz3')*1.e5

ghi3 = get_dyn3(0,'txt','zmh_ghi3')*1.0e-6
dir3 = get_dyn3(0,'txt','zmh_dir3')*1.e5

tt3 = get_dyn3(0,'txt','zmh_tt3')
;stop

;jump1:
;================

K= 20

var='ps'
dd = ps
lev1=cal_lev([870,1030],20)
plot_map4_new,dd,dd,xx,yy,lev1,lev1*100,title=var

var='psa'
dd = psa
lev1=cal_lev([-200.,200],20)
plot_map4_new,dd,dd,xx,yy,lev1,lev1*2,title=var

var='tt'
dd = reform(t3[*,K,*])/pt
lev1=cal_lev([-50, 15],20)
plot_map4_new,dd,dd,xx,yy,lev1,lev1*2,title=var

var='ghi'
dd = reform(ghi[*,K,*]) / 9.8
lev1=cal_lev([-1000., 1000],20)
plot_map4_new,dd,dd,xx,yy,lev1,lev1*2,title=var

var='u'
dd = reform(ut[*,K,*]) / pt
lev1=cal_lev([-30., 30],20)
plot_map4_new,dd,dd,xx,yy,lev1,lev1*2,title=var

var='v'
dd = reform(vt[*,K,*]) / pt
lev1=cal_lev([-30., 30],20)
plot_map4_new,dd,dd,xx,yy,lev1,lev1*2,title=var

var='ug'
dd = reform(ugt[*,K,*]) / pt
lev1=cal_lev([-30., 30],20)
plot_map4_new,dd,dd,xx,yy,lev1,lev1*2,title=var

var='vg'
dd = reform(vgt[*,K,*]) / pt
lev1=cal_lev([-30., 30],20)
plot_map4_new,dd,dd,xx,yy,lev1,lev1*2,title=var

var='py'
dd = reform(py[*,K,*]) / pt / 1.e-4
lev1=cal_lev([-30., 30],20)
plot_map4_new,dd,dd,xx,yy,lev1,lev1*200+10000,title=var

jump1:

var='pv'
dd = reform(pv[*,K,*]) 
lev1=cal_lev([-10., 10],20)
plot_map4_new,dd,dd,xx,yy,lev1,lev1*200+10000,title=var

var='vor'
dd = reform(vor[*,K,*])
lev1=cal_lev([-10., 10],20)
plot_map4_new,dd,dd,xx,yy,lev1,lev1*200+10000,title=var

var='pvz'
dd = reform(pvz[*,K,*])
lev1=cal_lev([-10., 10],20)
plot_map4_new,dd,dd,xx,yy,lev1,lev1*200+10000,title=var

var='gpv'
dd = reform(gpv[*,K,*])
lev1=cal_lev([-10., 10],20)
plot_map4_new,dd,dd,xx,yy,lev1,lev1*200+10000,title=var

var='gvor'
dd = reform(gvor[*,K,*])
lev1=cal_lev([-10., 10],20)
plot_map4_new,dd,dd,xx,yy,lev1,lev1*200+10000,title=var


var='gpvz'
dd = reform(gpvz[*,K,*])
lev1=cal_lev([-10., 10],20)
plot_map4_new,dd,dd,xx,yy,lev1,lev1*200+10000,title=var

var='ghi2'
dd = reform(ghi2[*,K,*]) 
lev1=cal_lev([-1000., 1000],20)
plot_map4_new,dd,dd,xx,yy,lev1,lev1*2,title=var

var='ghi3'
dd = reform(ghi3[*,K,*]) 
lev1=cal_lev([-1000., 1000],20)
plot_map4_new,dd,dd,xx,yy,lev1,lev1*2,title=var

var='dir2'
dd = reform(dir2[*,K,*]) 
lev1=cal_lev([-10., 10],20)
plot_map4_new,dd,dd,xx,yy,lev1,lev1*2,title=var

var='dir3'
dd = reform(dir3[*,K,*]) 
lev1=cal_lev([-10., 10],20)
plot_map4_new,dd,dd,xx,yy,lev1,lev1*2,title=var

var='tt3'
dd = reform(tt3[*,K,*])/pt
lev1=cal_lev([-50, 15],20)
plot_map4_new,dd,dd,xx,yy,lev1,lev1*2,title=var

;var='ghi,u,v'
;d = reform(ghi[*,K,*]) / 9.8
;ev1=cal_lev([-1000, 1000],20)
;lot_map_uv,dd,xx,yy,lev12,$
;   vect=1,uvel=reform(u3[*,k,*])/pt,vvel=reform(v3[*,k,*])/pt,title=var


end

