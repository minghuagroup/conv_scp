;goto,jump1

dlat = 180./128
xx = indgen(256)*dlat
yy = 90.-indgen(128)*dlat

rd = 287.6
b0 = 87.8

;pt = get_fldcas2(0,'txt','pt',folder='fout_20180901')
;ps = pt*pt*1000.
;dd = ps
;var = 'ps'
;lev1 = cal_lev([860.,1040],20)
;plot_map4_new,dd,dd,xx,yy,lev1,lev1,title=var
;mygif,'gifs/'+var+'.gif'
;stop

;==================
var = 'dug' ;tt1c';dt1';lghi1';' ;4'
;var = 'zmh_res4';' ;4' ;zmh_tt'; zmh_3' ;zmh_gpvz' ;ghi0' ;z0' ;ghi0'; dgh0' ;tt' ;zmh_pvz'
scale = 3600.

dd3 = get_fldcas3(0,'txt',var,folder='fout_20180901')*scale

K= 20
dd = reform(dd3[*,K,*])
lev1=cal_lev([min(dd),max(dd)],20) /5 ;1.e3
lev1=cal_lev([-3.,3],20)
plot_map4_new,dd,dd,xx,yy,lev1,lev1*200+30000,title=var
;mygif,'gifs/'+var+'.gif'
end
