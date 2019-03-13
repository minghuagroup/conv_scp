;goto,jump1

dlat = 180./128
xx = indgen(256)*dlat
yy = 90.-indgen(128)*dlat

rd = 287.6
b0 = 87.8

;pt = get_fldcas2(0,'txt','pt')
;ps = pt*pt*1000.
;dd = ps
;lev1 = cal_lev([860.,1040],20)

var = 'pyp'
scale = 1.e3 ;-4 ;5.e4
;dd = get_fldcas2(0,'txt',var,folder='fout')
dd = dd*scale
;goto,jump2
;plot_map4_new,dd,dd,xx,yy,lev1,lev1,title=var
;mygif,'gifs/'+var+'.gif'
;stop

;==================
var = 'dvg' ;tt1c';dt1';lghi1';' ;4'
;var = 'zmh_res4';' ;4' ;zmh_tt'; zmh_3' ;zmh_gpvz' ;ghi0' ;z0' ;ghi0'; dgh0' ;tt' ;zmh_pvz'
scale = 3600. ;1.;e3 ;-4 ;5.e4
dd3 = get_fldcas3(0,'txt',var,folder='fout')*scale
lev1=cal_lev([-5.,5],20) ;/2.
lev2 = (lev1 +2000.)*10

K= 10
dd = reform(dd3[*,K,*])
jump2:
;lev1=cal_lev([min(dd),max(dd)],20) ;/2.
print,min(dd),max(dd),min(lev1),max(lev1)
plot_map4_new,dd,dd,xx,yy,lev1,lev2,title=var
mygif,'gifs/'+var+'_0.gif'

end
