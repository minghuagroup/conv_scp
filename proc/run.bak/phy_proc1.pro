;goto,jump1
;0---
folder = 'fout9'
folder = 'fout28'
folder = 'fout'

;1---
var = 'qdiffg'
;var = 'vort3'
var = 'tdiff3'
var = 'qdiff3'
;var = 'q' & k=29
var = 'vort3b'
var = 'tpert2'
var = 'qpert'
var = 'sgh30'
var = 'qpert'
;2---
scale =1.e-2
scale =1.
scale =1.e3
aa = get_fldphy3(2,var,folder=folder,x=xx,y=yy)  ;! fout folder!

siz = size(aa)
if(siz[0] eq 3)then aa = reform(aa[*,k,*])
dd = aa*scale

range=[min(dd),max(dd)]
;3---
lev1=cal_lev([0,20.],20)
lev1 = cal_lev(range,20)
lev1=cal_lev([-5,5.],20)
lev1=cal_lev([0,5.],20)
lev1=cal_lev([0,2.],20)
plot_map4_new,dd,dd,xx,yy,lev1,lev1*100,title=var
print,var,' ',range
end
